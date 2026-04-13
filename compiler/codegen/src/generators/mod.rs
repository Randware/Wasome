mod expression;
mod function;
mod statement;

use crate::{Codegen, context::LLVMContext, errors::CodegenError, types::ModuleContext};
use ast::id::Id;
use ast::symbol::{EnumSymbol, FunctionSymbol, StructSymbol, SymbolWithTypeParameter};
use ast::top_level::{Function, FunctionType};
use ast::traversal::FunctionContainer;
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::traversal::file_traversal::FileTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::{AST, TypedAST};
use inkwell::types::{BasicType, BasicTypeEnum};
use std::ops::Deref;
use std::rc::Rc;

impl<'ctx> Codegen<'ctx> {
    pub fn compile(&mut self, to_compile: &AST<TypedAST>) -> Result<(), CodegenError<'_>> {
        let mut llvm_context = LLVMContext::new(self.context, self.opt_level);
        self.compile_project(&mut llvm_context, to_compile)?;
        todo!()
    }

    pub fn compile_project(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        to_compile: &AST<TypedAST>,
    ) -> Result<(), CodegenError<'_>> {
        let helper = DirectoryTraversalHelper::new_from_ast(to_compile);

        // Pass 1
        for project in helper.subdirectories_iterator() {
            llvm_context
                .add_module(project.inner().name())
                .ok_or_else(|| {
                    CodegenError::DuplicateProjectName(project.inner().name().to_string())
                })?;

            for st in recursive_structs_of_dir(project.clone()) {
                let lowered = self
                    .context
                    .opaque_struct_type(&mangle(st.name(), st.id().clone()));
                debug_assert!(
                    llvm_context
                        .type_registry_mut()
                        .register_struct(st, lowered)
                        .is_none()
                )
            }

            for en in recursive_enums_of_dir(project.clone()) {
                let lowered = self
                    .context
                    .opaque_struct_type(&mangle(en.name(), en.id().clone()));
                debug_assert!(
                    llvm_context
                        .type_registry_mut()
                        .register_enum(en, lowered)
                        .is_none()
                )
            }
        }

        // Pass 2
        for project in helper.subdirectories_iterator() {
            let project_name = project.inner().name();
            let module = llvm_context
                .get_module(project_name)
                .ok_or_else(|| CodegenError::Ice("LLVM module is missing".to_string()))?;

            for func in recursive_functions_of_dir(project.clone()) {
                let symbol = func.declaration_owned();
                let args = symbol
                    .params()
                    .iter()
                    .map(|arg| llvm_context.lower_type(arg.data_type()).unwrap().into())
                    .collect::<Vec<_>>();
                let lowered_type = symbol.return_type().map_or_else(
                    || llvm_context.context().void_type().fn_type(&args, false),
                    |ret| llvm_context.lower_type(ret).unwrap().fn_type(&args, false),
                );
                let name = match func.function_type() {
                    FunctionType::Regular(_) => mangle(symbol.name(), symbol.id().clone()),
                    FunctionType::External => {
                        let sizes = symbol
                            .params()
                            .iter()
                            .map(|param| param.data_type().size_bytes())
                            .map(|param| param.to_string())
                            .collect::<Vec<_>>()
                            .join("-");
                        format!("{}-{}", symbol.name(), sizes)
                    }
                };
                let lowered = module.inner.add_function(&name, lowered_type, None);
                debug_assert!(
                    llvm_context
                        .type_registry_mut()
                        .register_function(symbol, lowered)
                        .is_none()
                )
            }
        }

        for project in helper.subdirectories_iterator() {
            let project_name = project.inner().name();
            let module = llvm_context
                .get_module(project_name)
                .ok_or_else(|| CodegenError::Ice("LLVM module is missing".to_string()))?;

            for_each_func_of_dir(project, |func| self.compile_function(llvm_context, &func));
        }
        todo!()
    }

    pub fn compile_file(
        &mut self,
        context: &mut LLVMContext<'ctx>,
        module: ModuleContext<'ctx>,
    ) -> Result<(), CodegenError<'_>> {
        todo!()
    }
}

fn mangle(name: &str, id: Id) -> String {
    format!("{}-{}", name, id.as_unique_string())
}

fn recursive_functions_of_dir<'b>(
    dir: DirectoryTraversalHelper<'_, 'b, TypedAST>,
) -> Vec<&'b Function<TypedAST>> {
    recursive_files_of_dir(dir, &|file| {
        file.function_iterator()
            .map(|en| en.inner().deref())
            .chain(
                file.structs_iterator()
                    .map(|st| {
                        st.function_iterator()
                            .map(|func| func.inner().deref())
                            .collect::<Vec<_>>()
                            .into_iter()
                    })
                    .flatten(),
            )
            .collect::<Vec<_>>()
            .into_iter()
    })
}

fn for_each_func_of_dir<'b>(
    dir: DirectoryTraversalHelper<'_, 'b, TypedAST>,
    mut callback: impl for<'a> FnMut(FunctionTraversalHelper<'a, 'b, TypedAST>),
) {
    dir.subdirectories_iterator()
        .for_each(|subdir| for_each_func_of_dir(subdir, &mut callback));
    dir.files_iterator().for_each(|file| {
        file.function_iterator().for_each(&mut callback);
        file.structs_iterator()
            .for_each(|st| st.function_iterator().for_each(&mut callback));
    });
}

fn recursive_enums_of_dir(
    dir: DirectoryTraversalHelper<TypedAST>,
) -> Vec<Rc<EnumSymbol<TypedAST>>> {
    recursive_files_of_dir(dir, &|file| {
        file.enums_iterator()
            .map(|en| en.inner().symbol_owned())
            .collect::<Vec<_>>()
            .into_iter()
    })
}

fn recursive_structs_of_dir(
    dir: DirectoryTraversalHelper<TypedAST>,
) -> Vec<Rc<StructSymbol<TypedAST>>> {
    recursive_files_of_dir(dir, &|file| {
        file.structs_iterator()
            .map(|en| en.inner().symbol_owned())
            .collect::<Vec<_>>()
            .into_iter()
    })
}

fn recursive_files_of_dir<
    'b,
    T,
    Iter: Iterator<Item = T>,
    Mapper: Fn(FileTraversalHelper<'_, 'b, TypedAST>) -> Iter,
>(
    dir: DirectoryTraversalHelper<'_, 'b, TypedAST>,
    map_with: &Mapper,
) -> Vec<T> {
    dir.subdirectories_iterator()
        .map(|subdir| recursive_files_of_dir(subdir, map_with).into_iter())
        .flatten()
        .chain(dir.files_iterator().map(map_with).flatten())
        .collect()
}
