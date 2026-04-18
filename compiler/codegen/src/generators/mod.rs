mod expression;
mod function;
mod statement;

use crate::context::FunctionContext;
use crate::symbols::{EnumInformation, StructInformation};
use crate::{Codegen, context::LLVMContext};
use ast::id::Id;
use ast::symbol::SymbolWithTypeParameter;
use ast::top_level::FunctionType;
use ast::traversal::FunctionContainer;
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::file_traversal::FileTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use ast::{AST, TypedAST};
use inkwell::types::BasicType;
use std::iter::once;

impl<'ctx> Codegen<'ctx> {
    pub fn compile(&mut self, to_compile: &AST<TypedAST>) -> Vec<u8> {
        let mut llvm_context = LLVMContext::new(self.context, self.opt_level);
        self.compile_internal(&mut llvm_context, to_compile);
        llvm_context.get_object()
    }

    // Despite all the unwraps and expects, this never panics as the typed AST disallows all of
    // those problems
    #[allow(clippy::missing_panics_doc)]
    pub fn compile_internal(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        to_compile: &AST<TypedAST>,
    ) {
        let root = DirectoryTraversalHelper::new_from_ast(to_compile);

        self.register_structs(llvm_context, &root);

        Self::register_enums(llvm_context, &root);

        self.fill_structs(llvm_context, &root);

        self.fill_enums(llvm_context, &root);

        Self::register_functions(llvm_context, &root);

        self.create_struct_drop_functions(llvm_context, &root);

        self.create_enum_drop_functions(llvm_context, &root);

        self.impl_functions(llvm_context, &root);
    }

    fn impl_functions(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_functions_of_dir(root, |func| match func.inner().function_type() {
            FunctionType::Regular(_) => self.compile_function(llvm_context, &func),
            FunctionType::External => (),
        });
    }

    fn create_enum_drop_functions(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_enums_of_dir(root, |st| {
            let symbol = st.inner().symbol();
            let tr = llvm_context.type_registry_mut();
            let lowered = tr.get_enum(symbol).expect("Unregistered enum");
            let func = lowered.on_drop();
            drop(tr);
            let main_bb = self.context.append_basic_block(func, "main");
            let mut fc = FunctionContext::new(func, main_bb);
            llvm_context.builder().position_at_end(main_bb);

            self.compile_enum_drop(
                llvm_context,
                &mut fc,
                symbol,
                func.get_first_param()
                    .expect("Drop function takes no parameters")
                    .into_pointer_value(),
            );
        });
    }

    fn create_struct_drop_functions(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_structs_of_dir(root, |st| {
            let predrop = st
                .function_iterator()
                .find(|func| {
                    let symbol = func.inner().declaration();
                    symbol.name() == "predrop"
                        && symbol.type_parameters().is_empty()
                        && symbol.params().len() == 1
                        && symbol.return_type().is_none()
                })
                .map(|func| {
                    llvm_context
                        .type_registry()
                        .get_function(func.inner().declaration())
                        .expect("Unknown function")
                });
            let symbol = st.inner().symbol();
            let mut tr = llvm_context.type_registry_mut();
            let lowered = tr.get_struct_mut(symbol).expect("Unregistered struct");
            if let Some(predrop) = predrop {
                lowered.set_predrop(predrop);
            }
            let func = lowered.on_drop();
            drop(tr);
            let main_bb = self.context.append_basic_block(func, "main");
            let mut fc = FunctionContext::new(func, main_bb);
            llvm_context.builder().position_at_end(main_bb);

            self.compile_struct_drop(
                llvm_context,
                &mut fc,
                symbol,
                func.get_first_param()
                    .expect("Drop function takes no parameters")
                    .into_pointer_value(),
            );
        });
    }

    fn fill_enums(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_enums_of_dir(root, |st| {
            let symbol = st.inner().symbol();
            let mut tr = llvm_context.type_registry_mut();
            let lowered = tr.get_enum_mut(symbol).expect("Unregistered enum");
            let variants = st.inner().variants();
            let base_enum = &[
                self.context.i32_type().as_basic_type_enum(),
                self.context.i32_type().as_basic_type_enum(),
            ];
            for variant in variants {
                let fields_lowered = base_enum
                    .iter()
                    .copied()
                    .chain(
                        variant
                            .inner()
                            .fields()
                            .iter()
                            .map(|field| llvm_context.lower_type(field)),
                    )
                    .collect::<Vec<_>>();
                let variant_lowered = self.context.struct_type(&fields_lowered, false);
                lowered.insert(variant.inner_owned(), variant_lowered);
            }
        });
    }

    fn fill_structs(&self, llvm_context: &LLVMContext, root: &DirectoryTraversalHelper<TypedAST>) {
        recursive_structs_of_dir(root, |st| {
            let symbol = st.inner().symbol();
            let mut tr = llvm_context.type_registry_mut();
            let lowered = tr.get_struct_mut(symbol).expect("Unregistered struct");
            let fields = st.inner().fields();
            let ref_count_field = self.context.i32_type().as_basic_type_enum();
            let fields_lowered = once(ref_count_field)
                .chain(
                    fields
                        .iter()
                        .map(|field| llvm_context.lower_type(field.inner().data_type())),
                )
                .collect::<Vec<_>>();
            lowered.lowered().set_body(&fields_lowered, false);
            for field in fields {
                lowered.add_field(field.inner_owned());
            }
        });
    }

    fn register_functions(
        llvm_context: &mut LLVMContext,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        let module = llvm_context.module();
        recursive_functions_of_dir(root, |func| {
            let symbol = func.inner().declaration_owned();
            let args = symbol
                .params()
                .iter()
                .map(|arg| llvm_context.lower_type(arg.data_type()).into())
                .collect::<Vec<_>>();
            let lowered_type = symbol.return_type().map_or_else(
                || llvm_context.context().void_type().fn_type(&args, false),
                |ret| llvm_context.lower_type(ret).fn_type(&args, false),
            );
            let name = match func.inner().function_type() {
                FunctionType::Regular(_) => mangle(symbol.name(), symbol.id()),
                FunctionType::External => {
                    let sizes = symbol
                        .params()
                        .iter()
                        .map(|param| param.data_type().size_bytes())
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join("_");
                    if sizes.is_empty() {
                        symbol.name().to_string()
                    } else {
                        format!("{}_{}", symbol.name(), sizes)
                    }
                }
            };
            let lowered = module.add_function(&name, lowered_type, None);
            debug_assert!(
                llvm_context
                    .type_registry_mut()
                    .register_function(symbol, lowered)
                    .is_none()
            );
        });
    }

    fn register_enums(llvm_context: &mut LLVMContext, root: &DirectoryTraversalHelper<TypedAST>) {
        let module = llvm_context.module();
        recursive_enums_of_dir(root, |en| {
            let symbol = en.inner().symbol_owned();
            let name = mangle(symbol.name(), symbol.id());
            let drop = module.add_function(
                &format!("{name}-drop"),
                llvm_context.global_registry().drop(),
                None,
            );
            debug_assert!(
                llvm_context
                    .type_registry_mut()
                    .register_enum(symbol, EnumInformation::new(drop))
                    .is_none()
            );
        });
    }

    fn register_structs(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        let module = llvm_context.module();
        recursive_structs_of_dir(root, |st| {
            let symbol = st.inner().symbol_owned();
            let name = mangle(symbol.name(), symbol.id());
            let lowered = self.context.opaque_struct_type(&name);
            let drop = module.add_function(
                &format!("{name}-drop"),
                llvm_context.global_registry().drop(),
                None,
            );
            debug_assert!(
                llvm_context
                    .type_registry_mut()
                    .register_struct(symbol, StructInformation::new(lowered, drop))
                    .is_none()
            );
        });
    }
}

fn mangle(name: &str, id: &Id) -> String {
    format!("{}-{}", name, id.as_unique_string())
}

fn recursive_functions_of_dir<'b>(
    dir: &DirectoryTraversalHelper<'_, 'b, TypedAST>,
    mut callback: impl for<'a> FnMut(FunctionTraversalHelper<'a, 'b, TypedAST>),
) {
    recursive_files_of_dir(dir, &mut |file| {
        file.function_iterator().for_each(&mut callback);
        file.structs_iterator()
            .for_each(|st| st.function_iterator().for_each(&mut callback));
    });
}

fn recursive_enums_of_dir(
    dir: &DirectoryTraversalHelper<TypedAST>,
    mut callback: impl FnMut(EnumTraversalHelper<TypedAST>),
) {
    recursive_files_of_dir(dir, &mut |file| {
        file.enums_iterator().for_each(&mut callback);
    });
}

fn recursive_structs_of_dir(
    dir: &DirectoryTraversalHelper<TypedAST>,
    mut callback: impl FnMut(StructTraversalHelper<TypedAST>),
) {
    recursive_files_of_dir(dir, &mut |file| {
        file.structs_iterator().for_each(&mut callback);
    });
}

fn recursive_files_of_dir<'b, Callback: FnMut(FileTraversalHelper<'_, 'b, TypedAST>)>(
    dir: &DirectoryTraversalHelper<'_, 'b, TypedAST>,
    callback: &mut Callback,
) {
    dir.subdirectories_iterator()
        .for_each(|subdir| recursive_files_of_dir(&subdir, callback));
    dir.files_iterator().for_each(callback);
}
