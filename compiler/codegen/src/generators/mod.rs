use crate::{context::LLVMContext, errors::CodegenError, types::ModuleContext, Codegen};
use ast::id::Id;
use ast::symbol::{EnumSymbol, StructSymbol, SymbolWithTypeParameter};
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::traversal::file_traversal::FileTraversalHelper;
use ast::TypedAST;
use std::rc::Rc;

impl<'ctx> Codegen<'ctx> {
    pub fn compile(&mut self) -> Result<(), CodegenError<'_>> {
        let mut llvm_context = LLVMContext::new(self.context, self.opt_level);
        self.compile_project(&mut llvm_context)?;
        todo!()
    }

    pub fn compile_project(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
    ) -> Result<(), CodegenError<'_>> {
        let helper = DirectoryTraversalHelper::new_from_ast(&self.ast);

        // Pass 1
        for project in helper.subdirectories_iterator() {
            llvm_context
                .add_module(project.inner().name())
                .ok_or_else(|| {
                    CodegenError::DuplicateProjectName(project.inner().name().to_string())
                })?;

            for st in recursive_structs_of_dir(project.clone()) {
                let lowered = self.context.opaque_struct_type(&mangle(st.name(), st.id().clone()));
                debug_assert!(llvm_context.type_registry_mut().register_struct(st, lowered).is_none())
            }

            for en in recursive_enums_of_dir(project.clone()) {
                let lowered = self.context.opaque_struct_type(&mangle(en.name(), en.id().clone()));
                debug_assert!(llvm_context.type_registry_mut().register_enum(en, lowered).is_none())
            }
        }

        // Pass 2
        for project in helper.subdirectories_iterator() {
            let project_name = project.inner().name();
            let module = llvm_context
                .get_module(project_name)
                .ok_or_else(|| CodegenError::Ice("LLVM module is missing".to_string()))?;

            todo!("Call compile_file")
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

fn recursive_enums_of_dir(dir: DirectoryTraversalHelper<TypedAST>) -> Vec<Rc<EnumSymbol<TypedAST>>> {
    recursive_files_of_dir(dir,
                           &|file|
                               file.enums_iterator().map(|en| en.inner().symbol_owned()).collect::<Vec<_>>().into_iter())
}

fn recursive_structs_of_dir(dir: DirectoryTraversalHelper<TypedAST>) -> Vec<Rc<StructSymbol<TypedAST>>> {
    recursive_files_of_dir(dir,
                           &|file|
                               file.structs_iterator().map(|en| en.inner().symbol_owned()).collect::<Vec<_>>().into_iter())
}

fn recursive_files_of_dir<'b, T: 'static, Iter: Iterator<Item=T>, Mapper: Fn(FileTraversalHelper<'_, 'b, TypedAST>) -> Iter>
(dir: DirectoryTraversalHelper<'_, 'b, TypedAST>,
                                      map_with: &Mapper )
    -> Vec<T>{
    dir.subdirectories_iterator().map(|subdir| recursive_files_of_dir(subdir, map_with).into_iter())
        .flatten()
        .chain(dir.files_iterator().map(map_with).flatten()).collect()
}