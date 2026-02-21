use ast::traversal::directory_traversal::DirectoryTraversalHelper;

use crate::{Codegen, context::LLVMContext, errors::CodegenError, types::ModuleContext};

impl<'ctx> Codegen<'ctx> {
    pub fn compile(&mut self) -> Result<(), CodegenError<'_>> {
        let mut llvm_context = LLVMContext::new(self.context, self.opt_level);
        self.compile_dir(&mut llvm_context)?;
        todo!()
    }

    pub fn compile_dir(
        &mut self,
        llvm_context: &mut LLVMContext<'_>,
    ) -> Result<(), CodegenError<'_>> {
        let helper = DirectoryTraversalHelper::new_from_ast(&self.ast);

        // Pass 1
        for project in helper.subdirectories_iterator() {
            llvm_context
                .add_module(project.inner().name())
                .ok_or_else(|| {
                    CodegenError::DuplicateProjectName(project.inner().name().to_string())
                })?;
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
