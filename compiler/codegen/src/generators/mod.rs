use ast::traversal::directory_traversal::DirectoryTraversalHelper;

use crate::{Codegen, context::LLVMContext, errors::CodegenError};

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

        for project in helper.subdirectories_iterator() {
            llvm_context
                .add_module(project.inner().name())
                .ok_or_else(|| {
                    CodegenError::DuplicateProjectName(project.inner().name().to_string())
                })?;
        }

        todo!()
    }
}
