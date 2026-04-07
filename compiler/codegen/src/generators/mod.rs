use std::collections::btree_map::Iter;
use ast::file::File;
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::traversal::file_traversal::FileTraversalHelper;
use ast::TypedAST;
use crate::{Codegen, context::LLVMContext, errors::CodegenError, types::ModuleContext};

impl<'ctx> Codegen<'ctx> {
    pub fn compile(&mut self) -> Result<(), CodegenError<'_>> {
        let mut llvm_context = LLVMContext::new(self.context, self.opt_level);
        self.compile_project(&mut llvm_context)?;
        todo!()
    }

    pub fn compile_project(
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

pub fn recursive_files_of_dir<'a, 'b: 'a, T: 'static>(dir: DirectoryTraversalHelper<'b, 'b, TypedAST>,
                                      map_with: &impl Fn(FileTraversalHelper<'a, 'b, TypedAST>) -> T)
    -> Box<dyn Iterator<Item=T>> {
    Box::new(dir.subdirectories_iterator().map(|subdir| recursive_files_of_dir(subdir, map_with))
        .flatten()
        .chain(dir.files_iterator().map(map_with)))
}

/*struct FileIterator<'a, 'b> {
    dir: DirectoryTraversalHelper<'a, 'b, TypedAST>,
    subdirs_left: Vec<FileIterator<'a, 'b>>,
    files_left: Vec<FileTraversalHelper<'a, 'b, TypedAST>>
}

impl<'a, 'b> FileIterator<'a, 'b> {
    pub fn new(dir: DirectoryTraversalHelper<'b, 'b, TypedAST>) -> Self {
        let mut to_ret = Self {
            dir,
            subdirs_left: Vec::new(),
            files_left: Vec::new()
        };
        to_ret.files_left = to_ret.dir.files_iterator().collect();
        to_ret.subdirs_left = to_ret.dir.subdirectories_iterator().map(|subdir| FileIterator::new(subdir)).collect();
        to_ret
    }
}

impl<'a, 'b> Iterator for FileIterator<'a, 'b> {
    type Item = FileTraversalHelper<'a, 'b, TypedAST>;

    fn next(&mut self) -> Option<Self::Item> {
        self.files_left.pop()
            .or_else(|| {
                if self.subdirs_left.is_empty() {
                    return None;
                }
                let file = self.subdirs_left.last_mut()?.next();
                match file {
                    None => {
                        self.subdirs_left.pop();
                        self.next()
                    }
                    Some(val) => Some(val)
                }

            })
    }
}*/