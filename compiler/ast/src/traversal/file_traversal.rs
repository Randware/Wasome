use crate::file::File;
use crate::top_level::Function;
use crate::traversal::directory_traversal::DirectoryTraversalHelper;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{AST, ASTNode, ASTType};
use std::ops::Deref;
use std::path::PathBuf;

#[derive(Debug)]
pub struct FileTraversalHelper<'a, Type: ASTType> {
    inner: &'a ASTNode<File<Type>, PathBuf>,
    parent: &'a DirectoryTraversalHelper<'a, Type>, // TODO: Files can be standalone
}

impl<'a, Type: ASTType> FileTraversalHelper<'a, Type> {
    pub fn new(
        inner: &'a ASTNode<File<Type>, PathBuf>,
        parent: &'a DirectoryTraversalHelper<'a, Type>,
    ) -> Self {
        Self { inner, parent }
    }
    pub fn inner(&self) -> &'a ASTNode<File<Type>, PathBuf> {
        self.inner
    }
    pub fn len_functions(&self) -> usize {
        self.inner.functions().len()
    }
    pub fn index_function(&self, index: usize) -> FunctionTraversalHelper<'_, Type> {
        FunctionTraversalHelper::new(&self.inner.functions()[index], self)
    }

    pub fn specific_function(&self, name: &str) -> Option<FunctionTraversalHelper<'_, Type>> {
        self.function_iterator()
            .filter(|function| function.declaration().name() == name)
            .next()
    }

    pub fn function_iterator<'b>(
        &'b self,
    ) -> impl Iterator<Item = FunctionTraversalHelper<'b, Type>> + 'b {
        self.inner
            .functions()
            .iter()
            .map(|function| FunctionTraversalHelper::new(function, self))
    }
}

impl<'a, Type: ASTType> Deref for FileTraversalHelper<'a, Type> {
    type Target = File<Type>;

    fn deref(&self) -> &'a Self::Target {
        self.inner
    }
}
