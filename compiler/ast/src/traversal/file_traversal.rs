use crate::file::File;
use crate::top_level::{Function, Import};
use crate::traversal::directory_traversal::DirectoryTraversalHelper;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{AST, ASTNode, ASTType};
use std::ops::Deref;
use std::path::PathBuf;
use crate::symbol::Symbol;

#[derive(Debug)]
pub struct FileTraversalHelper<'a, 'b, Type: ASTType> {
    inner: &'b ASTNode<File<Type>, PathBuf>,
    parent: &'a DirectoryTraversalHelper<'a, 'b, Type>, // TODO: Files can be standalone
}

impl<'a, 'b, Type: ASTType> FileTraversalHelper<'a, 'b, Type> {
    pub fn new(
        inner: &'b ASTNode<File<Type>, PathBuf>,
        parent: &'a DirectoryTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self { inner, parent }
    }
    pub fn inner(&self) -> &'b ASTNode<File<Type>, PathBuf> {
        self.inner
    }
    pub fn len_functions(&self) -> usize {
        self.inner.functions().len()
    }
    pub fn index_function(&self, index: usize) -> FunctionTraversalHelper<'_, 'b, Type> {
        FunctionTraversalHelper::new(&self.inner.functions()[index], self)
    }

    pub fn specific_function(&self, name: &str) -> Option<FunctionTraversalHelper<'_, 'b, Type>> {
        self.function_iterator()
            .filter(|function| function.declaration().name() == name)
            .next()
    }

    pub fn function_iterator<'c>(
        &'c self,
    ) -> impl Iterator<Item = FunctionTraversalHelper<'c, 'b, Type>> + 'c {
        self.inner
            .functions()
            .iter()
            .map(|function| FunctionTraversalHelper::new(function, self))
    }

    pub fn resolve_import(&self, to_resolve: &Import) -> Option<Symbol<'a, Type>>
    {
        self.parent.resolve_import(to_resolve)
    }
}

impl<'a, 'b, Type: ASTType> Deref for FileTraversalHelper<'a, 'b, Type> {
    type Target = File<Type>;

    fn deref(&self) -> &'b Self::Target {
        self.inner
    }
}
