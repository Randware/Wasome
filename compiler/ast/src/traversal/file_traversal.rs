use crate::file::File;
use crate::top_level::{Function, Import};
use crate::traversal::directory_traversal::DirectoryTraversalHelper;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{AST, ASTNode, ASTType};
use std::ops::Deref;
use std::path::PathBuf;
use crate::symbol::{Symbol, SymbolTable};

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
            .filter(|function| function.inner().declaration().name() == name)
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

    pub fn resolve_import(&self, to_resolve: &Import) -> Option<Symbol<'b, Type>>
    {
        self.parent.resolve_import(to_resolve)
    }

    pub fn symbols(&self) -> impl SymbolTable<'b, Type>
    {
        FileSymbolTable::new_file_traversal_helper(self)
    }
}

struct FileSymbolTable<'a, 'b, Type: ASTType>
{
    symbols: Box<dyn Iterator<Item=Symbol<'b, Type>> + 'a>
}

impl<'a, 'b, Type: ASTType> FileSymbolTable<'a, 'b, Type>
{
    pub(crate) fn new_file_traversal_helper(symbol_source: &'a FileTraversalHelper<'a, 'b, Type>) -> Self {
        Self {
            symbols: Box::new(symbol_source.inner.imports().iter().filter_map(|import| symbol_source.resolve_import(import)))
        }
    }
}

impl<'a, 'b, Type: ASTType> Iterator for FileSymbolTable<'a, 'b, Type>
{
    type Item = Symbol<'b, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.next()
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for FileSymbolTable<'a, 'b, Type> {}
