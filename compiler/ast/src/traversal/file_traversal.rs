use crate::file::File;
use crate::symbol::{Symbol, SymbolTable};
use crate::top_level::Import;
use crate::traversal::directory_traversal::DirectoryTraversalHelper;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{ASTNode, ASTType};
use std::path::PathBuf;
use itertools::Itertools;
use crate::traversal::{FunctionContainer, HasSymbols};
use crate::traversal::statement_traversal::StatementTraversalHelper;

/** This struct helps with traversing files
It keeps a reference to a file and its parent (directory).

### Lifetimes

| Lifetime     | Purpose      |
| ------------- | ------------- |
| 'a | How long the traversal helper may life |
| 'b | How long the underlying data may life |
*/
#[derive(Debug)]
pub struct FileTraversalHelper<'a, 'b, Type: ASTType> {
    inner: &'b ASTNode<File<Type>, PathBuf>,
    parent: &'a DirectoryTraversalHelper<'a, 'b, Type>,
}

impl<'a, 'b, Type: ASTType> FileTraversalHelper<'a, 'b, Type> {
    /** Creates a new instance of self
     */
    pub(crate) fn new(
        inner: &'b ASTNode<File<Type>, PathBuf>,
        parent: &'a DirectoryTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self { inner, parent }
    }
    /** Gets the inner file
     */
    pub fn inner(&self) -> &'b ASTNode<File<Type>, PathBuf> {
        self.inner
    }

    /** Gets the symbol imported by a specific import
      Returns None if it doesn't exist
    */
    pub fn resolve_import(&self, to_resolve: &Import) -> Option<Symbol<'b, Type>> {
        self.parent.resolve_import(to_resolve)
    }
}

impl<'a, 'b, Type: ASTType> FunctionContainer<'b, Type> for FileTraversalHelper<'a, 'b, Type>
{
    fn len_functions(&self) -> usize {
        self.inner.functions().len()
    }

    fn index_function(&self, index: usize) -> FunctionTraversalHelper<'_, 'b, Type> {
        FunctionTraversalHelper::new(&self.inner.functions()[index], self)
    }

    fn function_iterator<'c>(&'c self) -> impl DoubleEndedIterator<Item=FunctionTraversalHelper<'c, 'b, Type>> + 'c
    where
        'b: 'c
    {
        self.inner
            .functions()
            .iter()
            .map(|function| FunctionTraversalHelper::new(function, self))
    }
}

impl<'a, 'b, Type: ASTType> HasSymbols<'b, Type> for FileTraversalHelper<'a, 'b, Type>
{
    fn symbols<'c>(&'c self) -> impl SymbolTable<'b, Type> + 'c {
        FileSymbolTable::new_file_traversal_helper(self)
    }

    fn symbols_trait_object(&self) -> Box<dyn SymbolTable<'b, Type> + '_> {
        Box::new(self.symbols())
    }
}


struct FileSymbolTable<'a, 'b, Type: ASTType> {
    symbols: Box<dyn Iterator<Item = Symbol<'b, Type>> + 'a>,
}

impl<'a, 'b, Type: ASTType> FileSymbolTable<'a, 'b, Type> {
    pub(crate) fn new_file_traversal_helper(
        symbol_source: &'a FileTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self {
            symbols: Box::new(
                symbol_source
                    .inner
                    .imports()
                    .iter()
                    // All imports in an ast must be valid
                    // A FileSymbolTable can not exist without an ast
                    // Therefore, we can't have an unresolved import here and can safely unwrap
                    .map(|import| symbol_source.resolve_import(import).unwrap())
                    .chain(symbol_source.function_iterator().map(|function| Symbol::Function(function.inner().declaration())))
                    .unique(),
            ),
        }
    }
}

impl<'a, 'b, Type: ASTType> Iterator for FileSymbolTable<'a, 'b, Type> {
    type Item = Symbol<'b, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.next()
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for FileSymbolTable<'a, 'b, Type> {}
