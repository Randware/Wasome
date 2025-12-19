use crate::file::File;
use crate::symbol::{Symbol, SymbolTable};
use crate::top_level::Import;
use crate::traversal::directory_traversal::DirectoryTraversalHelper;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{ASTNode, ASTType};
use std::path::PathBuf;

/// This struct helps with traversing files
/// It keeps a reference to a file and its parent (directory).
///
/// # Lifetimes
///
/// | Lifetime     | Purpose      |
/// | ------------- | ------------- |
/// | 'a | How long the traversal helper may life |
/// | 'b | How long the underlying data may life |
#[derive(Debug)]
pub struct FileTraversalHelper<'a, 'b, Type: ASTType> {
    inner: &'b ASTNode<File<Type>, PathBuf>,
    parent: &'a DirectoryTraversalHelper<'a, 'b, Type>,
}

impl<'a, 'b, Type: ASTType> FileTraversalHelper<'a, 'b, Type> {
    ///  Creates a new instance of self
    pub(crate) fn new(
        inner: &'b ASTNode<File<Type>, PathBuf>,
        parent: &'a DirectoryTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self { inner, parent }
    }
    ///  Gets the inner file
    pub fn inner(&self) -> &'b ASTNode<File<Type>, PathBuf> {
        self.inner
    }
    /// Gets the length of functions that self contains
    pub fn len_functions(&self) -> usize {
        self.inner.functions().len()
    }

    /// Gets the function at index
    ///
    /// # Panics
    /// Panics if `index > self.len_functions()`
    pub fn index_function(&self, index: usize) -> FunctionTraversalHelper<'_, 'b, Type> {
        FunctionTraversalHelper::new(&self.inner.functions()[index], self)
    }
    ///  Gets the function with the specified name
    /// Returns None if it doesn't exist
    pub fn function_by_name(&self, name: &str) -> Option<FunctionTraversalHelper<'_, 'b, Type>> {
        self.function_iterator()
            .find(|function| function.inner().declaration().name() == name)
    }
    /// Gets an iterator over all functions
    pub fn function_iterator<'c>(
        &'c self,
    ) -> impl Iterator<Item = FunctionTraversalHelper<'c, 'b, Type>> + 'c {
        self.inner
            .functions()
            .iter()
            .map(|function| FunctionTraversalHelper::new(function, self))
    }
    /// Gets the symbol imported by a specific import
    /// Returns None if it doesn't exist
    pub(crate) fn resolve_import(&self, to_resolve: &Import) -> Option<impl Iterator<Item=Symbol<'b, Type>>> {
        self.parent.resolve_import(to_resolve)
    }
    /// Gets all symbols defined in self
    pub fn symbols(&self) -> impl SymbolTable<'b, Type> {
        FileSymbolTable::new_file_traversal_helper(self)
    }
}

struct FileSymbolTable<'a, 'b, Type: ASTType> {
    symbols: Box<dyn Iterator<Item = (Option<&'b str>, Symbol<'b, Type>)> + 'a>,
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
                    // Also, imports with no import path can never be valid.
                    // So that always returns some
                    .flat_map(|import| symbol_source.resolve_import(import).unwrap()
                        .map(|imported_symbol|
                            (import.inner.path().last()
                                 .map(|path| path.as_str()), imported_symbol))),
            ),
        }
    }
}

impl<'a, 'b, Type: ASTType> Iterator for FileSymbolTable<'a, 'b, Type> {
    type Item = (Option<&'b str>, Symbol<'b, Type>);

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.next()
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for FileSymbolTable<'a, 'b, Type> {}
