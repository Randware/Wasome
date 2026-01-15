use crate::file::File;
use crate::symbol::{ModuleUsageNameSymbol, DirectlyAvailableSymbol, SymbolTable, EnumSymbol, StructSymbol};
use crate::top_level::Import;
use crate::traversal::directory_traversal::DirectoryTraversalHelper;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{ASTNode, ASTType};
use std::iter;
use std::path::PathBuf;
use crate::composite::Enum;
use crate::traversal::{FunctionContainer, HasSymbols};
use crate::traversal::struct_traversal::StructTraversalHelper;

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

    /// Gets the length of the enums
    pub fn len_enums(&self) -> usize {
        self.inner.enums().len()
    }

    /// Gets the enum at a specific index
    ///
    /// ### Panics
    ///
    /// Panics if `index > self.len_enums()`
    pub fn index_enums(&self, index: usize) -> &'b ASTNode<Enum<Type>> {
        &self.inner.enums()[index]
    }

    /// Gets the subdirectory with the specified name.
    ///
    /// Returns None if it doesn't exist
    pub fn enum_by_name(&self, name: &str) -> Option<&'b ASTNode<Enum<Type>>> {
        self.inner().enum_by_name(name)
    }

    /// Gets an iterator over all enums
    pub fn enums_iterator<'c>(&'c self) -> impl Iterator<Item = &'b ASTNode<Enum<Type>>> + 'c {
        self.inner.enum_iterator()
    }

    /// Gets the length of the enums
    pub fn len_structs(&self) -> usize {
        self.inner.structs().len()
    }

    /// Gets the enum at a specific index
    ///
    /// ### Panics
    ///
    /// Panics if `index > self.len_structs()`
    pub fn index_struct(&self, index: usize) -> StructTraversalHelper<'_, 'b, Type> {
        StructTraversalHelper::new(&self.inner.structs()[index], self)
    }

    /// Gets the subdirectory with the specified name.
    ///
    /// Returns None if it doesn't exist
    pub fn struct_by_name(&self, name: &str) -> Option<StructTraversalHelper<'_, 'b, Type>> {
        self.inner()
            .struct_by_name(name)
            .map(|st| StructTraversalHelper::new(st, self))
    }

    /// Gets an iterator over all enums
    pub fn structs_iterator<'c>(
        &'c self,
    ) -> impl Iterator<Item = StructTraversalHelper<'c, 'b, Type>> + 'c {
        self.inner
            .struct_iterator()
            .map(|st| StructTraversalHelper::new(st, self))
    }

    /// Gets the symbols imported by a specific import
    ///
    /// # Return
    ///
    /// - `None` if the import could not be resolved
    /// - `Some(<Symbols>)` if the import was successfully resolved
    pub(crate) fn resolve_import(
        &self,
        to_resolve: &Import,
    ) -> Option<impl Iterator<Item = DirectlyAvailableSymbol<'b, Type>>> {
        self.parent.resolve_import(to_resolve)
    }
}

impl<'a, 'b, Type: ASTType> FunctionContainer<'b, Type> for FileTraversalHelper<'a, 'b, Type> {
    fn len_functions(&self) -> usize {
        self.inner.functions().len()
    }

    fn index_function(&self, index: usize) -> Option<FunctionTraversalHelper<'_, 'b, Type>> {
        Some(FunctionTraversalHelper::new(self.inner.functions().get(index)?, self))
    }

    fn function_iterator<'c>(
        &'c self,
    ) -> impl DoubleEndedIterator<Item = FunctionTraversalHelper<'c, 'b, Type>> + 'c
    where
        'b: 'c,
    {
        self.inner
            .functions()
            .iter()
            .map(|function| FunctionTraversalHelper::new(function, self))
    }
}

impl<'a, 'b, Type: ASTType> HasSymbols<'b, Type> for FileTraversalHelper<'a, 'b, Type> {
    /// Gets all symbols defined in self
    fn symbols(&self) -> impl SymbolTable<'b, Type> {
        FileSymbolTable::new_file_traversal_helper(self)
    }

    fn symbols_trait_object(&self) -> Box<dyn SymbolTable<'b, Type> + '_> {
        Box::new(self.symbols())
    }
}

struct FileSymbolTable<'a, 'b, Type: ASTType> {
    symbols: Box<dyn Iterator<Item = (Option<&'b ModuleUsageNameSymbol>, DirectlyAvailableSymbol<'b, Type>)> + 'a>,
    enum_symbols: Box<dyn Iterator<Item = &'b EnumSymbol> + 'a>,
    struct_symbols: Box<dyn Iterator<Item = &'b StructSymbol> + 'a>,
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
                    .flat_map(|import| {
                        symbol_source
                            .resolve_import(import)
                            .unwrap()
                            .map(|imported_symbol| {
                                (Some(import.inner.usage_name()), imported_symbol)
                            })
                            .chain(iter::once((
                                None,
                                DirectlyAvailableSymbol::ModuleUsageName(import.usage_name()),
                            )))
                    }).chain(symbol_source.function_iterator().map(|func| (None, DirectlyAvailableSymbol::Function(func.inner().declaration())))),
            ),
            enum_symbols: Box::new(symbol_source.enums_iterator().map(|en| en.symbol())),
            struct_symbols: Box::new(
                symbol_source
                    .structs_iterator()
                    .map(|st| st.inner().symbol()),
            ),
        }
    }
}

impl<'a, 'b, Type: ASTType> Iterator for FileSymbolTable<'a, 'b, Type> {
    type Item = (Option<&'b ModuleUsageNameSymbol>, DirectlyAvailableSymbol<'b, Type>);

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols
            .next()
            .or_else(|| {
                self.enum_symbols
                    .next()
                    .map(|en| (None, DirectlyAvailableSymbol::Enum(en)))
            })
            .or_else(|| {
                self.struct_symbols
                    .next()
                    .map(|st| (None, DirectlyAvailableSymbol::Struct(st)))
            })
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for FileSymbolTable<'a, 'b, Type> {}
