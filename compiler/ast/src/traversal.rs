use crate::symbol::{SymbolTable, TypeParameterSymbol};
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::ASTType;
use std::fmt::Debug;

pub mod directory_traversal;
pub mod enum_traversal;
pub mod file_traversal;
pub mod function_traversal;
pub mod statement_traversal;
pub mod struct_traversal;

/// Any type that includes symbols
///
/// This trait is dyn compatible
pub trait HasSymbols<'b, Type: ASTType>: Debug {
    /// Gets a symbol table containing all symbols available to self.
    ///
    /// The where condition ensures that this function can't be used in a dyn setting and therefore
    /// retains dyn compatibility
    fn symbols(&self) -> impl SymbolTable<'b, Type> + '_
    where
        Self: Sized;

    /// Like [`Self::symbols`], but uses a trait object. This retains dyn compatibility.
    // Providing a default implementation is not possible due to trait bounds
    // Removing Self: Sized from symbols() is not possible either as it would remove dyn compatibility
    fn symbols_trait_object(&self) -> Box<dyn SymbolTable<'b, Type> + '_>;
}

/// A traversalhelper that contains Functions
pub trait FunctionContainer<'b, Type: ASTType> {
    /// Gets the length of functions that self contains
    fn len_functions(&self) -> usize;

    /// Gets the function at index
    ///
    /// # Errors
    ///
    /// Errors if `index > self.len_functions()`
    fn index_function(&self, index: usize) -> Option<FunctionTraversalHelper<'_, 'b, Type>>;

    // TODO
    /// Gets the function with the specified name
    /// Returns None if it doesn't exist
    fn function_by_name(&self, name: &str) -> Option<FunctionTraversalHelper<'_, 'b, Type>> {
        self.function_iterator()
            .find(|function| function.inner().declaration().name() == name)
    }
    /// Gets an iterator over all functions
    fn function_iterator<'c>(
        &'c self,
    ) -> impl DoubleEndedIterator<Item = FunctionTraversalHelper<'c, 'b, Type>> + 'c
    where
        'b: 'c;
}
