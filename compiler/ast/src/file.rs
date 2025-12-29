use crate::symbol::{FunctionSymbol, Symbol};
use crate::top_level::{Function, Import};
use crate::visibility::{Visibility, Visible};
use crate::{ASTNode, ASTType, SemanticEq};

/// A file containing code
///
/// Files are located in directories
///
/// # Contents
///
/// It has a name, imports and functions
/// + imports and functions may both be empty vecs
#[derive(Debug, PartialEq)]
pub struct File<Type: ASTType> {
    /// Filename without the file extension
    name: String,
    imports: Vec<ASTNode<Import>>,
    functions: Vec<ASTNode<Function<Type>>>,
}

impl<Type: ASTType> File<Type> {
    pub fn new(
        name: String,
        imports: Vec<ASTNode<Import>>,
        functions: Vec<ASTNode<Function<Type>>>,
    ) -> Self {
        Self {
            name,
            imports,
            functions,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn imports(&self) -> &[ASTNode<Import>] {
        &self.imports
    }

    pub fn functions(&self) -> &[ASTNode<Function<Type>>] {
        &self.functions
    }

    /// Gets the symbol with the specified name
    pub fn symbol(&self, name: &str) -> Option<Symbol<'_, Type>> {
        self.symbol_chosen_public(name, true)
    }

    /// Gets the symbol with the specified name if it is public
    pub fn symbol_public(&self, name: &str) -> Option<Symbol<'_, Type>> {
        self.symbol_chosen_public(name, false)
    }

    /// Gets the requested symbol. It does not differentiate between function- and non-function
    /// symbols.
    /// - Note that currently, there are only function symbols
    ///
    /// # Parameter
    ///
    /// - `only_public`: If true, the symbol is only returned if it is public. Otherwise, there is no filtering
    ///
    /// # Return
    ///
    /// - `None`: If no symbol was found
    /// - `Some(<Symbol>)`: If a symbol was found
    fn symbol_chosen_public(&self, name: &str, only_public: bool) -> Option<Symbol<'_, Type>> {
        self.symbols_chosen_public(only_public)
            .find(|symbol| symbol.name() == name)
    }

    /// Gets the function with the specified name
    pub fn specific_function(&self, name: &str) -> Option<&ASTNode<Function<Type>>> {
        self.functions()
            .iter()
            .find(|function| function.declaration().name() == name)
    }

    /// Gets symbols, including non-public ones. It does not differentiate between function- and non-function
    /// symbols.
    /// - Note that currently, there are only function symbols
    ///
    /// # Return
    ///
    /// The requested symbols
    pub fn symbols(&self) -> impl Iterator<Item = Symbol<'_, Type>> {
        self.symbols_chosen_public(false)
    }

    /// Gets public symbols. It does not differentiate between function- and non-function
    /// symbols.
    /// - Note that currently, there are only function symbols
    /// # Return
    /// The requested symbols
    pub fn symbols_public(&self) -> impl Iterator<Item = Symbol<'_, Type>> {
        self.symbols_chosen_public(true)
    }

    /// Gets the requested symbols. It does not differentiate between function- and non-function
    /// symbols.
    /// - Note that currently, there are only function symbols
    ///
    /// # Parameter
    ///
    /// - `only_public`: If true, only `pub` symbols are requested. Otherwise, all are requested
    ///
    /// # Return
    ///
    /// The requested symbols
    fn symbols_chosen_public(&self, only_public: bool) -> impl Iterator<Item = Symbol<'_, Type>> {
        self.function_symbols(only_public)
            .map(|function_symbol| Symbol::Function(function_symbol))
    }

    /// Gets function symbols
    ///
    /// # Parameter
    ///
    /// - `only_public`: Decides if only public function symbols (`true`)
    ///   or all function symbols (`false`) should be returned
    ///
    /// # Return
    ///
    /// An iterator over the function symbols. Note that it may be empty if there are no function
    /// symbols that meet the provided requirement
    fn function_symbols(&self, only_public: bool) -> impl Iterator<Item = &FunctionSymbol<Type>> {
        self.functions()
            .iter()
            .filter(move |function| !only_public || (*function).visibility() == Visibility::Public)
            .map(|function| function.declaration())
    }
}

impl<Type: ASTType> SemanticEq for File<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.imports().semantic_eq(other.imports())
            && self.functions().semantic_eq(other.functions())
    }
}
