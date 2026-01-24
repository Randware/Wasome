use crate::composite::{Enum, Struct};
use crate::symbol::{
    DirectlyAvailableSymbol, EnumSymbol, FunctionSymbol, StructSymbol, SymbolWithTypeParameter,
};
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
    enums: Vec<ASTNode<Enum<Type>>>,
    structs: Vec<ASTNode<Struct<Type>>>,
}

impl<Type: ASTType> File<Type> {
    pub fn new(
        name: String,
        imports: Vec<ASTNode<Import>>,
        functions: Vec<ASTNode<Function<Type>>>,
        enums: Vec<ASTNode<Enum<Type>>>,
        structs: Vec<ASTNode<Struct<Type>>>,
    ) -> Self {
        Self {
            name,
            imports,
            functions,
            enums,
            structs,
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

    pub fn enums(&self) -> &Vec<ASTNode<Enum<Type>>> {
        &self.enums
    }

    pub fn structs(&self) -> &Vec<ASTNode<Struct<Type>>> {
        &self.structs
    }

    /// Gets the symbol with the specified identifier
    pub fn symbol(&self, identifier: Type::SymbolIdentifier<'_>,) -> Option<DirectlyAvailableSymbol<'_, Type>> {
        self.symbol_chosen_public(identifier, true)
    }

    /// Gets the symbol with the specified identifier if it is public
    pub fn symbol_public(&self, identifier: Type::SymbolIdentifier<'_>,) -> Option<DirectlyAvailableSymbol<'_, Type>> {
        self.symbol_chosen_public(identifier, false)
    }

    /// Gets the requested symbol. It does not differentiate between function- and non-function
    /// symbols.
    ///
    /// # Parameter
    ///
    /// - `identifier`: The identifier of the symbol
    ///     - If the symbol has no type parameters, this may have neither
    /// - `only_public`: If true, the symbol is only returned if it is public. Otherwise, there is no filtering
    ///
    /// # Return
    ///
    /// - `None`: If no symbol was found
    /// - `Some(<Symbol>)`: If a symbol was found
    fn symbol_chosen_public(
        &self,
        identifier: Type::SymbolIdentifier<'_>,
        only_public: bool,
    ) -> Option<DirectlyAvailableSymbol<'_, Type>> {
        self.symbols_chosen_public(only_public)
            .find(|symbol| symbol.matches_identifier(identifier))
    }

    /// Gets the function with the specified name
    pub fn function_by_identifier(&self, identifier: Type::SymbolIdentifier<'_>,) -> Option<&ASTNode<Function<Type>>> {
        self.function_iterator()
            .find(|function| Type::symbol_with_type_parameter_matches_identifier(identifier, function.declaration()))
    }

    /// Gets an iterator over all functions inside this file
    pub fn function_iterator(&self) -> impl Iterator<Item = &ASTNode<Function<Type>>> {
        self.functions().iter()
    }

    /// Gets the struct with a specified identifier
    pub fn struct_by_identifier(
        &self,
        identifier: Type::SymbolIdentifier<'_>,
    ) -> Option<&ASTNode<Struct<Type>>> {
        self.structs()
            .iter()
            .find(|st| Type::symbol_with_type_parameter_matches_identifier(identifier, st.symbol()))
    }

    /// Gets the enum with the specified identifier
    pub fn enum_by_identifier(
        &self,
        identifier: Type::SymbolIdentifier<'_>,
    ) -> Option<&ASTNode<Enum<Type>>> {
        self.enums()
            .iter()
            .find(|en| Type::symbol_with_type_parameter_matches_identifier(identifier, en.symbol()))
    }

    /// Gets an iterator over all enums
    pub fn enum_iterator(&self) -> impl Iterator<Item = &ASTNode<Enum<Type>>> {
        self.enums().iter()
    }

    /// Gets an iterator over all structs
    pub fn struct_iterator(&self) -> impl Iterator<Item = &ASTNode<Struct<Type>>> {
        self.structs().iter()
    }

    /// Gets symbols, including non-public ones. It does not differentiate between function- and non-function
    /// symbols.
    /// - Note that currently, there are only function symbols
    ///
    /// # Return
    ///
    /// The requested symbols
    pub fn symbols(&self) -> impl Iterator<Item = DirectlyAvailableSymbol<'_, Type>> {
        self.symbols_chosen_public(false)
    }

    /// Gets public symbols. It does not differentiate between function- and non-function
    /// symbols.
    /// - Note that currently, there are only function symbols
    /// # Return
    /// The requested symbols
    pub fn symbols_public(&self) -> impl Iterator<Item = DirectlyAvailableSymbol<'_, Type>> {
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
    fn symbols_chosen_public(
        &self,
        only_public: bool,
    ) -> impl Iterator<Item = DirectlyAvailableSymbol<'_, Type>> {
        self.function_symbols(only_public)
            .map(|function_symbol| DirectlyAvailableSymbol::Function(function_symbol))
            .chain(
                self.enum_symbols(only_public)
                    .map(|enum_symbol| DirectlyAvailableSymbol::Enum(enum_symbol)),
            )
            .chain(
                self.struct_symbols(only_public)
                    .map(|struct_symbol| DirectlyAvailableSymbol::Struct(struct_symbol)),
            )
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
        self.function_iterator()
            .filter(move |function| !only_public || (*function).visibility() == Visibility::Public)
            .map(|function| function.declaration())
    }

    /// Gets struct symbols
    ///
    /// # Parameter
    ///
    /// - `only_public`: Decides if only public function symbols (`true`)
    ///   or all function symbols (`false`) should be returned
    ///
    /// # Return
    ///
    /// An iterator over the struct symbols. Note that it may be empty if there are no function
    /// symbols that meet the provided requirement
    fn struct_symbols(&self, only_public: bool) -> impl Iterator<Item = &StructSymbol<Type>> {
        self.struct_iterator()
            .filter(move |stru| !only_public || (*stru).visibility() == Visibility::Public)
            .map(|stru| stru.symbol())
    }

    /// Gets enum symbols
    ///
    /// # Parameter
    ///
    /// - `only_public`: Decides if only public function symbols (`true`)
    ///   or all function symbols (`false`) should be returned
    ///
    /// # Return
    ///
    /// An iterator over the struct symbols. Note that it may be empty if there are no function
    /// symbols that meet the provided requirement
    fn enum_symbols(&self, only_public: bool) -> impl Iterator<Item = &EnumSymbol<Type>> {
        self.enum_iterator()
            .filter(move |en| !only_public || (*en).visibility() == Visibility::Public)
            .map(|en| en.symbol())
    }
}

impl<Type: ASTType> SemanticEq for File<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.imports().semantic_eq(other.imports())
            && self.functions().semantic_eq(other.functions())
            && self.enums().semantic_eq(other.enums())
            && self.structs().semantic_eq(other.structs())
    }
}

/// This represents all composites
/// inside a [File]
pub struct Composites<Type: ASTType> {
    enums: Vec<ASTNode<Enum<Type>>>,
    structs: Vec<ASTNode<Struct<Type>>>,
}

impl<Type: ASTType> Composites<Type> {
    pub fn new(enums: Vec<ASTNode<Enum<Type>>>, structs: Vec<ASTNode<Struct<Type>>>) -> Self {
        Self { enums, structs }
    }

    pub fn enums(&self) -> &Vec<ASTNode<Enum<Type>>> {
        &self.enums
    }

    pub fn structs(&self) -> &Vec<ASTNode<Struct<Type>>> {
        &self.structs
    }
}
