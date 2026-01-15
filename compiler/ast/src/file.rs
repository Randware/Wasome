use crate::{ASTNode, ASTType, SemanticEq};
use crate::composite::{Enum, Struct};
use crate::symbol::{DirectlyAvailableSymbol, EnumSymbol, FunctionSymbol, StructSymbol};
use crate::top_level::{Function, Import};
use crate::visibility::{Visibility, Visible};

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

    /// Gets the symbol with the specified origin if it is public or outside is false
    fn symbol_specified_origin(
        &self,
        origin: &[String],
        outside: bool,
    ) -> Option<DirectlyAvailableSymbol<'_, Type>> {
        // Symbols can be a direct function...
        self.function_symbol(origin.first()?, outside)
            .map(|function_symbol| DirectlyAvailableSymbol::Function(function_symbol))
            // ... or a function in a struct ...
            .or_else(|| {
                Some(DirectlyAvailableSymbol::Function(
                    self.struct_by_name(&origin[0])?
                        .function_symbol(origin.get(1)?, outside)?,
                ))
            })
            // ... or a struct
            .or_else(|| {
                Some(DirectlyAvailableSymbol::Struct(
                    self.struct_by_name(&origin[0])?.symbol(),
                ))
            })
            // ... or an enum
            .or_else(|| {
                Some(DirectlyAvailableSymbol::Enum(
                    self.enum_by_name(&origin[0])?.symbol(),
                ))
            })
    }

    /// Gets the symbol with the specified name
    pub fn symbol(&self, name: &str) -> Option<DirectlyAvailableSymbol<'_, Type>> {
        self.symbol_chosen_public(name, true)
    }

    /// Gets the symbol with the specified name if it is public
    pub fn symbol_public(&self, name: &str) -> Option<DirectlyAvailableSymbol<'_, Type>> {
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
    fn symbol_chosen_public(&self, name: &str, only_public: bool) -> Option<DirectlyAvailableSymbol<'_, Type>> {
        self.symbols_chosen_public(only_public)
            .find(|symbol| symbol.name() == name)
    }

    /// Gets the function with the specified name
    pub fn specific_function(&self, name: &str) -> Option<&ASTNode<Function<Type>>> {
        self.functions()
            .iter()
            .find(|function| function.declaration().name() == name)
    }

    /// Gets the function with the specified name if it is public or only_public is false
    fn function_symbol(&self, name: &str, only_public: bool) -> Option<&FunctionSymbol<Type>> {
        self.function_by_name(name)
            .filter(|function| !only_public || function.visibility() == Visibility::Public)
            .map(|function| function.declaration())
    }

    /// Gets the function with the specified name
    pub fn function_by_name(&self, name: &str) -> Option<&ASTNode<Function<Type>>> {
        self.function_iterator()
            .find(|function| function.declaration().name() == name)
    }

    /// Gets an iterator over all functions inside this file
    pub fn function_iterator(&self) -> impl Iterator<Item = &ASTNode<Function<Type>>> {
        self.functions().iter()
    }

    /// Gets the struct with the specified name
    pub fn struct_by_name(&self, name: &str) -> Option<&ASTNode<Struct<Type>>> {
        self.structs().iter().find(|st| st.symbol().name() == name)
    }

    /// Gets the enum with the specified name
    pub fn enum_by_name(&self, name: &str) -> Option<&ASTNode<Enum<Type>>> {
        self.enums().iter().find(|en| en.symbol().name() == name)
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
    fn symbols_chosen_public(&self, only_public: bool) -> impl Iterator<Item = DirectlyAvailableSymbol<'_, Type>> {
        self.function_symbols(only_public)
            .map(|function_symbol| DirectlyAvailableSymbol::Function(function_symbol))
            .chain(self.enum_symbols(only_public)
                .map(|enum_symbol| DirectlyAvailableSymbol::Enum(enum_symbol)))
            .chain(self.struct_symbols(only_public)
                .map(|struct_symbol| DirectlyAvailableSymbol::Struct(struct_symbol)))
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
    fn struct_symbols(&self, only_public: bool) -> impl Iterator<Item = &StructSymbol> {
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
    fn enum_symbols(&self, only_public: bool) -> impl Iterator<Item = &EnumSymbol> {
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
