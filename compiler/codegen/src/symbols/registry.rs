use std::{collections::HashMap, rc::Rc};

use crate::symbols::enum_information::EnumInformation;
use crate::symbols::struct_information::StructInformation;
use ast::{
    symbol::{EnumSymbol, FunctionSymbol, StructSymbol},
    TypedAST,
};
use inkwell::values::FunctionValue;

/// Maps AST symbol definitions to their lowered LLVM representations.
///
/// The [`SymbolRegistry`] is the central lookup table that connects AST-level symbols
/// (structs, enums, and functions) to their LLVM IR equivalents. It is stored inside
/// [`LLVMContext`](crate::context::LLVMContext) and accessed throughout the code generation pipeline.
///
/// The registry is populated in phases:
/// 1. Structs and enums are registered with opaque types during the registration phase
/// 2. Struct and enum types are filled with complete layouts during the fill phase
/// 3. Functions are registered with their LLVM [`FunctionValue`] during the registration phase
/// 4. Drop functions are generated for structs and enums
pub struct SymbolRegistry<'ctx> {
    /// Maps struct symbols to their [`StructInformation`].
    structs: HashMap<Rc<StructSymbol<TypedAST>>, StructInformation<'ctx>>,
    /// Maps enum symbols to their [`EnumInformation`].
    enums: HashMap<Rc<EnumSymbol<TypedAST>>, EnumInformation<'ctx>>,
    /// Maps function symbols to their LLVM [`FunctionValue`].
    functions: HashMap<Rc<FunctionSymbol<TypedAST>>, FunctionValue<'ctx>>,
}

impl Default for SymbolRegistry<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> SymbolRegistry<'ctx> {
    /// Creates an empty symbol registry.
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            enums: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    /// Registers a struct symbol with its lowered [`StructInformation`].
    ///
    /// Called during the struct registration phase. The struct type starts as opaque
    /// and is later filled with the complete field layout.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The struct symbol to register
    /// * `struct_type` - The [`StructInformation`] containing the opaque LLVM type and drop function
    ///
    /// # Returns
    ///
    /// The previous entry if one existed (should be `None` for well-formed programs).
    pub fn register_struct(
        &mut self,
        symbol: Rc<StructSymbol<TypedAST>>,
        struct_type: StructInformation<'ctx>,
    ) -> Option<StructInformation<'ctx>> {
        self.structs.insert(symbol, struct_type)
    }

    /// Retrieves [`StructInformation`] for a struct symbol.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The struct symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(&StructInformation)` - The struct's lowered information
    /// * `None` - The struct is not registered
    pub fn get_struct(&self, symbol: &StructSymbol<TypedAST>) -> Option<&StructInformation<'ctx>> {
        self.structs.get(symbol)
    }

    /// Retrieves mutable [`StructInformation`] for a struct symbol.
    ///
    /// Used during the fill phase to update the struct's LLVM type body and field list.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The struct symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(&mut StructInformation)` - Mutable access to the struct's lowered information
    /// * `None` - The struct is not registered
    pub fn get_struct_mut(
        &mut self,
        symbol: &StructSymbol<TypedAST>,
    ) -> Option<&mut StructInformation<'ctx>> {
        self.structs.get_mut(symbol)
    }

    /// Registers an enum symbol with its lowered [`EnumInformation`].
    ///
    /// Called during the enum registration phase. The enum variants are filled in during
    /// the fill phase.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The enum symbol to register
    /// * `struct_type` - The [`EnumInformation`] containing the drop function
    ///
    /// # Returns
    ///
    /// The previous entry if one existed (should be `None` for well-formed programs).
    pub fn register_enum(
        &mut self,
        symbol: Rc<EnumSymbol<TypedAST>>,
        struct_type: EnumInformation<'ctx>,
    ) -> Option<EnumInformation<'ctx>> {
        self.enums.insert(symbol, struct_type)
    }

    /// Retrieves [`EnumInformation`] for an enum symbol.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The enum symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(&EnumInformation)` - The enum's lowered information
    /// * `None` - The enum is not registered
    pub fn get_enum(&self, symbol: &EnumSymbol<TypedAST>) -> Option<&EnumInformation<'ctx>> {
        self.enums.get(symbol)
    }

    /// Retrieves mutable [`EnumInformation`] for an enum symbol.
    ///
    /// Used during the fill phase to add variant types.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The enum symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(&mut EnumInformation)` - Mutable access to the enum's lowered information
    /// * `None` - The enum is not registered
    pub fn get_enum_mut(
        &mut self,
        symbol: &EnumSymbol<TypedAST>,
    ) -> Option<&mut EnumInformation<'ctx>> {
        self.enums.get_mut(symbol)
    }

    /// Registers a function symbol with its LLVM [`FunctionValue`].
    ///
    /// Called during the function registration phase.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The function symbol to register
    /// * `function_value` - The LLVM [`FunctionValue`] for the function
    ///
    /// # Returns
    ///
    /// The previous entry if one existed (should be `None` for well-formed programs).
    pub fn register_function(
        &mut self,
        symbol: Rc<FunctionSymbol<TypedAST>>,
        function_value: FunctionValue<'ctx>,
    ) -> Option<FunctionValue<'ctx>> {
        self.functions.insert(symbol, function_value)
    }

    /// Retrieves the LLVM [`FunctionValue`] for a function symbol.
    ///
    /// # Arguments
    ///
    /// * `symbol` - The function symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(FunctionValue)` - The LLVM function value
    /// * `None` - The function is not registered
    pub fn get_function(&self, symbol: &FunctionSymbol<TypedAST>) -> Option<FunctionValue<'ctx>> {
        self.functions.get(symbol).copied()
    }
}
