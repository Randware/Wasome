use std::{collections::HashMap, rc::Rc};

use ast::{
    symbol::{EnumSymbol, FunctionSymbol, StructSymbol},
    TypedAST,
};
use inkwell::values::FunctionValue;
use crate::symbols::enum_information::EnumInformation;
use crate::symbols::struct_information::StructInformation;

pub struct SymbolRegistry<'ctx> {
    structs: HashMap<Rc<StructSymbol<TypedAST>>, StructInformation<'ctx>>,
    enums: HashMap<Rc<EnumSymbol<TypedAST>>, EnumInformation<'ctx>>,
    functions: HashMap<Rc<FunctionSymbol<TypedAST>>, FunctionValue<'ctx>>,
}

impl<'ctx> SymbolRegistry<'ctx> {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            enums: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn register_struct(
        &mut self,
        symbol: Rc<StructSymbol<TypedAST>>,
        struct_type: StructInformation<'ctx>,
    ) -> Option<StructInformation<'ctx>> {
        self.structs.insert(symbol, struct_type)
    }

    pub fn get_struct(&self, symbol: &StructSymbol<TypedAST>) -> Option<&StructInformation<'ctx>> {
        self.structs.get(symbol)
    }

    pub fn get_struct_mut(
        &mut self,
        symbol: &StructSymbol<TypedAST>,
    ) -> Option<&mut StructInformation<'ctx>> {
        self.structs.get_mut(symbol)
    }

    pub fn register_enum(
        &mut self,
        symbol: Rc<EnumSymbol<TypedAST>>,
        struct_type: EnumInformation<'ctx>,
    ) -> Option<EnumInformation<'ctx>> {
        self.enums.insert(symbol, struct_type)
    }

    pub fn get_enum(&self, symbol: &EnumSymbol<TypedAST>) -> Option<&EnumInformation<'ctx>> {
        self.enums.get(symbol)
    }

    pub fn get_enum_mut(
        &mut self,
        symbol: &EnumSymbol<TypedAST>,
    ) -> Option<&mut EnumInformation<'ctx>> {
        self.enums.get_mut(symbol)
    }

    pub fn register_function(
        &mut self,
        symbol: Rc<FunctionSymbol<TypedAST>>,
        function_value: FunctionValue<'ctx>,
    ) -> Option<FunctionValue<'ctx>> {
        self.functions.insert(symbol, function_value)
    }

    pub fn get_function(&self, symbol: &FunctionSymbol<TypedAST>) -> Option<FunctionValue<'ctx>> {
        self.functions.get(symbol).copied()
    }
}
