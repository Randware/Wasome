use std::{collections::HashMap, rc::Rc};

use ast::{
    TypedAST,
    symbol::{EnumSymbol, FunctionSymbol, StructSymbol},
};
use inkwell::{types::StructType, values::FunctionValue};

pub struct SymbolRegistry<'ctx> {
    structs: HashMap<Rc<StructSymbol<TypedAST>>, StructType<'ctx>>,
    enums: HashMap<Rc<EnumSymbol<TypedAST>>, StructType<'ctx>>,
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
        struct_type: StructType<'ctx>,
    ) -> Option<StructType<'ctx>> {
        self.structs.insert(symbol, struct_type)
    }

    pub fn get_struct(&self, symbol: &Rc<StructSymbol<TypedAST>>) -> Option<StructType<'ctx>> {
        self.structs.get(symbol).copied()
    }

    pub fn register_enum(
        &mut self,
        symbol: Rc<EnumSymbol<TypedAST>>,
        struct_type: StructType<'ctx>,
    ) -> Option<StructType<'ctx>> {
        self.enums.insert(symbol, struct_type)
    }

    pub fn get_enum(&self, symbol: &Rc<EnumSymbol<TypedAST>>) -> Option<StructType<'ctx>> {
        self.enums.get(symbol).copied()
    }

    pub fn register_function(
        &mut self,
        symbol: Rc<FunctionSymbol<TypedAST>>,
        function_value: FunctionValue<'ctx>,
    ) -> Option<FunctionValue<'ctx>> {
        self.functions.insert(symbol, function_value)
    }

    pub fn get_function(
        &self,
        symbol: &Rc<FunctionSymbol<TypedAST>>,
    ) -> Option<FunctionValue<'ctx>> {
        self.functions.get(symbol).copied()
    }
}
