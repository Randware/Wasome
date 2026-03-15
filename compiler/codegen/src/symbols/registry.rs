use std::{collections::HashMap, rc::Rc};

use ast::{TypedAST, symbol::StructSymbol};
use inkwell::{context::Context, types::StructType};

pub struct TypeRegistry<'ctx> {
    structs: HashMap<Rc<StructSymbol<TypedAST>>, StructType<'ctx>>,
    enums: HashMap<Rc<StructSymbol<TypedAST>>, StructType<'ctx>>,
}

impl<'ctx> TypeRegistry<'ctx> {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            enums: HashMap::new(),
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
        symbol: Rc<StructSymbol<TypedAST>>,
        struct_type: StructType<'ctx>,
    ) -> Option<StructType<'ctx>> {
        self.structs.insert(symbol, struct_type)
    }

    pub fn get_enum(&self, symbol: &Rc<StructSymbol<TypedAST>>) -> Option<StructType<'ctx>> {
        self.structs.get(symbol).copied()
    }
}
