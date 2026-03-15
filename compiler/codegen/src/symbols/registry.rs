use std::{collections::HashMap, rc::Rc};

use ast::{TypedAST, symbol::StructSymbol};
use inkwell::{context::Context, types::StructType};

pub struct TypeRegistry<'ctx> {
    registry: HashMap<Rc<StructSymbol<TypedAST>>, StructType<'ctx>>,
}

impl<'ctx> TypeRegistry<'ctx> {
    pub fn new() -> Self {
        Self {
            registry: HashMap::new(),
        }
    }

    pub fn register_struct(
        &mut self,
        symbol: Rc<StructSymbol<TypedAST>>,
        struct_type: StructType<'ctx>,
    ) -> Option<StructType<'ctx>> {
        self.registry.insert(symbol, struct_type)
    }

    pub fn get_struct(&self, symbol: &Rc<StructSymbol<TypedAST>>) -> Option<StructType<'ctx>> {
        self.registry.get(symbol).copied()
    }
}
