mod registry;
mod variable;

use ast::symbol::{EnumVariantSymbol, StructFieldSymbol};
use ast::TypedAST;
use inkwell::types::StructType;
use inkwell::values::FunctionValue;
pub use registry::SymbolRegistry;
use std::rc::Rc;
pub use variable::*;

#[derive(Eq, PartialEq)]
pub struct EnumInformation<'ctx> {
    variants: Vec<(Rc<EnumVariantSymbol<TypedAST>>, StructType<'ctx>)>,
    on_drop: FunctionValue<'ctx>
}

impl<'ctx> EnumInformation<'ctx> {
    pub fn new(on_drop: FunctionValue<'ctx>) -> Self {
        Self {
            variants: Vec::new(),
            on_drop
        }
    }

    pub fn insert(&mut self, key: Rc<EnumVariantSymbol<TypedAST>>, val: StructType<'ctx>) {
        self.variants.push((key, val))
    }

    pub fn lookup(&self, key: &EnumVariantSymbol<TypedAST>) -> Option<StructType<'ctx>> {
        self.variants
            .iter()
            .find(|to_check| &*to_check.0 == key)
            .map(|variant| &variant.1)
            .copied()
    }

    pub fn index_of(&self, key: &EnumVariantSymbol<TypedAST>) -> Option<usize> {
        self.variants
            .iter()
            .enumerate()
            .find(|(_, to_check)| &*to_check.0 == key)
            .map(|(i, _)| i)
    }

    pub fn on_drop(&self) -> FunctionValue<'ctx> {
        self.on_drop
    }

    pub fn variants(&self) -> &Vec<(Rc<EnumVariantSymbol<TypedAST>>, StructType<'ctx>)> {
        &self.variants
    }
}

#[derive(PartialEq, Eq)]
pub struct StructInformation<'ctx> {
    variants: Vec<Rc<StructFieldSymbol<TypedAST>>>,
    lowered: StructType<'ctx>,
    on_drop: FunctionValue<'ctx>,
    predrop: Option<FunctionValue<'ctx>>
}

impl<'ctx> StructInformation<'ctx> {
    pub fn new(lowered: StructType<'ctx>, on_drop: FunctionValue<'ctx>) -> Self {
        Self {
            variants: Vec::new(),
            lowered,
            on_drop,
            predrop: None
        }
    }
    pub fn lowered(&self) -> StructType<'ctx> {
        self.lowered
    }

    pub fn fields(&self) -> &Vec<Rc<StructFieldSymbol<TypedAST>>> {
        &self.variants
    }
    pub fn add_field(&mut self, field: Rc<StructFieldSymbol<TypedAST>>) {
        self.variants.push(field)
    }

    pub fn on_drop(&self) -> FunctionValue<'ctx> {
        self.on_drop
    }

    pub fn predrop(&self) -> Option<FunctionValue<'ctx>> {
        self.predrop
    }

    pub fn set_predrop(&mut self, predrop: FunctionValue<'ctx>) {
        self.predrop = Some(predrop);
    }
}
