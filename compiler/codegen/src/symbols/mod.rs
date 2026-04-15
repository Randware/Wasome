mod registry;
mod variable;

use ast::TypedAST;
use ast::symbol::{EnumSymbol, EnumVariantSymbol, StructFieldSymbol, StructSymbol};
use inkwell::types::StructType;
pub use registry::SymbolRegistry;
use std::collections::HashMap;
use std::rc::Rc;
pub use variable::*;

#[derive(Eq, PartialEq)]
pub struct EnumInformation<'ctx> {
    variants: Vec<(Rc<EnumVariantSymbol<TypedAST>>, StructType<'ctx>)>,
}

impl<'ctx> EnumInformation<'ctx> {
    pub fn new() -> Self {
        Self {
            variants: Vec::new(),
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
}

#[derive(PartialEq, Eq)]
pub struct StructInformation<'ctx> {
    variants: Vec<Rc<StructFieldSymbol<TypedAST>>>,
    lowered: StructType<'ctx>,
}

impl<'ctx> StructInformation<'ctx> {
    pub fn new(lowered: StructType<'ctx>) -> Self {
        Self {
            variants: Vec::new(),
            lowered,
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
}
