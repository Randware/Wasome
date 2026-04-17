use ast::TypedAST;
use ast::symbol::EnumVariantSymbol;
use inkwell::types::StructType;
use inkwell::values::FunctionValue;
use std::rc::Rc;

#[derive(Eq, PartialEq)]
pub struct EnumInformation<'ctx> {
    variants: Vec<(Rc<EnumVariantSymbol<TypedAST>>, StructType<'ctx>)>,
    on_drop: FunctionValue<'ctx>,
}

impl<'ctx> EnumInformation<'ctx> {
    pub fn new(on_drop: FunctionValue<'ctx>) -> Self {
        Self {
            variants: Vec::new(),
            on_drop,
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
