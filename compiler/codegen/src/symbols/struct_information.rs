use ast::TypedAST;
use ast::symbol::StructFieldSymbol;
use inkwell::types::StructType;
use inkwell::values::FunctionValue;
use std::rc::Rc;

#[derive(PartialEq, Eq)]
pub struct StructInformation<'ctx> {
    variants: Vec<Rc<StructFieldSymbol<TypedAST>>>,
    lowered: StructType<'ctx>,
    on_drop: FunctionValue<'ctx>,
    predrop: Option<FunctionValue<'ctx>>,
}

impl<'ctx> StructInformation<'ctx> {
    pub const fn new(lowered: StructType<'ctx>, on_drop: FunctionValue<'ctx>) -> Self {
        Self {
            variants: Vec::new(),
            lowered,
            on_drop,
            predrop: None,
        }
    }
    pub const fn lowered(&self) -> StructType<'ctx> {
        self.lowered
    }

    pub const fn fields(&self) -> &Vec<Rc<StructFieldSymbol<TypedAST>>> {
        &self.variants
    }
    pub fn add_field(&mut self, field: Rc<StructFieldSymbol<TypedAST>>) {
        self.variants.push(field);
    }

    pub const fn on_drop(&self) -> FunctionValue<'ctx> {
        self.on_drop
    }

    pub const fn predrop(&self) -> Option<FunctionValue<'ctx>> {
        self.predrop
    }

    pub const fn set_predrop(&mut self, predrop: FunctionValue<'ctx>) {
        self.predrop = Some(predrop);
    }
}
