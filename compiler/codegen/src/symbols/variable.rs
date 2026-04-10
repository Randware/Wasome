use ast::TypedAST;
use ast::symbol::VariableSymbol;
use inkwell::values::PointerValue;
use std::collections::HashMap;
use std::rc::Rc;

pub struct VariableInfo<'ctx> {
    pub pointer: PointerValue<'ctx>,
}

pub struct VariableTable<'ctx> {
    vars: HashMap<Rc<VariableSymbol<TypedAST>>, VariableInfo<'ctx>>,
}

impl<'ctx> VariableTable<'ctx> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, var: Rc<VariableSymbol<TypedAST>>, ptr: PointerValue<'ctx>) {
        self.vars.insert(var, VariableInfo { pointer: ptr });
    }

    pub fn lookup(&self, id: &VariableSymbol<TypedAST>) -> Option<&VariableInfo<'ctx>> {
        self.vars.get(id)
    }
}
