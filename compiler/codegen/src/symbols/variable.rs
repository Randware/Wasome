use std::collections::HashMap;

use inkwell::values::PointerValue;

pub struct VariableInfo<'ctx> {
    pub pointer: PointerValue<'ctx>,
    pub type_name: String,
}

pub struct VariableTable<'ctx> {
    scopes: Vec<HashMap<u64, VariableInfo<'ctx>>>,
}

impl<'ctx> VariableTable<'ctx> {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<HashMap<u64, VariableInfo<'ctx>>> {
        self.scopes.pop()
    }

    pub fn insert(&mut self, id: u64, ptr: PointerValue<'ctx>, type_name: String) {
        if self.scopes.is_empty() {
            self.push_scope();
        }

        self.scopes
            .last_mut()
            .expect("Created scope not found")
            .insert(
                id,
                VariableInfo {
                    pointer: ptr,
                    type_name,
                },
            );
    }

    pub fn lookup(&self, id: u64) -> Option<&VariableInfo<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(&id) {
                return Some(info);
            }
        }
        None
    }
}
