use inkwell::{builder::Builder, context::ContextRef, module::Module};

use crate::{context::LLVMContext, symbols::VariableTable};

pub struct ModuleContext<'ctx> {
    pub(crate) inner: Module<'ctx>,
    pub(crate) variables: VariableTable<'ctx>,
}

impl<'ctx> ModuleContext<'ctx> {
    pub fn new(module_name: &str, context: ContextRef<'ctx>) -> Self {
        let module = context.create_module(module_name.as_ref());

        Self {
            inner: module,
            variables: VariableTable::new(),
        }
    }
}

impl<'ctx> From<Module<'ctx>> for ModuleContext<'ctx> {
    fn from(value: Module<'ctx>) -> Self {
        // FIXME: Can a module have non valid UTF-8??
        Self::new(
            value
                .get_name()
                .to_str()
                .expect("Module contained invalid UTF-8"),
            value.get_context(),
        )
    }
}
