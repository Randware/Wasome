use inkwell::{builder::Builder, context::Context, module::Module};

pub struct LLVMContext<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
}
