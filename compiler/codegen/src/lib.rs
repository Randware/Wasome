mod context;
mod errors;
mod generators;
mod global_registry;
mod memory;
mod symbols;
mod temp;
mod types;
mod value;

use std::path::PathBuf;

use ast::{AST, TypedAST};
use bon::bon;
use inkwell::context::Context;

use crate::{context::LLVMContext, types::OptLevel};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    opt_level: OptLevel,
    output: PathBuf,
}

#[bon]
impl<'ctx> Codegen<'ctx> {
    #[builder]
    pub fn new(
        context: &'ctx Context,
        #[builder(default = OptLevel::O0)] opt_level: OptLevel,
        output: PathBuf,
    ) -> Self {
        Self {
            context,
            opt_level,
            output,
        }
    }
}
