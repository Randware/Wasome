mod context;
mod errors;
mod generators;
mod memory;
mod symbols;
mod temp;
mod types;
use std::path::PathBuf;

use ast::{AST, TypedAST};
use bon::bon;
use inkwell::context::Context;

use crate::{context::LLVMContext, types::OptLevel};

pub struct Codegen<'ctx> {
    ast: AST<TypedAST>,
    context: &'ctx Context,
    opt_level: OptLevel,
    output: PathBuf,
}

#[bon]
impl<'ctx> Codegen<'ctx> {
    #[builder]
    pub fn new(
        context: &'ctx Context,
        ast: AST<TypedAST>,
        #[builder(default = OptLevel::O0)] opt_level: OptLevel,
        output: PathBuf,
    ) -> Self {
        Self {
            ast,
            context,
            opt_level,
            output,
        }
    }
}
