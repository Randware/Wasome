#![warn(clippy::pedantic, clippy::nursery)]
mod context;
mod generators;
mod global_registry;
mod memory;
mod symbols;
mod types;

pub use types::OptLevel;
use ast::TypedAST;
use ast::symbol::FunctionSymbol;
use bon::bon;
use inkwell::context::Context;
use std::rc::Rc;

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    opt_level: OptLevel,
    main_function: Rc<FunctionSymbol<TypedAST>>,
}

#[bon]
impl<'ctx> Codegen<'ctx> {
    #[builder]
    pub fn new(
        context: &'ctx Context,
        #[builder(default = OptLevel::O0)] opt_level: OptLevel,
        /// Must return void and take no arguments
        /// Must exist in the AST
        /// May not be external
        main_function: Rc<FunctionSymbol<TypedAST>>,
    ) -> Result<Self, CodegenCreationError> {
        if main_function.return_type().is_some() {
            return Err(CodegenCreationError::MainFunctionNonVoidReturn);
        }
        if !main_function.params().is_empty() {
            return Err(CodegenCreationError::MainFunctionTakesArguments);
        }
        Ok(Self {
            context,
            opt_level,
            main_function,
        })
    }
}

#[derive(Debug)]
pub enum CodegenCreationError {
    MainFunctionNonVoidReturn,
    MainFunctionTakesArguments,
}
