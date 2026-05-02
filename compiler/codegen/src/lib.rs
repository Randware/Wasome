#![warn(clippy::pedantic, clippy::nursery)]
mod context;
mod generators;
mod global_registry;
mod memory;
mod symbols;
mod types;

use ast::{TypedAST, AST};
use ast::symbol::FunctionSymbol;
use bon::bon;
use inkwell::context::Context;
use std::rc::Rc;
pub use types::OptLevel;

pub fn codegen(opt_level: OptLevel, main_function: Rc<FunctionSymbol<TypedAST>>, to_generate: AST<TypedAST>) -> Result<Vec<u8>, CodegenCreationError> {
    let context = Context::create();
    let mut codegen = Codegen::builder().opt_level(opt_level).main_function(main_function).context(&context).build()?;
    Ok(codegen.compile(&to_generate))
}
/// Top-level code generator that orchestrates compilation of a typed AST to WebAssembly object code.
///
/// The [`Codegen`] struct is the entry point for the compilation pipeline. It holds the
/// LLVM [`Context`] reference, the target optimization level, and the main function symbol
/// that serves as the WASM entry point (`_start`).
///
/// The compilation process is triggered via [`Codegen::compile`], which creates an internal
/// LLVM context, registers all types and functions from the AST, generates code for each
/// function body, applies optimization passes, and returns the compiled WASM object bytes.
///
/// # Validation
///
/// The `Codegen::new` constructor validates the main function:
/// * It must have a void return type
/// * It must take no parameters
/// * It must not be an external function
pub struct Codegen<'ctx> {
    /// The underlying LLVM context used for all LLVM IR operations.
    context: &'ctx Context,
    /// The optimization level to apply during the LLVM pass pipeline.
    opt_level: OptLevel,
    /// The main function symbol, which becomes the `_start` entry point in the WASM module.
    main_function: Rc<FunctionSymbol<TypedAST>>,
}

#[bon]
impl<'ctx> Codegen<'ctx> {
    /// Creates a new `Codegen` instance for the given LLVM context and main function.
    ///
    /// Validates that the main function meets the requirements for a WASM entry point:
    /// void return type, no parameters, and not an external function.
    ///     - It not being an external function is unchecked
    ///
    /// # Arguments
    ///
    /// * `context` - The LLVM [`Context`] to use for all IR operations
    /// * `opt_level` - The optimization level for the LLVM pass pipeline. Defaults to [`OptLevel::O0`]
    /// * `main_function` - The main function symbol. Must return void, take no arguments, and not be external
    ///
    /// # Returns
    ///
    /// * `Ok(Codegen)` - A new code generator instance ready for compilation
    /// * `Err(CodegenCreationError::MainFunctionNonVoidReturn)` - The main function has a non-void return type
    /// * `Err(CodegenCreationError::MainFunctionTakesArguments)` - The main function takes parameters
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

/// Errors that can occur when creating a [`Codegen`] instance.
///
/// These errors represent validation failures for the main function that serves as the
/// WASM entry point. They are checked at construction time before any code generation begins.
#[derive(Debug)]
pub enum CodegenCreationError {
    /// The main function has a non-void return type.
    ///
    /// WASM entry points must not return a value. The main function's return type
    /// must be void (i.e., the function declaration has no return type specified).
    MainFunctionNonVoidReturn,
    /// The main function takes arguments.
    ///
    /// WASM entry points must not accept parameters. The main function must have
    /// an empty parameter list.
    MainFunctionTakesArguments,
}
