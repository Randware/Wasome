//! This part of the driver is responsible for loading programs from a [`SourceMap`] and
//! using the parser to turn it into an untyped AST.
//! 
//! The parsing and loading can't be separated as the imports of a program, which will only be known
//! after parsing, decide what to load next.
mod ast_builder;
mod directory_builder;
mod module_path;

use crate::parser_driver::ast_builder::ASTBuilder;
use crate::program_information::ProgramInformation;
use ast::{AST, UntypedAST};
use io::FullIO;
use source::SourceMap;

/// Generates an entire untyped ast by loading it from the provided [`SourceMap`]
///
/// # Parameters
///
/// - **`program_info`** - Information about the program to generate an AST for
/// - **`load_from`** - The [`SourceMap`] to load from
///
/// # Return
///
/// The AST
///
/// # Errors
///
/// There are many error conditions, mainly:
/// - Syntax errors
/// - Unresolved imports
/// - File system errors
///     - Including paths from `program_info` being unresolved
pub fn generate_untyped_ast<Loader: FullIO>(
    program_info: &ProgramInformation,
    load_from: &mut SourceMap<Loader>,
) -> Option<AST<UntypedAST>> {
    ASTBuilder::new(program_info, load_from).map(ASTBuilder::build)
}
