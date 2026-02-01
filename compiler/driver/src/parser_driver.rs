mod ast_builder;
mod directory_builder;
mod module_path;

use ast::{UntypedAST, AST};
use io::FullIO;
use crate::program_information::ProgramInformation;
use source::SourceMap;
use crate::parser_driver::ast_builder::ASTBuilder;

/// Generates an entire untyped ast by loading it from the provided [`SourceMap`]
///
/// # Parameters
///
/// - **program_info** - Information about the program to generate an AST for
/// - **load_from** - The [`SourceMap`] to load from
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
    ASTBuilder::new(program_info, load_from).map(|ast_builder| ast_builder.build())
}