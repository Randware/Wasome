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
use ast::{UntypedAST, AST};
use error::diagnostic::Diagnostic;
use io::FullIO;
use source::SourceMap;

pub fn generate_untyped_ast<Loader: FullIO>(
    program_info: &ProgramInformation,
    load_from: &mut SourceMap<Loader>,
) -> Result<AST<UntypedAST>, Diagnostic> {
    ASTBuilder::new(program_info, load_from)
        .map(ASTBuilder::build)
        .map_err(Into::into)
}
