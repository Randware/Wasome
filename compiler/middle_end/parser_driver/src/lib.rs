mod ast_builder;
mod directory_builder;
mod module_path;

use crate::ast_builder::ASTBuilder;
use ast::{AST, UntypedAST};
use io::FullIO;
use shared::program_information::ProgramInformation;
use source::SourceMap;

pub fn generate_untyped_ast<Loader: FullIO>(
    program_info: &ProgramInformation,
    load_from: &mut SourceMap<Loader>,
) -> Option<AST<UntypedAST>> {
    ASTBuilder::new(program_info, load_from).map(|ast_builder| ast_builder.build())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
