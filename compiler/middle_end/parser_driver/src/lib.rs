mod directory_builder;

use ast::{AST, UntypedAST};
use shared::program_information::ProgramInformation;

pub fn generate_untyped_ast(program_info: &ProgramInformation) -> AST<UntypedAST> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
