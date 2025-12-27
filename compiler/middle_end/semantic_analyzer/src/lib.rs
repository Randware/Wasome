mod expression_sa;
mod file_symbol_mapper;
mod function_symbol_mapper;
mod mics_sa;
mod statement_sa;
mod top_level_sa;

use ast::{AST, TypedAST, UntypedAST};

pub fn analyze(to_analyze: AST<UntypedAST>) -> Option<AST<TypedAST>> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
}
