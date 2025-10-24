mod expression_sa;
mod mics_sa;
mod statement_sa;
mod top_level_sa;
mod symbol_mapper;

use ast::{AST, TypedAST, UntypedAST};

pub fn analyze(to_analyze: AST<UntypedAST>) -> Option<AST<TypedAST>> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
}
