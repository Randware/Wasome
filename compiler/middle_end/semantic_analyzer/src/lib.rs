mod expression_sa;
mod statement_sa;
mod top_level_sa;
mod mics_sa;

use ast::{TypedAST, UntypedAST, AST};

pub fn analyze(to_analyze: AST<UntypedAST>) -> Option<AST<TypedAST>>
{
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

}
