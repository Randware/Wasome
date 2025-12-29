mod expression_sa;
mod file_symbol_mapper;
mod function_symbol_mapper;
mod global_system_collector;
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

#[cfg(test)]
mod test_shared {
    use std::path::PathBuf;
    use ast::{ASTNode, ASTType};
    use ast::directory::Directory;
    use ast::file::File;
    use ast::top_level::Function;
    use super::*;

    pub(crate) fn functions_into_ast<Type: ASTType>(
        functions: Vec<ASTNode<Function<Type>>>,
    ) -> AST<Type> {
        let mut src_path = PathBuf::new();
        src_path.push("src");
        let mut main_path = src_path.clone();
        main_path.push("main.waso");
        AST::new(ASTNode::new(
            Directory::new(
                "src".to_string(),
                Vec::new(),
                vec![ASTNode::new(
                    File::new("main.waso".to_string(), Vec::new(), functions),
                    main_path,
                )],
            ),
            PathBuf::new(),
        ))
            .unwrap()
    }
}
