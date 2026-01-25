mod directory_sa;
mod expression_sa;
mod file_sa;
mod mics_sa;
mod statement_sa;
mod top_level_sa;
mod symbol_translation;

use ast::{TypedAST, UntypedAST, AST};
use ast::symbol::{Symbol, SymbolTable};

pub fn analyze(to_analyze: AST<UntypedAST>) -> Option<AST<TypedAST>> {
    todo!()
}

pub(crate) fn symbol_by_name<'a>(name: &str, mut from: impl SymbolTable<'a, UntypedAST>) -> Option<Symbol<'a, UntypedAST>> {
    let mut parts = name.split('.');

    let first = parts.next()?;
    let (prefix_name, name) =
        if let Some(second) = parts.next() {
            // If there are three parts, error
            if matches!(parts.next(), Some(_)) {
                return None;
            }
            (Some(first), second)
        }
        else {
            (None, first)
        };

    from.find(|symbol|
        symbol.0.as_ref().map(|mun| mun.name()) == prefix_name
        && symbol.1.name() == name
    ).map(|symbol| symbol.1)
}
#[cfg(test)]
mod tests {}

#[cfg(test)]
mod test_shared {
    use super::*;
    use ast::directory::Directory;
    use ast::file::File;
    use ast::top_level::Function;
    use ast::{ASTNode, ASTType};
    use std::path::PathBuf;

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
