mod directory_sa;
mod expression_sa;
mod file_sa;
mod mics_sa;
mod statement_sa;
mod symbol;
mod top_level_sa;

use std::ops::Deref;
use crate::directory_sa::analyze_directory;
use crate::symbol::global_system_collector::{collect_global_symbols, TraversalHelpers};
use ast::symbol::{DirectlyAvailableSymbol, SymbolTable};
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::{AST, TypedAST, UntypedAST, ASTType, SymbolIdentifier};

pub fn analyze(to_analyze: AST<UntypedAST>) -> Option<AST<TypedAST>> {
    let to_alloc_in = TraversalHelpers::new();
    let mut global_symbols = collect_global_symbols(&to_analyze, &to_alloc_in).ok()?;
    global_symbols.fill()?;
    
    analyze_directory(to_analyze.deref(), &mut global_symbols)
        .ok()
        .map(|root_dir| {
            // The typed AST has the same constraints as the untyped AST
            // Therefore, this can never error
            AST::new(root_dir).unwrap()
        })
}

pub(crate) fn symbol_by_name<'a>(
    name: &str,
    mut from: impl SymbolTable<'a, UntypedAST>,
) -> Option<DirectlyAvailableSymbol<'a, UntypedAST>> {
    let mut parts = name.split('.');

    let first = parts.next()?;
    // TODO: Support structs
    let (prefix_name, name) = if let Some(second) = parts.next() {
        // If there are three parts, error
        if parts.next().is_some() {
            return None;
        }
        (Some(first), second)
    } else {
        (None, first)
    };

    from.find(|symbol| {
        symbol.0.as_ref().map(|mun| mun.name()) == prefix_name && symbol.1.name() == name
    })
    .map(|symbol| symbol.1)
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
                    File::new("main.waso".to_string(), Vec::new(), functions, Vec::new(), Vec::new()),
                    main_path,
                )],
            ),
            PathBuf::new(),
        ))
        .unwrap()
    }
}
