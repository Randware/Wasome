mod directory_sa;
mod expression_sa;
mod file_sa;
mod mics_sa;
mod statement_sa;
mod symbol;
mod top_level_sa;

use crate::directory_sa::analyze_directory;
use crate::symbol::global_system_collector::{TraversalHelpers, collect_global_symbols};
use ast::symbol::{DirectlyAvailableSymbol, SymbolTable};
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::{AST, TypedAST, UntypedAST};
use std::ops::Deref;

pub fn analyze(to_analyze: AST<UntypedAST>) -> Option<AST<TypedAST>> {
    let to_alloc_in = TraversalHelpers::new();
    let root = DirectoryTraversalHelper::new_from_ast(&to_analyze);
    let mut global_symbols = collect_global_symbols(&root, &to_alloc_in).ok()?;
    global_symbols.fill()?;

    analyze_directory(to_analyze.deref(), &mut global_symbols)
        .ok()
        .map(|root_dir| {
            // The typed AST has the same constraints as the untyped AST
            // Therefore, this can never error
            AST::new(root_dir).unwrap()
        })
}

/// Resolves a symbol by its name within the given symbol table.
///
/// # Resolution Logic
/// *   The name is split by `.`.
/// *   Maximum supported depth is 2 (e.g., `module.symbol` or `symbol`).
/// *   If the name has more than 2 parts (e.g., `a.b.c`), resolution fails.
///
/// # Parameters
/// *   `name` - The name to resolve.
/// *   `from` - The symbol table to search in.
///
/// # Returns
/// *   `Some(DirectlyAvailableSymbol)` if found.
/// *   `None` if not found or if the name format is invalid.
pub(crate) fn symbol_by_name<'a>(
    name: &str,
    mut from: impl SymbolTable<'a, UntypedAST>,
) -> Option<DirectlyAvailableSymbol<'a, UntypedAST>> {
    let mut parts = name.split('.');

    let first = parts.next()?;
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
