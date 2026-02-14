mod directory_sa;
mod expression_sa;
mod file_sa;
mod mics_sa;
mod statement_sa;
mod symbol;
mod top_level_sa;
mod error_sa;

use crate::directory_sa::analyze_directory;
use crate::symbol::global_system_collector::{TraversalHelpers, collect_global_symbols};
use ast::symbol::{DirectlyAvailableSymbol, SymbolTable};
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::{AST, TypedAST, UntypedAST};
use std::ops::Deref;
use error::diagnostic::Diagnostic;


/// Analyzes the untyped AST and returns a fully typed AST or a Diagnostic error.
pub fn analyze(to_analyze: AST<UntypedAST>) -> Result<AST<TypedAST>, Diagnostic> {
    let to_alloc_in = TraversalHelpers::new();
    let root = DirectoryTraversalHelper::new_from_ast(&to_analyze);

    // Temporary error mapping until `collect_global_symbols` is refactored
    let mut global_symbols = collect_global_symbols(&root, &to_alloc_in)
        .map_err(|e| Diagnostic::builder().message(e).build())?;

    // Temporary error mapping until `fill` is refactored
    global_symbols.fill()
        .ok_or_else(|| Diagnostic::builder().message("Semantic analysis failed during fill phase.").build())?;

    analyze_directory(to_analyze.deref(), &mut global_symbols)
        .map(|root_dir| {
            // The typed AST has the same constraints as the untyped AST.
            // Therefore, this can never error.
            AST::new(root_dir).unwrap()
        })
        .map_err(|e| {
            // Convert the internal SemanticError to a user-facing Diagnostic
            // Currently `analyze_directory` still returns String, we will change this soon.
            Diagnostic::builder().message(e).build()
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
