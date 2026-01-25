use crate::symbol_translation::file_symbol_mapper::FileSymbolMapper;
use crate::symbol_translation::function_symbol_mapper::FunctionSymbolMapper;
use crate::symbol_translation::global_system_collector::GlobalSymbolMap;
use crate::statement_sa::analyze_statement;
use ast::symbol::FunctionSymbol;
use ast::top_level::Function;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::visibility::Visible;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::rc::Rc;

// I think that this is now obsolete
/*
/// Analyzes a top-level element (e.g., a Function) and converts it into its typed representation.
///
/// This process ensures semantic correctness for the element's implementation body.
///
/// # Parameters
/// * `to_analyze` - The untyped element to be analyzed (`TopLevelElement<UntypedAST>`).
/// * `file_mapper` - Provides global context for symbol lookup (e.g., function declarations) (`&mut FileSymbolMapper`).
/// * `root_helper` - The traversal context for the function's implementation (`&FunctionTraversalHelper<UntypedAST>`).
///
/// # Returns
/// * `Some(TopLevelElement<TypedAST>)` if the element (including its body) could be successfully analyzed.
/// * `None` if analysis fails (type errors, invalid constructs, or other semantic errors).
pub(crate) fn analyze_top_level(
    to_analyze: TopLevelElement<UntypedAST>,
    file_mapper: &mut FileSymbolMapper,
    root_helper: &FunctionTraversalHelper<UntypedAST>,
) -> Option<TopLevelElement<TypedAST>> {
    match to_analyze {
        TopLevelElement::Function(untyped_function) => {
            analyze_function(&untyped_function, root_helper, file_mapper)
                .map(TopLevelElement::Function)
        }
    }
}*/

/// Analyzes the implementation (body) of an untyped function.
///
/// Initializes the function's local scope, registers parameters, and validates all contained statements and expressions.
/// Requires the function's signature to be present in the global symbol map prior to execution.
///
/// # Parameters
/// * `untyped_function` - Function element with the body to analyze (`&Function<UntypedAST>`).
/// * `root_helper` - Traversal context for the function's body (`&FunctionTraversalHelper<UntypedAST>`).
/// * `file_mapper` - Needed to initialize the `FunctionSymbolMapper` (local scope manager).
/// * `global_map` - Global symbol table for resolving function signatures and analyzing statements.
///
/// # Returns
/// * `Some(Function<TypedAST>)` if the body is semantically correct.
/// * `None` if analysis fails (e.g., type or scope errors, or symbol missing in global map).
pub(crate) fn analyze_function(
    untyped_function: &Function<UntypedAST>,
    root_helper: &FunctionTraversalHelper<UntypedAST>,
    file_mapper: &mut FileSymbolMapper,
) -> Option<Function<TypedAST>> {
    let untyped_symbol = untyped_function.declaration();
    let typed_declaration: Rc<FunctionSymbol<TypedAST>> = file_mapper.lookup_function_rc(untyped_symbol)?.clone();

    let mut func_mapper = FunctionSymbolMapper::new(file_mapper);
    func_mapper.set_current_function_return_type(typed_declaration.return_type().cloned());

    for param_symbol in typed_declaration.params().iter() {
        func_mapper
            .add_variable(param_symbol.clone())
            .expect("Internal error: Function parameters should not conflict.");
    }

    let impl_helper = StatementTraversalHelper::new_root(root_helper);
    let typed_implementation_statement =
        analyze_statement(impl_helper, &mut func_mapper)?;

    let code_area = untyped_function.implementation().position().clone();
    let implementation_node = ASTNode::new(typed_implementation_statement, code_area);

    Some(Function::new(
        typed_declaration,
        implementation_node,
        untyped_function.visibility(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression_sa::sample_codearea;
    use crate::symbol_translation::global_system_collector::GlobalSymbolMap;
    use crate::test_shared::functions_into_ast;
    use ast::statement::{CodeBlock, Statement};
    use ast::symbol::{FunctionSymbol, Symbol};
    use ast::traversal::directory_traversal::DirectoryTraversalHelper;
    use ast::visibility::Visibility;
    use ast::{ASTNode, TypedAST, UntypedAST};
    use std::rc::Rc;

    /*/// Tests the successful analysis of a simple void function with no parameters.
    /// It verifies that the function is correctly resolved from the global map and its body is processed.
    #[test]
    fn analyze_function_ok() {
        let func_name = "test".to_string();
        let func_symbol_untyped_raw = FunctionSymbol::new(func_name.clone(), None, Vec::new());
        let func_symbol_untyped_rc = Rc::new(func_symbol_untyped_raw.clone());

        let body_block = CodeBlock::new(Vec::new());
        let body_node = ASTNode::new(Statement::Codeblock(body_block), sample_codearea());

        let func = Function::new(func_symbol_untyped_rc, body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let mut global_map = GlobalSymbolMap::new();
        let func_symbol_typed =
            Rc::new(FunctionSymbol::<TypedAST>::new(func_name, None, Vec::new()));
        global_map.insert(func_symbol_untyped_raw, func_symbol_typed.clone());

        let context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let mut file_mapper = FileSymbolMapper::new(&old_global_map, &context);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);

        let analyzed_func =
            analyze_function(func_ref.inner(), &func_ref, &mut file_mapper, &global_map);

        assert!(analyzed_func.is_some(), "Function analysis should succeed");

        let typed_func = analyzed_func.unwrap();
        assert_eq!(typed_func.declaration().name(), "test");
        assert!(typed_func.declaration().return_type().is_none());
    }*/
}
