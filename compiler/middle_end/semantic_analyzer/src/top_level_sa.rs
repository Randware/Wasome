use crate::statement_sa::analyze_statement;
use crate::symbol::SyntaxContext;
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use ast::composite::{Enum, EnumVariant};
use ast::statement::{ControlStructure, Statement};
use ast::symbol::{EnumSymbol, EnumVariantSymbol, FunctionSymbol};
use ast::top_level::Function;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::visibility::Visible;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::ops::Deref;
use std::rc::Rc;

/// Analyzes the implementation (body) of an untyped function.
///
/// Initializes the function's local scope, registers parameters, and validates all contained statements and expressions.
/// Requires the function's signature to be present in the global symbol map prior to execution.
///
/// # Panics
/// *   Panics if function parameters conflict (assumes upstream uniqueness guarantees).
///
/// # Control Flow Analysis
/// Checks if the function always returns a value (if a return type is declared) using `always_return`.
///
/// # Parameters
/// * `symbol` - The typed function symbol containing the return type and parameters.
/// * `context` - The syntax context containing the function traversal helper for the body.
///
/// # Returns
/// * `Some(Function<TypedAST>)` if the body is semantically correct.
/// * `None` if analysis fails (e.g., type or scope errors, or symbol missing in global map).
pub(crate) fn analyze_function(
    symbol: Rc<FunctionSymbol<TypedAST>>,
    context: &SyntaxContext<&FunctionTraversalHelper<UntypedAST>>,
) -> Option<ASTNode<Function<TypedAST>>> {
    let mut func_mapper = FunctionSymbolMapper::new();
    func_mapper.set_current_function_return_type(symbol.return_type().cloned());

    for param_symbol in symbol.params().iter() {
        func_mapper.add_variable(param_symbol.clone()).ok()?;
    }

    let sth = StatementTraversalHelper::new_root(context.ast_reference);
    let new_context = context.with_ast_reference(&sth);
    let typed_implementation_statement = analyze_statement(&new_context, &mut func_mapper)?;

    if symbol.return_type().is_some() && !always_return(&typed_implementation_statement) {
        // We have to return a value but don't
        return None;
    }
    let to_analyze = &context.ast_reference;
    let code_area = to_analyze.inner().implementation().position().clone();
    let implementation_node = ASTNode::new(typed_implementation_statement, code_area);

    Some(ASTNode::new(
        Function::new(symbol, implementation_node, to_analyze.inner().visibility()),
        to_analyze.inner().position().clone(),
    ))
}

pub(crate) fn analyze_enum(
    symbol: Rc<EnumSymbol<TypedAST>>,
    variants: Vec<Rc<EnumVariantSymbol<TypedAST>>>,
    context: &SyntaxContext<&EnumTraversalHelper<UntypedAST>>,
) -> ASTNode<Enum<TypedAST>> {
    let untyped_enum = context.ast_reference.inner();
    ASTNode::new(
        Enum::new(
            symbol,
            variants
                .into_iter()
                .zip(untyped_enum.variants().iter())
                .map(|(typed, untyped)| {
                    ASTNode::new(EnumVariant::new(typed), untyped.position().clone())
                })
                .collect(),
            untyped_enum.visibility(),
        ),
        untyped_enum.position().clone(),
    )
}

/// Checks wherever a statement will always encounter a return statement before finishing execution
///
/// Note that due to the
/// [halting problem](https://www.geeksforgeeks.org/theory-of-computation/halting-problem-in-theory-of-computation/),
/// a perfect solution is impossible
///
/// Instead, an approximation is used that accepts false-negatives, but not false-positives. This
/// means that in cases where a return will always be encountered, false might be returned but never the
/// other way around
///
/// The exact cases where a false-negative happens may change over time, but cases may only be
/// resolved and not added.
///
/// # Limitations
/// *   **Loops**: Infinite loops (`loop { ... }` or `while (true) { ... }`) are **not** considered to "return", even though they diverge.
/// *   **Code Blocks**: Only the **last** statement in a code block is checked. If an early return exists but is not the last statement (e.g., followed by unreachable code), it might not be detected.
fn always_return(to_check: &Statement<TypedAST>) -> bool {
    match to_check {
        Statement::Return(_) => true,
        Statement::Codeblock(codeblock) => codeblock
            .last()
            .map(|statement| always_return(statement.deref()))
            .unwrap_or(false),
        Statement::ControlStructure(crtl) => match crtl.deref() {
            ControlStructure::Conditional(cond) => {
                always_return(cond.then_statement())
                    && cond
                        .else_statement()
                        .map(|else_statement| always_return(else_statement))
                        .unwrap_or(false)
            }
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod tests {
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
