use crate::statement_sa::analyze_statement;
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use crate::symbol::{SyntaxContext, TypeParameterContext};
use ast::statement::{ControlStructure, Statement};
use ast::symbol::FunctionSymbol;
use ast::top_level::Function;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::visibility::Visible;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::ops::Deref;
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
    context: &mut SyntaxContext<impl TypeParameterContext, FunctionTraversalHelper<UntypedAST>>,
) -> Option<ASTNode<Function<TypedAST>>> {
    let to_analyze = &context.ast_reference;
    let untyped_symbol = to_analyze.inner().declaration();
    let typed_declaration: Rc<FunctionSymbol<TypedAST>> =
        context.global_elements.get_typed_function_symbol(
            untyped_symbol,
            context.type_parameter_context.untyped_type_parameters(),
        )?;

    let mut func_mapper = FunctionSymbolMapper::new();
    func_mapper.set_current_function_return_type(typed_declaration.return_type().cloned());

    for param_symbol in typed_declaration.params().iter() {
        func_mapper
            .add_variable(param_symbol.clone())
            .expect("Internal error: Function parameters should not conflict.");
    }

    let mut new_context =
        context.with_ast_reference(|to_analyze| StatementTraversalHelper::new_root(to_analyze));
    let typed_implementation_statement = analyze_statement(&mut new_context, &mut func_mapper)?;

    if typed_declaration.return_type().is_some() && !always_return(&typed_implementation_statement) {
        // We have to return a value but don't
        return None;
    }
    let to_analyze = &context.ast_reference;
    let code_area = to_analyze.inner().implementation().position().clone();
    let implementation_node = ASTNode::new(typed_implementation_statement, code_area);

    Some(ASTNode::new(Function::new(
        typed_declaration,
        implementation_node,
        to_analyze.inner().visibility()
    ), to_analyze.inner().position().clone()))
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
    use super::*;
    use crate::expression_sa::sample_codearea;
    use crate::test_shared::functions_into_ast;
    use ast::statement::{CodeBlock, Statement};
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
