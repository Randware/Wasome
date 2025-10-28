use crate::expression_sa::analyze_expression;
use crate::function_symbol_mapper::FunctionSymbolMapper;
use ast::block::CodeBlock;
use ast::data_type::{DataType, Typed};
use ast::statement::{Conditional, ControlStructure, Loop, LoopType, Return, Statement};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{TypedAST, UntypedAST};

/** Analyzes a statement referenced by a traversal helper and converts it into a typed statement node
@params  to_analyze: StatementTraversalHelper<UntypedAST> - traversal helper pointing to the statement to analyze
         function_symbol_mapper: &mut FunctionSymbolMapper - provides current function return type and scope context for validation
@return Some(Statement<TypedAST>) if the statement (and any nested expressions/statements) could be successfully analyzed
        None if analysis fails (type errors, invalid constructs, or other semantic errors)
*/
pub(crate) fn analyze_statement(
    to_analyze: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Statement<TypedAST>> {
    match to_analyze.get_inner() {
        Statement::VariableAssignment(_) => todo!(),
        Statement::VariableDeclaration(_) => todo!(),
        Statement::Expression(inner) => {
            let typed_expr = analyze_expression(inner, function_symbol_mapper)?;
            Some(Statement::Expression(typed_expr))
        }
        Statement::Return(inner) => {
            let typed_return = analyze_return(inner, function_symbol_mapper)?;
            Some(Statement::Return(typed_return))
        }
        Statement::ControlStructure(_) => {
            let typed_cs = Box::new(analyze_control_structure(
                to_analyze,
                function_symbol_mapper,
            )?);
            Some(Statement::ControlStructure(typed_cs))
        }
        Statement::Codeblock(_) => {
            let analyzed_cb = analyze_codeblock(to_analyze,function_symbol_mapper)?;
            Some(Statement::Codeblock(analyzed_cb))
        }
        Statement::VoidFunctionCall(_) => todo!(),
    }
}

/** Analyzes a Return statement and converts it into a typed Return node
@params  to_analyze: &Return<UntypedAST> - the untyped Return statement to analyze
          function_symbol_mapper: &mut FunctionSymbolMapper - provides the current function return type and scope context for validation
 @return Some(Return<TypedAST>) if the Return (and its inner expression, if present) can be analyzed and types match
         None if analysis fails, a type mismatch occurs, or an invalid return is found (e.g. value returned from void function or missing value for non-void function)
*/
fn analyze_return(
    to_analyze: &Return<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Return<TypedAST>> {
    let expected_type = function_symbol_mapper.get_current_function_return_type();

    let untyped_expr_option = to_analyze.to_return();

    match (expected_type, untyped_expr_option) {
        (None, Some(untyped_expr_ref)) => {
            /*eprintln!(
                "Error: Void function cannot return a value. Found value of type: {:?}",
                untyped_expr_ref
            );*/
            None
        }

        (Some(expected), None) => {
            /*eprintln!(
                "Error: Function expects a return value of type {:?} but got 'return;'.",
                expected
            );*/
            None
        }

        (None, None) => Some(Return::new(None)),

        (Some(expected), Some(untyped_expr_ref)) => {
            let untyped_expr = untyped_expr_ref.clone();
            let typed_expr = analyze_expression(untyped_expr, function_symbol_mapper)?;
            let actual_type = typed_expr.data_type();

            if actual_type != expected {
                /* eprintln!(
                    "Error: Return type mismatch. Expected {:?} but returned {:?}.",
                    expected, actual_type
                );*/
                return None;
            }

            Some(Return::new(Some(typed_expr)))
        }
    }
}

/** Analyzes a control-structure statement referenced by a traversal helper and returns a typed control structure
@params  to_analyze: StatementTraversalHelper<UntypedAST> - traversal helper pointing to the control-structure statement
         function_symbol_mapper: &mut FunctionSymbolMapper - provides scope context and supports nested statement analysis
@return Some(ControlStructure<TypedAST>) if the contained control structure (conditional or loop) was successfully analyzed
        None if the helper does not point to a control structure or if nested analysis fails
*/
fn analyze_control_structure(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<ControlStructure<TypedAST>> {
    let inner_control_structure = match to_analyze_helper.get_inner() {
        Statement::ControlStructure(cs) => cs,
        _ => return None, //"Expected a ControlStructure statement.",
    };

    match **inner_control_structure {
        ControlStructure::Conditional(_) => {
            analyze_conditional(to_analyze_helper, function_symbol_mapper)
                .map(ControlStructure::Conditional)
        }
        ControlStructure::Loop(_) => {
            analyze_loop(to_analyze_helper, function_symbol_mapper).map(ControlStructure::Loop)
        }
    }
}

/** Analyzes a Conditional control structure (if/then/else) and converts it into a typed Conditional node
  @params  to_analyze_helper: StatementTraversalHelper<UntypedAST> - traversal helper positioned at the Conditional statement
        function_symbol_mapper: &mut FunctionSymbolMapper - used to validate the condition type and manage scopes for then/else branches
@return Some(Conditional<TypedAST>) if the condition and both branches (when present) are semantically valid and typed
        None if the helper is not a Conditional, the condition is not boolean, or nested branch analysis fails
*/
fn analyze_conditional(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Conditional<TypedAST>> {
    let inner_box_ref = match to_analyze_helper.get_inner() {
        Statement::ControlStructure(b) => b,
        _ => return None,
    };

    let untyped_conditional_ref: &Conditional<UntypedAST> = match **inner_box_ref {
        ControlStructure::Conditional(ref c) => c,
        ControlStructure::Loop(_) => return None, //"Error: analyze_conditional expects Conditional, but found Loop."
    };
    let untyped_condition = untyped_conditional_ref.condition();
    let typed_condition = analyze_expression(untyped_condition, function_symbol_mapper)?;

    if typed_condition.data_type() != DataType::Bool {
        return None; //Error: Conditional expression must be of type Bool
    }
    function_symbol_mapper.enter_scope();
    let then_helper = to_analyze_helper.index(0);
    let typed_then_statement = analyze_statement(then_helper, function_symbol_mapper)?;
    function_symbol_mapper.exit_scope();

    let typed_else_statement = if untyped_conditional_ref.else_statement().is_some() {
        function_symbol_mapper.enter_scope();
        let else_helper = to_analyze_helper.index(1);
        let typed_block = analyze_statement(else_helper, function_symbol_mapper)?;
        function_symbol_mapper.exit_scope();
        Some(typed_block)
    } else {
        None
    };

    Some(Conditional::new(
        typed_condition,
        typed_then_statement,
        typed_else_statement,
    ))
}

/** Analyzes a Loop control structure and converts it into a typed Loop node
  @params  to_analyze_helper: StatementTraversalHelper<UntypedAST> - traversal helper positioned at the Loop statement
           mapper: &mut FunctionSymbolMapper - used to validate loop components and manage a new scope for loop body and headers
  @return Some(Loop<TypedAST>) if the loop header (while/for) and body are semantically valid and typed
*          None if the helper is not a Loop, a condition is not boolean, or nested analysis fails
 */
fn analyze_loop(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Loop<TypedAST>> {
    let inner_box_ref = match to_analyze_helper.get_inner() {
        Statement::ControlStructure(b) => b,
        _ => return None,
    };
    let untyped_loop_ref: &Loop<UntypedAST> = match **inner_box_ref {
        ControlStructure::Loop(ref l) => l,
        _ => return None, // "Error: analyze_loop expects Loop, but found Conditional."
    };

    function_symbol_mapper.enter_scope();

    let typed_loop_type = match untyped_loop_ref.loop_type().clone() {
        LoopType::Infinite => LoopType::Infinite,

        LoopType::While(condition) => {
            let typed_condition = analyze_expression(condition, function_symbol_mapper)?;

            if typed_condition.data_type() != DataType::Bool {
                function_symbol_mapper.exit_scope();
                return None; // "Error: While condition must be of type Bool"
            }
            LoopType::While(typed_condition)
        }

        LoopType::For {
            start,
            cond,
            after_each,
        } => {
            let start_helper = to_analyze_helper.index(0);
            let typed_start = analyze_statement(start_helper, function_symbol_mapper)?;

            let typed_cond = analyze_expression(cond, function_symbol_mapper)?;
            if typed_cond.data_type() != DataType::Bool {
                function_symbol_mapper.exit_scope();
                return None; //"Error: For loop condition must be of type Bool, but found"
            }
            let after_each_helper = to_analyze_helper.index(1);
            let typed_after_each = analyze_statement(after_each_helper, function_symbol_mapper)?;
            LoopType::For {
                start: typed_start,
                cond: typed_cond,
                after_each: typed_after_each,
            }
        }
    };

    let to_loop_on_index = untyped_loop_ref.loop_type().len();
    let to_loop_on_helper = to_analyze_helper.index(to_loop_on_index);
    let typed_to_loop_on = analyze_statement(to_loop_on_helper, function_symbol_mapper)?;

    function_symbol_mapper.exit_scope();

    Some(Loop::new(typed_to_loop_on, typed_loop_type))
}

/** Analyzes a code block referenced by a traversal helper and converts it into a typed CodeBlock node
@params  to_analyze_helper: StatementTraversalHelper<UntypedAST> - traversal helper positioned at the code block to analyze
         function_symbol_mapper: &mut FunctionSymbolMapper - provides scope management and context for nested statement analysis
@return Some(CodeBlock<TypedAST>) containing typed statements if all child statements were successfully analyzed
        None if any child statement fails semantic analysis or a nested error occurs
*/
fn analyze_codeblock(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<CodeBlock<TypedAST>> {
    function_symbol_mapper.enter_scope();

    let mut typed_statements = Vec::new();
    let statement_count = to_analyze_helper.child_len();

    for i in 0..statement_count {
        let child_helper = to_analyze_helper.index(i);

        if let Some(typed_statement) = analyze_statement(child_helper, function_symbol_mapper) {
            typed_statements.push(typed_statement);
        } else {
            function_symbol_mapper.exit_scope();
            return None;
        }
    }
    function_symbol_mapper.exit_scope();

    Some(CodeBlock::new(typed_statements))
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::UntypedAST;
    use ast::data_type::DataType;
    use ast::expression::Expression;
    use ast::statement::Return;

    #[test]
    fn analyze_return_ok_matching_void() {
        let mut mapper = FunctionSymbolMapper::new();
        let ret: Return<UntypedAST> = Return::new(None);
        let analyzed = analyze_return(&ret, &mut mapper);
        assert!(
            analyzed.is_some(),
            "Expected Some for void return without expression"
        );
        let analyzed_ret = analyzed.unwrap();
        assert!(
            analyzed_ret.to_return().is_none(),
            "Expected returned Return to contain no expression"
        );
    }

    #[test]
    fn analyze_return_ok_matching_types() {
        let expected_type = DataType::S32;
        let mut mapper = FunctionSymbolMapper::new();
        mapper.set_current_function_return_type(Some(expected_type));

        let untyped_literal = Expression::Literal(String::from("42"));
        let untyped_return = Return::new(Some(untyped_literal));

        let result = analyze_return(&untyped_return, &mut mapper);

        assert!(result.is_some(), "Should be successful and return");

        let typed_return = result.unwrap();

        let actual_returned_type = typed_return
            .return_type()
            .expect("Should contain a return type.");

        assert_eq!(
            expected_type, actual_returned_type,
            "The actual returned type of the statement should be S32."
        );

        assert!(
            matches!(typed_return.to_return().unwrap(), Expression::Literal(_)),
            "The returned expression should be a typed S32 literal."
        );
    }
}
