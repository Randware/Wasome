use crate::expression_sa;
use crate::expression_sa::analyze_expression;
use crate::function_symbol_mapper::FunctionSymbolMapper;
use ast::data_type::Typed;
use ast::expression::Expression;
use ast::statement::{Return, Statement};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{TypedAST, UntypedAST};

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
        Statement::ControlStructure(_) => todo!(), //3
        Statement::Codeblock(_) => todo!(),        //4
        Statement::VoidFunctionCall(_) => todo!(),
    }
}

/** Analyzes a return statement and converts it to a typed return node
 @params  to_analyze: &Return<UntypedAST> - the untyped return statement to be analyzed
        symbol_mapper: &mut SymbolMapper - provides the current function return type and scope context used for validation
 @return Some(Return<TypedAST>) if the return statement (and its inner expression, if present) can be analyzed and the types match
        None if analysis fails, a type mismatch occurs, or an invalid return is encountered (e.g. value returned from void function or missing value for non-void)
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

#[cfg(test)]
mod tests {
    use super::*;
    use ast::UntypedAST;
    use ast::data_type::DataType;
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
