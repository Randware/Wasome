use crate::function_symbol_mapper::FunctionSymbolMapper;
use crate::mics_sa::analyze_data_type;
use ast::expression::{BinaryOp, Expression, Literal, Typecast, UnaryOp, UnaryOpType};
use ast::{ASTNode, TypedAST, UntypedAST};

/** Analyzes an untyped expression and converts it into a typed `Expression`.
@params
to_analyze: The expression to be analyzed (`Expression<UntypedAST>`).
symbol_mapper: Mutable reference to `SymbolMapper` used for resolving symbols and tracking type information during analysis.
@return
Some(`Expression<TypedAST>`) if the expression and all nested sub-expressions can be successfully analyzed and typed.
None if analysis or conversion fails for the expression or any of its sub-expressions.
*/
pub(crate) fn analyze_expression(
    to_analyze: &Expression<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Expression<TypedAST>> {
    Some(match to_analyze {
        Expression::FunctionCall(_) => todo!(),
        Expression::Variable(_) => todo!(),
        Expression::Literal(inner) => Expression::Literal(analyze_literal(&inner)?),
        Expression::UnaryOp(inner) => {
            Expression::UnaryOp(analyze_unary_op(inner, function_symbol_mapper)?)
        }
        Expression::BinaryOp(inner) => {
            Expression::BinaryOp(analyze_binary_op(inner, function_symbol_mapper)?)
        }
    })
}

/** Analyzes a literal string and converts it into a `Literal` type
 @params
 to_analyze: The literal string to be analyzed
 @return
Some(Literal) if the string can be successfully recognized and converted as a literal
None if the analysis or conversion fails
*/
fn analyze_literal(to_analyze: &str) -> Option<Literal> {
    if to_analyze == "true" {
        return Some(Literal::Bool(true));
    }
    if to_analyze == "false" {
        return Some(Literal::Bool(false));
    }

    if to_analyze.starts_with('\'') && to_analyze.ends_with('\'') {
        let inner = &to_analyze[1..to_analyze.len() - 1];
        if let Some(c) = inner.parse::<char>().ok() {
            return Some(Literal::Char(c as u32));
        }
    }

    if to_analyze.contains('.') {
        if let Ok(f64_val) = to_analyze.parse::<f64>() {
            return Some(Literal::F64(f64_val));
        }
    }

    if let Ok(s32_val) = to_analyze.parse::<i32>() {
        return Some(Literal::S32(s32_val));
    }

    None
}

/** Creates a new instance of UnaryOp
 @params
 to_analyze: The unary operation to be analyzed
 @return
Some(Box<UnaryOp<TypedAST>>) if the unary operation and its operand can be analyzed and converted to a typed form
None if analysis or conversion fails
*/
fn analyze_unary_op(
    to_analyze: &Box<UnaryOp<UntypedAST>>,
    symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Box<UnaryOp<TypedAST>>> {
    let (op_type, expression) = to_analyze.destructure();

    let converted_input = analyze_expression(&expression, symbol_mapper)?;

    let converted_unary_op_type = match op_type {
        UnaryOpType::Typecast(inner) => {
            let data_type = inner.target();
            let analyzed_data_type = analyze_data_type(data_type)?;
            let typed_typecast = Typecast::<TypedAST>::new(analyzed_data_type);
            UnaryOpType::Typecast(typed_typecast)
        }
        UnaryOpType::Negative => UnaryOpType::Negative,
        UnaryOpType::Not => UnaryOpType::Not,
    };

    let postion = expression.position().clone();

    let analyzed = UnaryOp::<TypedAST>::new(
        converted_unary_op_type,
        ASTNode::new(converted_input, postion),
    )?;
    Some(Box::new(analyzed))
}

/** Creates a new instance of BinaryOP
 @params
 to_analyze: The Binary operation to be analyzed
 @return
Some(Box<BinaryOp<TypedAST>>) if the Binary operation and its operand can be analyzed and converted to a typed form
None if analysis or conversion fails
*/
fn analyze_binary_op(
    to_analyze: &Box<BinaryOp<UntypedAST>>,
    symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Box<BinaryOp<TypedAST>>> {
    let (op_type, left_expr, right_expr) = to_analyze.destructure();

    let converted_left = analyze_expression(left_expr, symbol_mapper)?;
    let converted_right = analyze_expression(right_expr, symbol_mapper)?;

    let left_position = left_expr.position().clone();
    let right_position = right_expr.position().clone();

    let typed_left_node = ASTNode::new(converted_left, left_position);
    let typed_right_node = ASTNode::new(converted_right, right_position);

    let analyzed = BinaryOp::<TypedAST>::new(op_type, typed_left_node, typed_right_node)?;

    Some(Box::new(analyzed))
}

#[cfg(test)]
pub(crate) fn sample_codearea() -> shared::code_reference::CodeArea {
    use shared::code_file::CodeFile;
    use shared::code_reference::{CodeArea, CodeLocation};
    use std::path::PathBuf;

    CodeArea::new(
        CodeLocation::new(0, 0),
        CodeLocation::new(0, 10),
        CodeFile::new(PathBuf::from("test/test")),
    )
    .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_symbol_mapper::FileSymbolMapper;
    use ast::UntypedAST;
    use ast::expression::Expression;
    use ast::expression::Literal;

    /** Tests the `analyze_literal` helper function to ensure it correctly identifies and parses
        * various literal types (boolean, char, floating-point, and integer) from string input, and returns None for invalid input.
     */
    #[test]
    fn analyze_literal_recognizes_values() {
        assert_eq!(analyze_literal("true"), Some(Literal::Bool(true)));
        assert_eq!(analyze_literal("false"), Some(Literal::Bool(false)));
        assert_eq!(analyze_literal("'\n'"), Some(Literal::Char('\n' as u32)));
        assert_eq!(analyze_literal("3.14"), Some(Literal::F64(3.14)));
        assert_eq!(analyze_literal("42"), Some(Literal::S32(42)));
        assert_eq!(analyze_literal("nope"), None);
    }

    /** Tests the main `analyze_expression` function's ability to handle literal expressions, 
        * confirming that untyped string literals are correctly parsed and converted into their corresponding typed AST literals (S32 and F64).
     */
    #[test]
    fn analyze_expression_literal_converts_to_typed_literal() {
        let input: Expression<UntypedAST> = Expression::Literal(String::from("42"));

        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let output = analyze_expression(&input, &mut mapper).expect("should convert literal");

        let input2: Expression<UntypedAST> = Expression::Literal(String::from("12.2"));
        let output2 = analyze_expression(&input2, &mut mapper).expect("should convert literal (2)");

        assert_eq!(Expression::Literal(Literal::S32(42)), output);

        assert_eq!(Expression::Literal(Literal::F64(12.2)), output2);
    }

    /** Tests the semantic analysis of a Unary Operation (e.g., `-42`), ensuring that the inner expression 
        * is analyzed and typed correctly (S32) before the outer unary operation (Negative) is successfully created.
     */
    #[test]
    fn analyze_unary_negative_converts_op() {
        use ast::expression::{Expression, Literal, UnaryOp, UnaryOpType};
        let inner_expr = Expression::Literal(String::from("42"));

        let inner_node = ASTNode::new(inner_expr, sample_codearea());

        let untyped = UnaryOp::<UntypedAST>::new(UnaryOpType::Negative, inner_node);

        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let result =
            analyze_unary_op(&Box::new(untyped), &mut mapper).expect("should analyze unary op");

        let (op_type, expr) = result.destructure();

        assert_eq!(&UnaryOpType::Negative, op_type);
        assert_eq!(&Expression::Literal(Literal::S32(42)), &**expr);
    }

    /** Tests the semantic analysis of a Binary Operation (e.g., `17 + 5`), ensuring that both 
        * literal operands are analyzed and typed correctly (S32) before the binary addition operation is successfully created.
     */
    #[test]
    fn analyze_binary_add_converts_op() {
        use ast::expression::{BinaryOp, BinaryOpType, Expression, Literal};
        let left_node = ASTNode::new(Expression::Literal(String::from("17")), sample_codearea());
        let right_node = ASTNode::new(Expression::Literal(String::from("5")), sample_codearea());

        let untyped = BinaryOp::<UntypedAST>::new(BinaryOpType::Addition, left_node, right_node);
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let result =
            analyze_binary_op(&Box::new(untyped), &mut mapper).expect("should analyze binary op");

        let (op_type, l_expr, r_expr) = result.destructure();
        assert_eq!(BinaryOpType::Addition, op_type);

        assert_eq!(&Expression::Literal(Literal::S32(17)), &**l_expr);

        assert_eq!(&Expression::Literal(Literal::S32(5)), &**r_expr);
    }
}
