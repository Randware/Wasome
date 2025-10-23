use std::task::Context;
use crate::mics_sa::analyze_data_type;
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, Literal, Typecast, UnaryOp, UnaryOpType,
};
use ast::{ASTType, TypedAST, UntypedAST};
use ast::symbol::FunctionCall;

pub(crate) fn analyze_expression(
    to_analyze: Expression<UntypedAST>,
) -> Option<Expression<TypedAST>> {
    Some(match to_analyze {
        Expression::FunctionCall(_) => todo!(),
        Expression::Variable(_) => todo!(),
        Expression::Literal(inner) => Expression::Literal(analyze_literal(&inner)?),
        Expression::UnaryOp(inner) => Expression::UnaryOp(analyze_unary_op(inner)?),
        Expression::BinaryOp(inner) => Expression::BinaryOp(analyze_binary_op(inner)?),
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
fn analyze_unary_op(to_analyze: Box<UnaryOp<UntypedAST>>) -> Option<Box<UnaryOp<TypedAST>>> {
    let (op_type, expression) = to_analyze.destructure();
    let converted_input = analyze_expression(expression)?;
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

    let analyzed = UnaryOp::<TypedAST>::new(converted_unary_op_type, converted_input)?;
    Some(Box::new(analyzed))
}

/** Creates a new instance of BinaryOP
 @params
 to_analyze: The Binary operation to be analyzed
 @return
Some(Box<BinaryOp<TypedAST>>) if the Binary operation and its operand can be analyzed and converted to a typed form
None if analysis or conversion fails
*/
fn analyze_binary_op(to_analyze: Box<BinaryOp<UntypedAST>>) -> Option<Box<BinaryOp<TypedAST>>> {
    let (op_type, left_expr, right_expr) = to_analyze.destructure();
    let converted_left = analyze_expression(left_expr)?;
    let converted_right = analyze_expression(right_expr)?;

    let analyzed =
        BinaryOp::<TypedAST>::new(op_type, converted_left, converted_right)?;

    Some(Box::new(analyzed))
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::expression::Literal;
    use ast::expression::Expression;
    use ast::{UntypedAST, AST};

    #[test]
    fn analyze_literal_recognizes_values() {
        assert_eq!(analyze_literal("true"), Some(Literal::Bool(true)));
        assert_eq!(analyze_literal("false"), Some(Literal::Bool(false)));
        assert_eq!(analyze_literal("'\\n'"), Some(Literal::Char('\n' as u32)));
        assert_eq!(analyze_literal("3.14"), Some(Literal::F64(3.14)));
        assert_eq!(analyze_literal("42"), Some(Literal::S32(42)));
        assert_eq!(analyze_literal("nope"), None);
    }

    #[test]
    fn analyze_expression_literal_converts_to_typed_literal() {
        let input: Expression<UntypedAST> = Expression::Literal(String::from("42"));
        let output = analyze_expression(input).expect("should convert literal");

        let input2: Expression<UntypedAST> = Expression::Literal(String::from("12.2"));
        let output2 = analyze_expression(input2).expect("should convert literal (2)");

        match output {
            Expression::Literal(Literal::S32(42)) => (),
            other => panic!("unexpected result: {:?}", other),
        }

        match output2 {
            Expression::Literal(Literal::F64(12.2)) => (),
            other => panic!("unexpected result: {:?}", other),
        }

    }

    #[test]
    fn analyze_unary_negative_converts_op() {
        use ast::expression::{UnaryOp, UnaryOpType, Expression, Literal};
        let inner = Expression::Literal(String::from("42"));
        let untyped = UnaryOp::<UntypedAST>::new(UnaryOpType::Negative, inner);

        let result = analyze_unary_op(Box::new(untyped)).expect("should analyze unary op");

        let (op_type, expr) = result.destructure();
        match op_type {
            UnaryOpType::Negative => (),
            other => panic!("unexpected unary op: {:?}", other),
        }
        match expr {
            Expression::Literal(Literal::S32(42)) => (),
            other => panic!("unexpected unary operand: {:?}", other),
        }
    }

    #[test]
    fn analyze_binary_add_converts_op(){
        use ast::expression::{BinaryOp,BinaryOpType,Expression,Literal};
        let left = Expression::Literal(String::from("17"));
        let right = Expression::Literal(String::from("5"));
        let untyped = BinaryOp::<UntypedAST>::new(BinaryOpType::Addition,left,right);
        let result = analyze_binary_op(Box::new(untyped)).expect("should analyze binary op");

        let (op_type, l_expr, r_expr) = result.destructure();
        match op_type {
            BinaryOpType::Addition => (),
            other => panic!("unexpected binary op: {:?}", other),
        }
        match l_expr {
            Expression::Literal(Literal::S32(17)) => (),
            other => panic!("unexpected left operand: {:?}", other),
        }
        match r_expr {
            Expression::Literal(Literal::S32(5)) => (),
            other => panic!("unexpected right operand: {:?}", other),
        }
    }
}