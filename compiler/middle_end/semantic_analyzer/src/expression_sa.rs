use crate::mics_sa::analyze_data_type;
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, Literal, Typecast, UnaryOp, UnaryOpType,
};
use ast::{ASTType, TypedAST, UntypedAST};

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

    let converted_binary_op_type = match op_type {
        BinaryOpType::Addition => BinaryOpType::Addition,
        BinaryOpType::Subtraction => BinaryOpType::Subtraction,
        BinaryOpType::Multiplication => BinaryOpType::Multiplication,
        BinaryOpType::Division => BinaryOpType::Division,
        BinaryOpType::Modulo => BinaryOpType::Modulo,
        BinaryOpType::LeftShift => BinaryOpType::LeftShift,
        BinaryOpType::RightShift => BinaryOpType::RightShift,
        BinaryOpType::BitwiseOr => BinaryOpType::BitwiseOr,
        BinaryOpType::Or => BinaryOpType::Or,
        BinaryOpType::BitwiseAnd => BinaryOpType::BitwiseAnd,
        BinaryOpType::And => BinaryOpType::And,
        BinaryOpType::BitwiseXor => BinaryOpType::BitwiseXor,
        BinaryOpType::Xor => BinaryOpType::Xor,
        BinaryOpType::Equals => BinaryOpType::Equals,
        BinaryOpType::NotEquals => BinaryOpType::NotEquals,
        BinaryOpType::Greater => BinaryOpType::Greater,
        BinaryOpType::GreaterEquals => BinaryOpType::GreaterEquals,
        BinaryOpType::Lesser => BinaryOpType::Lesser,
        BinaryOpType::LesserEquals => BinaryOpType::LesserEquals,
    };
    let analyzed =
        BinaryOp::<TypedAST>::new(converted_binary_op_type, converted_left, converted_right)?;

    Some(Box::new(analyzed))
}
