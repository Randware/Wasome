use ast::{TypedAST, UntypedAST};
use ast::expression::{BinaryOp, Expression, Literal, Typecast, UnaryOp, UnaryOpType};
use crate::mics_sa::analyze_data_type;

pub(crate) fn analyze_expression(to_analyze: Expression<UntypedAST>) -> Option<Expression<TypedAST>>
{
    Some(match to_analyze
    {
        Expression::FunctionCall(_) => todo!(),
        Expression::Variable(_) => todo!(),
        Expression::Literal(inner) => Expression::Literal(analyze_literal(&inner)?),
        Expression::UnaryOp(inner) => Expression::UnaryOp(analyze_unary_op(inner)?),
        Expression::BinaryOp(inner) => Expression::BinaryOp(analyze_binary_op(inner)?),
    })
}

fn analyze_literal(to_analyze: &str) -> Option<Literal>
{
    todo!()
}

fn analyze_unary_op(to_analyze: Box<UnaryOp<UntypedAST>>) -> Option<Box<UnaryOp<TypedAST>>> {
    let (op_type, expression) = to_analyze.destructure();
    let converted_input = analyze_expression(expression)?;
    let converted_unary_op_type = match op_type
    {
        UnaryOpType::Typecast(inner) =>
            {
                let data_type = inner.target();
                let analyzed_data_type = analyze_data_type(data_type)?;
                let typed_typecast = Typecast::<TypedAST>::new(analyzed_data_type);
                UnaryOpType::Typecast(typed_typecast)
            }
        UnaryOpType::Negative => UnaryOpType::Negative,
        UnaryOpType::Not => UnaryOpType::Not

    };

    let analyzed = UnaryOp::<TypedAST>::new(converted_unary_op_type, converted_input)?;
    Some(Box::new(analyzed))
}

fn analyze_binary_op(to_analyze: Box<BinaryOp<UntypedAST>>) -> Option<Box<BinaryOp<TypedAST>>>
{
    todo!()
}