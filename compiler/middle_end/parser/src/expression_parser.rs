use crate::misc_parsers::{
    cross_module_capable_identifier_parser, datatype_parser, identifier_parser, token_parser,
};
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, Typecast, UnaryOp, UnaryOpType,
};
use ast::{ASTNode, UntypedAST};
use chumsky::prelude::*;
use lexer::TokenType;
use shared::code_reference::{CodeArea, CodeLocation};

/// Parses an expression
pub(crate) fn expression_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], ASTNode<Expression<UntypedAST>>> + Clone {
    recursive(|expr| {
        let literal = custom::<_, &[PosInfoWrapper<TokenType>], ASTNode<Expression<UntypedAST>>, _>(
            |token| {
                let next_token = token.next().ok_or(EmptyErr::default())?;
                let (tok, pos) = (next_token.inner, next_token.pos_info);
                Ok(ASTNode::new(
                    Expression::Literal(match tok {
                        TokenType::Decimal(inner) => {
                            if inner.fract() == 0.0 {
                                format!("{:.1}", inner)
                            } else {
                                inner.to_string()
                            }
                        }
                        TokenType::Integer(inner) => inner.to_string(),
                        TokenType::CharLiteral(inner) => format!("'{}'", inner),
                        TokenType::True => "true".to_owned(),
                        TokenType::False => "false".to_owned(),
                        _ => return Err(EmptyErr::default()),
                    }),
                    pos,
                ))
            },
        );

        let ident = identifier_parser();

        let call = cross_module_capable_identifier_parser()
            .clone()
            .then(
                expr.clone()
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .collect::<Vec<ASTNode<Expression<UntypedAST>>>>()
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    ),
            )
            .map(|(name, args)| {
                let pos = combine_code_areas_succeeding(
                    &name.pos_info,
                    args.last()
                        .map(|to_map| to_map.position())
                        .unwrap_or(name.pos_info()),
                );
                ASTNode::new(
                    Expression::FunctionCall(FunctionCall::<UntypedAST>::new(name.inner, args)),
                    pos,
                )
            });

        let atom = call
            .or(literal)
            .or(ident.map(|input| ASTNode::new(Expression::Variable(input.inner), input.pos_info)))
            .or(expr.delimited_by(
                token_parser(TokenType::OpenParen),
                token_parser(TokenType::CloseParen),
            ));

        let typecast = atom.clone().foldl(
            token_parser(TokenType::As)
                .ignored()
                .then(datatype_parser())
                .repeated(),
            |expr, (_, new_type)| {
                let new_pos = combine_code_areas_succeeding(expr.position(), &new_type.pos_info);
                ASTNode::new(
                    Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(new_type.inner)),
                        expr,
                    ))),
                    new_pos,
                )
            },
        );

        let unary_op = choice((
            token_parser(TokenType::Subtraction).map(|token| {
                unary_op_mapper(UnaryOpType::Negative, token.pos_info.start().clone())
            }),
            token_parser(TokenType::Not)
                .map(|token| unary_op_mapper(UnaryOpType::Not, token.pos_info.start().clone())),
        ));
        let unary = unary_op.repeated().foldr(typecast, |op, rhs| op(rhs));

        let product = binary_operator_parser(
            unary,
            &[
                (TokenType::Multiplication, BinaryOpType::Multiplication),
                (TokenType::Slash, BinaryOpType::Division),
                (TokenType::Modulo, BinaryOpType::Modulo),
            ],
        )
        .boxed();

        let sum = binary_operator_parser(
            product,
            &[
                (TokenType::Addition, BinaryOpType::Addition),
                (TokenType::Subtraction, BinaryOpType::Subtraction),
            ],
        );

        let bitshift = binary_operator_parser(
            sum,
            &[
                (TokenType::RShift, BinaryOpType::RightShift),
                (TokenType::LShift, BinaryOpType::LeftShift),
            ],
        )
        .boxed();

        let comparison = binary_operator_parser(
            bitshift,
            &[
                (TokenType::LessThan, BinaryOpType::Lesser),
                (TokenType::LessThanEqual, BinaryOpType::LesserEquals),
                (TokenType::GreaterThanEqual, BinaryOpType::GreaterEquals),
                (TokenType::GreaterThan, BinaryOpType::Greater),
            ],
        );

        let equals = binary_operator_parser(
            comparison,
            &[
                (TokenType::Comparison, BinaryOpType::Equals),
                (TokenType::NotEqual, BinaryOpType::NotEquals),
            ],
        )
        .boxed();

        let bitand =
            binary_operator_parser(equals, &[(TokenType::BitAnd, BinaryOpType::BitwiseAnd)]);

        let bitor =
            binary_operator_parser(bitand, &[(TokenType::BitOr, BinaryOpType::BitwiseOr)]).boxed();

        let and = binary_operator_parser(bitor, &[(TokenType::And, BinaryOpType::And)]);

        binary_operator_parser(and, &[(TokenType::Or, BinaryOpType::Or)]).boxed()
    })
}

/// Parses binary ops
///
/// # Parameter
///
/// **ops**: The tokens to parse and what to parse them to
fn binary_operator_parser<'a>(
    input: impl Parser<'a, &'a [PosInfoWrapper<TokenType>], ASTNode<Expression<UntypedAST>>> + Clone,
    ops: &[(TokenType, BinaryOpType)],
) -> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], ASTNode<Expression<UntypedAST>>> + Clone {
    input.clone().foldl(
        choice(
            ops.iter()
                .map(|(token, op)| token_parser(token.clone()).map(|_| binary_op_mapper(*op)))
                .collect::<Vec<_>>(),
        )
        .then(input)
        .repeated(),
        |lhs, (op, rhs)| op(lhs, rhs),
    )
}

fn binary_op_mapper(
    token_type: BinaryOpType,
) -> impl Fn(
    ASTNode<Expression<UntypedAST>>,
    ASTNode<Expression<UntypedAST>>,
) -> ASTNode<Expression<UntypedAST>> {
    move |lhs, rhs| map_binary_op(token_type, lhs, rhs)
}

fn map_binary_op(
    operator_type: BinaryOpType,
    lhs: ASTNode<Expression<UntypedAST>>,
    rhs: ASTNode<Expression<UntypedAST>>,
) -> ASTNode<Expression<UntypedAST>> {
    let combined_pos = CodeArea::new(
        lhs.position().start().clone(),
        rhs.position().end().clone(),
        lhs.position().file().clone(),
    )
    .expect("This should never happen. lhs should always be before rhs");
    ASTNode::new(
        Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            operator_type,
            lhs,
            rhs,
        ))),
        combined_pos,
    )
}

fn unary_op_mapper(
    token_type: UnaryOpType<UntypedAST>,
    op_start: CodeLocation,
) -> impl Fn(ASTNode<Expression<UntypedAST>>) -> ASTNode<Expression<UntypedAST>> {
    move |expr| map_unary_op(token_type.clone(), op_start.clone(), expr)
}

fn map_unary_op(
    operator_type: UnaryOpType<UntypedAST>,
    op_start: CodeLocation,
    input: ASTNode<Expression<UntypedAST>>,
) -> ASTNode<Expression<UntypedAST>> {
    let combined_pos = CodeArea::new(
        op_start,
        input.position().end().clone(),
        input.position().file().clone(),
    )
    .expect("This should never happen. The operator should always be before the expression");
    ASTNode::new(
        Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
            operator_type.clone(),
            input,
        ))),
        combined_pos,
    )
}

#[cfg(test)]
mod tests {
    use crate::expression_parser::expression_parser;
    use crate::test_shared::{wrap_in_ast_node, wrap_token};
    use ast::expression::{
        BinaryOp, BinaryOpType, Expression, FunctionCall, Typecast, UnaryOp, UnaryOpType,
    };
    use ast::{SemanticEq, UntypedAST};
    use chumsky::Parser;
    use lexer::TokenType;

    #[test]
    fn parse() {
        let to_parse = [
            TokenType::Identifier("test".to_string()),
            TokenType::OpenParen,
            TokenType::Integer(5),
            TokenType::As,
            TokenType::F32,
            TokenType::ArgumentSeparator,
            TokenType::Identifier("test2".to_string()),
            TokenType::NotEqual,
            TokenType::Decimal(5.0),
            TokenType::Multiplication,
            TokenType::Decimal(10.0),
            TokenType::CloseParen, //TokenType::Integer(10), TokenType::As, TokenType::Identifier("f32".to_string())
        ]
        .map(wrap_token);

        let parser = expression_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        let expected = wrap_in_ast_node(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            "test".to_string(),
            vec![
                wrap_in_ast_node(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                    wrap_in_ast_node(Expression::Literal("5".to_string())),
                )))),
                wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    wrap_in_ast_node(Expression::Variable("test2".to_string())),
                    wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        wrap_in_ast_node(Expression::Literal("5.0".to_string())),
                        wrap_in_ast_node(Expression::Literal("10.0".to_string())),
                    )))),
                )))),
            ],
        )));
        assert!(parsed.semantic_eq(&expected));
    }

    #[test]
    fn parse_parens() {
        let to_parse = [
            TokenType::OpenParen,
            TokenType::Identifier("test".to_string()),
            TokenType::OpenParen,
            TokenType::Integer(5),
            TokenType::As,
            TokenType::F32,
            TokenType::ArgumentSeparator,
            TokenType::Identifier("test2".to_string()),
            TokenType::NotEqual,
            TokenType::Decimal(5.0),
            TokenType::Multiplication,
            TokenType::Decimal(10.0),
            TokenType::CloseParen,
            TokenType::CloseParen, //TokenType::Integer(10), TokenType::As, TokenType::Identifier("f32".to_string())
        ]
        .map(wrap_token);

        let parser = expression_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        let expected = wrap_in_ast_node(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            "test".to_string(),
            vec![
                wrap_in_ast_node(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                    wrap_in_ast_node(Expression::Literal("5".to_string())),
                )))),
                wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    wrap_in_ast_node(Expression::Variable("test2".to_string())),
                    wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        wrap_in_ast_node(Expression::Literal("5.0".to_string())),
                        wrap_in_ast_node(Expression::Literal("10.0".to_string())),
                    )))),
                )))),
            ],
        )));
        assert!(parsed.semantic_eq(&expected));
    }

    #[test]
    fn parse_just_identifier() {
        let to_parse = [TokenType::Identifier("test".to_string())].map(wrap_token);

        let parser = expression_parser();

        let expected = wrap_in_ast_node(Expression::Variable("test".to_string()));
        assert!(expected.semantic_eq(&parser.parse(&to_parse).unwrap()));
    }

    #[test]
    fn parse_invalid() {
        let to_parse = [
            TokenType::Bool,
            TokenType::Identifier("var".to_string()),
            TokenType::Assign,
            TokenType::Identifier("test".to_string()),
            TokenType::OpenParen,
            TokenType::Integer(5),
            TokenType::As,
            TokenType::F32,
            TokenType::ArgumentSeparator,
            TokenType::Identifier("test2".to_string()),
            TokenType::NotEqual,
            TokenType::Decimal(5.0),
            TokenType::Multiplication,
            TokenType::Decimal(10.0),
            TokenType::CloseParen,
        ]
        .map(wrap_token);

        let parser = expression_parser();

        assert!(parser.parse(&to_parse).has_errors());
    }
}
