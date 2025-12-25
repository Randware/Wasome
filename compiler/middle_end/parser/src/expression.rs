use crate::misc::{datatype_parser, identifier_parser, just_token};
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, Typecast, UnaryOp, UnaryOpType,
};
use ast::{ASTNode, UntypedAST};
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use shared::code_reference::{CodeArea, CodeLocation};

fn narrow<
    'src,
    T: Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Expression<UntypedAST>>> + Clone,
>(
    input: T,
) -> T {
    input
}
/** This parses a slice of tokens into an expression
*/
pub(crate) fn expression_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Expression<UntypedAST>>> + Clone
{
    recursive(|expr| {
        let expr = narrow(expr);
        let literal =
            custom::<_, &[PosInfoWrapper<Token, CodeFile>], ASTNode<Expression<UntypedAST>>, _>(
                |token| {
                    {
                        let next_token = token.next().ok_or(EmptyErr::default())?;
                        let (tok, pos) = (next_token.inner, next_token.pos_info);
                        Ok(ASTNode::new(
                            Expression::Literal(
                                match tok.kind {
                                    TokenType::Decimal(inner) => inner.to_string(),
                                    TokenType::Integer(inner) => inner.to_string(),
                                    _ => return Err(EmptyErr::default()),
                                },
                                // new only returns None if start > end
                                // If this is the case, then there is a bug
                                // So the error is unrecoverable
                            ),
                            CodeArea::new(
                                CodeLocation::new(tok.line, tok.span.start),
                                CodeLocation::new(tok.line, tok.span.end),
                                pos,
                            )
                            .unwrap(),
                        ))
                    }
                },
            );

        let ident = identifier_parser();

        let call = ident
            .clone()
            .then(
                expr.clone()
                    .separated_by(just_token(TokenType::ArgumentSeparator))
                    .collect::<Vec<ASTNode<Expression<UntypedAST>>>>()
                    .delimited_by(
                        just_token(TokenType::OpenParen),
                        just_token(TokenType::CloseParen),
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
                just_token(TokenType::OpenParen),
                just_token(TokenType::CloseParen),
            ));

        let typecast = atom
            .clone()
            .then_ignore(just_token(TokenType::As))
            .then(datatype_parser())
            .map(|(expr, new_type)| {
                let new_pos = combine_code_areas_succeeding(expr.position(), &new_type.pos_info);
                ASTNode::new(
                    Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(new_type.inner)),
                        expr,
                    ))),
                    new_pos,
                )
            });

        let unary_op = choice((
            just_token(TokenType::Subtraction).map(|token| {
                unary_op_mapper(UnaryOpType::Negative, token.pos_info.start().clone())
            }),
            just_token(TokenType::Not)
                .map(|token| unary_op_mapper(UnaryOpType::Not, token.pos_info.start().clone())),
        ));
        let unary = choice((typecast, unary_op.repeated().foldr(atom, |op, rhs| op(rhs))));

        let product = binary_operator_parser(
            unary,
            &[
                (TokenType::Multiplication, BinaryOpType::Multiplication),
                (TokenType::Division, BinaryOpType::Division),
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
                (TokenType::LShift, BinaryOpType::RightShift),
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

fn binary_operator_parser<'a>(
    input: impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], ASTNode<Expression<UntypedAST>>>
    + Clone,
    ops: &[(TokenType, BinaryOpType)],
) -> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], ASTNode<Expression<UntypedAST>>> + Clone
{
    input.clone().foldl(
        choice(
            ops.iter()
                .map(|(token, op)| just_token(token.clone()).map(|_| binary_op_mapper(*op)))
                .collect::<Vec<_>>(),
        )
        .then(input)
        .repeated(),
        |lhs, (op, rhs)| op(lhs, rhs),
    )
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
    .expect("This should never happen. The unary operator is on front of the expression");
    ASTNode::new(
        Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
            operator_type.clone(),
            input,
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
    .expect("This should never happen. The unary operator is on front of the expression");
    ASTNode::new(
        Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            operator_type,
            lhs,
            rhs,
        ))),
        combined_pos,
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

/*// There is no way known to me to split the return type up
#[allow(clippy::type_complexity)]
fn single_binary<'a>(
    input: TokenType,
    operator_type: BinaryOpType,
) -> To<
    impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<TokenType>> + Clone,
    //Just<TokenType, &'a [TokenType], Full<EmptyErr, (), ()>>,
    PosInfoWrapper<TokenType>,
    impl Clone
    + Fn(
        PosInfoWrapper<Expression<UntypedAST>>,
        PosInfoWrapper<Expression<UntypedAST>>,
    ) -> PosInfoWrapper<Expression<UntypedAST>>,
> {
    just_token(input).to(
        move |lhs: PosInfoWrapper<Expression<UntypedAST>>,
              rhs: PosInfoWrapper<Expression<UntypedAST>>| {
            {
                let (lhs_expr, lhs_pos) = (lhs.inner, lhs.pos_info);
                let (rhs_expr, rhs_pos) = (rhs.inner, rhs.pos_info);
                PosInfoWrapper::new(
                    Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        operator_type,
                        lhs_expr,
                        rhs_expr,
                        // new only returns None if start > end
                        // If this is the case, then there is a bug
                        // So the error is unrecoverable
                    ))),
                    CodeArea::new(
                        lhs_pos.start().clone(),
                        rhs_pos.end().clone(),
                        lhs_pos.file().clone(),
                    )
                    .unwrap(),
                )
            }
        },
    )
}*/

#[cfg(test)]
mod tests {
    use crate::expression::expression_parser;
    use crate::test_shared::{wrap_in_astnode, wrap_token};
    use ast::expression::{
        BinaryOp, BinaryOpType, Expression, FunctionCall, Typecast, UnaryOp, UnaryOpType,
    };
    use ast::{SemanticEquality, UntypedAST};
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
        let expected = wrap_in_astnode(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            "test".to_string(),
            vec![
                wrap_in_astnode(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                    wrap_in_astnode(Expression::Literal("5".to_string())),
                )))),
                wrap_in_astnode(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    wrap_in_astnode(Expression::Variable("test2".to_string())),
                    wrap_in_astnode(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        wrap_in_astnode(Expression::Literal("5".to_string())),
                        wrap_in_astnode(Expression::Literal("10".to_string())),
                    )))),
                )))),
            ],
        )));
        assert!(parsed.semantic_equals(&expected));
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
        let expected = wrap_in_astnode(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            "test".to_string(),
            vec![
                wrap_in_astnode(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                    wrap_in_astnode(Expression::Literal("5".to_string())),
                )))),
                wrap_in_astnode(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    wrap_in_astnode(Expression::Variable("test2".to_string())),
                    wrap_in_astnode(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        wrap_in_astnode(Expression::Literal("5".to_string())),
                        wrap_in_astnode(Expression::Literal("10".to_string())),
                    )))),
                )))),
            ],
        )));
        assert!(parsed.semantic_equals(&expected));
    }

    #[test]
    fn parse_just_idendifier() {
        let to_parse = [TokenType::Identifier("test".to_string())].map(wrap_token);

        let parser = expression_parser();

        let expected = wrap_in_astnode(Expression::Variable("test".to_string()));
        assert!(expected.semantic_equals(&parser.parse(&to_parse).unwrap()));
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
