use crate::PosInfoWrapper;
use crate::misc::{datatype_parser, identifier_parser, just_token};
use ast::UntypedAST;
use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
use ast::symbol::FunctionCall;
use chumsky::combinator::To;
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use shared::code_reference::{CodeArea, CodeLocation};

fn narrow<
    'src,
    T: Parser<
            'src,
            &'src [PosInfoWrapper<Token, CodeFile>],
            PosInfoWrapper<Expression<UntypedAST>>,
        > + Clone,
>(
    input: T,
) -> T {
    input
}
/** This parses a slice of tokens into an expression
*/
pub(crate) fn expression_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<Expression<UntypedAST>>>
+ Clone {
    recursive(|expr| {
        let expr = narrow(expr);
        let literal = custom::<
            _,
            &[PosInfoWrapper<Token, CodeFile>],
            PosInfoWrapper<Expression<UntypedAST>>,
            _,
        >(|token| {
            {
                let next_token = token.next().ok_or(EmptyErr::default())?;
                let (tok, pos) = (next_token.inner, next_token.pos_info);
                Ok(PosInfoWrapper::new(
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
        });

        let ident = identifier_parser();

        let call = ident
            .clone()
            .then(
                expr.clone()
                    .separated_by(just_token(TokenType::ArgumentSeparator))
                    .collect::<Vec<PosInfoWrapper<Expression<UntypedAST>>>>()
                    .delimited_by(
                        just_token(TokenType::OpenParen),
                        just_token(TokenType::CloseParen),
                    ),
            ) //.then(just(TokenType::Return).ignore_then(ident).or_not())
            .map(|(name, args)| {
                // new only returns None if start > end
                // If this is the case, then there is a bug
                // So the error is unrecoverable
                let pos = CodeArea::new(
                    name.pos_info.start().clone(),
                    args.last()
                        .map(|arg| arg.pos_info.end())
                        .unwrap_or(name.pos_info.end())
                        .clone(),
                    name.pos_info.file().clone(),
                )
                .unwrap();
                PosInfoWrapper::new(
                    Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
                        name.inner,
                        args.into_iter().map(|arg| arg.inner).collect(),
                    )),
                    pos,
                )
            });

        let atom = call
            .or(literal)
            .or(ident.map(|input| input.map(Expression::Variable)))
            .or(expr.delimited_by(
                just_token(TokenType::OpenParen),
                just_token(TokenType::CloseParen),
            ));

        let typecast = atom
            .clone()
            .then_ignore(just_token(TokenType::As))
            .then(datatype_parser())
            .map(|(expr, new_type)| {
                // new only returns None if start > end
                // If this is the case, then there is a bug
                // So the error is unrecoverable
                let pos = CodeArea::new(
                    expr.pos_info.start().clone(),
                    new_type.pos_info().end().clone(),
                    expr.pos_info.file().clone(),
                )
                .unwrap();
                PosInfoWrapper::new(
                    Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(new_type.inner)),
                        expr.inner,
                    ))),
                    pos,
                )
            });

        let unary_op = choice((
            single_unary(TokenType::Subtraction, UnaryOpType::Negative),
            single_unary(TokenType::Not, UnaryOpType::Not),
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
    input: impl Parser<
        'a,
        &'a [PosInfoWrapper<Token, CodeFile>],
        PosInfoWrapper<Expression<UntypedAST>>,
    > + Clone,
    ops: &[(TokenType, BinaryOpType)],
) -> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<Expression<UntypedAST>>> + Clone
{
    input.clone().foldl(
        choice(
            ops.iter()
                .map(|(token, op)| single_binary(token.clone(), *op))
                .collect::<Vec<_>>(),
        )
        .then(input)
        .repeated(),
        |lhs, (op, rhs)| op(lhs, rhs),
    )
}

// There is no way known to me to split the return type up
#[allow(clippy::type_complexity)]
fn single_unary<'a>(
    input: TokenType,
    operator_type: UnaryOpType<UntypedAST>,
) -> To<
    impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<TokenType, CodeArea>> + Clone,
    //Just<TokenType, &'a [PosInfoWrapper<Token>], Full<EmptyErr, (), ()>>,
    PosInfoWrapper<TokenType>,
    impl Clone + Fn(PosInfoWrapper<Expression<UntypedAST>>) -> PosInfoWrapper<Expression<UntypedAST>>,
> {
    just_token(input).to(move |input: PosInfoWrapper<Expression<UntypedAST>>| {
        let (expr, pos) = (input.inner, input.pos_info);
        PosInfoWrapper::new(
            Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                operator_type.clone(),
                expr,
            ))),
            pos,
        )
    })
}

// There is no way known to me to split the return type up
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
}

#[cfg(test)]
mod tests {
    use crate::expression::expression_parser;
    use crate::test_shared::prepare_token;
    use ast::UntypedAST;
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
    use ast::symbol::FunctionCall;
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
        .map(prepare_token);

        let parser = expression_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        let expected = Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            "test".to_string(),
            vec![
                Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                    Expression::Literal("5".to_string()),
                ))),
                Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    Expression::Variable("test2".to_string()),
                    Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        Expression::Literal("5".to_string()),
                        Expression::Literal("10".to_string()),
                    ))),
                ))),
            ],
        ));
        assert_eq!(parsed.inner(), &expected);
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
        .map(prepare_token);

        let parser = expression_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        let expected = Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            "test".to_string(),
            vec![
                Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                    Expression::Literal("5".to_string()),
                ))),
                Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    Expression::Variable("test2".to_string()),
                    Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        Expression::Literal("5".to_string()),
                        Expression::Literal("10".to_string()),
                    ))),
                ))),
            ],
        ));
        assert_eq!(parsed.inner, expected);
    }

    #[test]
    fn parse_just_idendifier() {
        let to_parse = [TokenType::Identifier("test".to_string())].map(prepare_token);

        let parser = expression_parser();

        let expected = Expression::Variable("test".to_string());
        assert_eq!(&expected, parser.parse(&to_parse).unwrap().inner());
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
        .map(prepare_token);

        let parser = expression_parser();

        assert!(parser.parse(&to_parse).has_errors());
    }
}
