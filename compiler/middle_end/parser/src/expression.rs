use chumsky::combinator::To;
use chumsky::extra::Full;
use chumsky::prelude::*;
use chumsky::primitive::Just;
use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
use ast::symbol::FunctionCall;
use ast::UntypedAST;
use lexer::Token;
use crate::misc::{datatype_parser, identifier_parser};

pub(crate) fn expression_parser<'src>() -> impl Parser<'src, &'src [Token], Expression<UntypedAST>>+Clone
{

    recursive(|expr| {
        let literal = custom::<_, &[Token], _, _>(|token|
        Ok(Expression::Literal(match token.next().ok_or(EmptyErr::default())?
        {
            Token::Decimal(inner) => inner.to_string(),
            Token::Integer(inner) => inner.to_string(),
            _ => return Err(EmptyErr::default())
        })));

        let ident = identifier_parser();

        let call = ident.clone()
            .then(
                expr.clone()
                    .separated_by(just(Token::Semicolon)) //TODO
                    .collect::<Vec<Expression<UntypedAST>>>()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )//.then(just(Token::Return).ignore_then(ident).or_not())
            .map(|(name, args)|
                Expression::FunctionCall(FunctionCall::<UntypedAST>::new(name, args))
                    );

        let atom = call
            .or(literal)
            .or(ident.map(Expression::Variable))
            .or(expr.delimited_by(just(Token::OpenParen), just(Token::CloseParen)));

        let typecast =
            atom.clone()
            .then_ignore(just(Token::As))
            .then(datatype_parser())
            .map(|(expr, new_type)|
            Expression::UnaryOp(
                Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(
                        Typecast::new(new_type)
                    ),
                    expr
                ))
            ));

        let unary_op = choice((
            unary(Token::Subtraction, UnaryOpType::Negative),
            unary(Token::Not, UnaryOpType::Not)
        ));
        let unary =
        choice((
            typecast,
            unary_op
                .repeated()
                .foldr(atom, |op, rhs| op(rhs))
            ));

        let product = binary_operator_parser(unary,
                &[(Token::Multiplication, BinaryOpType::Multiplication),
                (Token::Division, BinaryOpType::Division),
                (Token::Modulo, BinaryOpType::Modulo)],
            ).boxed();

        let sum = binary_operator_parser(product,
                                         &[(Token::Addition, BinaryOpType::Addition),
                                             (Token::Subtraction, BinaryOpType::Subtraction),],
        );

        let bitshift = binary_operator_parser(sum,
                                         &[(Token::RShift, BinaryOpType::RightShift),
                                             (Token::LShift, BinaryOpType::RightShift)],
        ).boxed();

        let comparison = binary_operator_parser(bitshift,
                                              &[(Token::LessThan, BinaryOpType::Lesser),
                                                  (Token::LessThanEqual, BinaryOpType::LesserEquals),
                                                  (Token::GreaterThanEqual, BinaryOpType::GreaterEquals),
                                                  (Token::GreaterThan, BinaryOpType::Greater)],
        );

        let equals = binary_operator_parser(comparison,
                                              &[(Token::Comparison, BinaryOpType::Equals),
                                                  (Token::NotEqual, BinaryOpType::NotEquals)],
        ).boxed();

        let bitand = binary_operator_parser(equals,
                                            &[(Token::BitAnd, BinaryOpType::BitwiseAnd)],
        );

        let bitor = binary_operator_parser(bitand,
                                            &[(Token::BitOr, BinaryOpType::BitwiseOr)],
        ).boxed();

        let and = binary_operator_parser(bitor,
                                            &[(Token::And, BinaryOpType::And)],
        );

        let or = binary_operator_parser(and,
                                           &[(Token::Or, BinaryOpType::Or)],
        ).boxed();

        or
    })
}

fn binary_operator_parser<'a>(
    input: impl Parser<'a, &'a [Token], Expression<UntypedAST>>+Clone,
    ops: &[(Token, BinaryOpType)])
    -> impl Parser<'a, &'a [Token], Expression<UntypedAST>>+Clone
{
    input.clone().foldl(
        choice(ops
            .iter()
            .map(|(token, op)|
                binary(token.clone(), op.clone())).collect::<Vec<_>>())
            .then(input)
            .repeated(),
        |lhs, (op, rhs)| op(lhs, rhs),
    )
}

fn unary<'a>(input: Token, operator_type: UnaryOpType<UntypedAST>)
             -> To<Just<Token, &'a [Token], Full<EmptyErr, (), ()>>, Token, impl Clone+Fn(Expression<UntypedAST>) -> Expression<UntypedAST>>
{
    just(input).to(move |input|
        Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(operator_type.clone(), input))))
}

fn binary<'a>(input: Token, operator_type: BinaryOpType)
    -> To<Just<Token, &'a [Token], Full<EmptyErr, (), ()>>, Token, impl Clone+Fn(Expression<UntypedAST>, Expression<UntypedAST>) -> Expression<UntypedAST>>
{
    just(input).to(move |lhs, rhs|
        Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(operator_type.clone(), lhs, rhs))))
}

#[cfg(test)]
mod tests
{
    use lexer::Token;
    use crate::expression::expression_parser;
    use chumsky::Parser;
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
    use ast::symbol::FunctionCall;
    use ast::UntypedAST;

    #[test]
    fn parse()
    {
        let to_parse = vec![
            Token::Identifier("test".to_string()),
            Token::OpenParen,
                Token::Integer(5), Token::As, Token::F32, Token::Semicolon, //TODO
                Token::Identifier("test2".to_string()), Token::NotEqual, Token::Decimal(5.0), Token::Multiplication, Token::Decimal(10.0),
            Token::CloseParen
            //Token::Integer(10), Token::As, Token::Identifier("f32".to_string())
        ];

        let parser = expression_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        let expected =  Expression::FunctionCall(
            FunctionCall::<UntypedAST>::new(
                "test".to_string(),
                vec![
                    Expression::UnaryOp(
                        Box::new(UnaryOp::<UntypedAST>::new(
                            UnaryOpType::Typecast(
                                Typecast::new("f32".to_string())
                            ),
                            Expression::Literal("5".to_string())
                        ))
                    ),
                    Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::NotEquals,
                        Expression::Variable("test2".to_string()),
                        Expression::BinaryOp(
                            Box::new(BinaryOp::<UntypedAST>::new(
                                BinaryOpType::Multiplication,
                                Expression::Literal("5".to_string()),
                                Expression::Literal("10".to_string())
                            ))
                        )
                    )))
                ]
            )
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_parens()
    {
        let to_parse = vec![
            Token::OpenParen,
            Token::Identifier("test".to_string()),
            Token::OpenParen,
            Token::Integer(5), Token::As, Token::F32, Token::Semicolon, //TODO
            Token::Identifier("test2".to_string()), Token::NotEqual, Token::Decimal(5.0), Token::Multiplication, Token::Decimal(10.0),
            Token::CloseParen,
            Token::CloseParen
            //Token::Integer(10), Token::As, Token::Identifier("f32".to_string())
        ];

        let parser = expression_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        let expected =  Expression::FunctionCall(
            FunctionCall::<UntypedAST>::new(
                "test".to_string(),
                vec![
                    Expression::UnaryOp(
                        Box::new(UnaryOp::<UntypedAST>::new(
                            UnaryOpType::Typecast(
                                Typecast::new("f32".to_string())
                            ),
                            Expression::Literal("5".to_string())
                        ))
                    ),
                    Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::NotEquals,
                        Expression::Variable("test2".to_string()),
                        Expression::BinaryOp(
                            Box::new(BinaryOp::<UntypedAST>::new(
                                BinaryOpType::Multiplication,
                                Expression::Literal("5".to_string()),
                                Expression::Literal("10".to_string())
                            ))
                        )
                    )))
                ]
            )
        );
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_just_idendifier()
    {
        let to_parse = vec![
            Token::Identifier("test".to_string())
        ];

        let parser = expression_parser();

        let expected = Expression::Variable("test".to_string());
        assert_eq!(expected, parser.parse(&to_parse).unwrap());
    }

    #[test]
    fn parse_invalid()
    {
        let to_parse = vec![
            Token::Bool,
            Token::Identifier("var".to_string()),
            Token::Assign,
            Token::Identifier("test".to_string()),
            Token::OpenParen,
            Token::Integer(5), Token::As, Token::F32, Token::Semicolon, //TODO
            Token::Identifier("test2".to_string()), Token::NotEqual, Token::Decimal(5.0), Token::Multiplication, Token::Decimal(10.0),
            Token::CloseParen
        ];

        let parser = expression_parser();

        assert!(parser.parse(&to_parse).has_errors());
    }
}