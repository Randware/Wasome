use std::rc::Rc;
use chumsky::prelude::*;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::{Function, TopLevelElement};
use ast::UntypedAST;
use lexer::Token;
use crate::misc::{datatype_parser, identifier_parser};
use crate::statement::statement_parser;

pub(crate) fn top_level_parser<'src>() -> impl Parser<'src, &'src [Token], TopLevelElement<UntypedAST>>
{
    // This currently only handles functions
    function_parser().map(|func| TopLevelElement::Function(func))
}

fn function_parser<'src>() -> impl Parser<'src, &'src [Token], Function<UntypedAST>>
{
    let statement = statement_parser();
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let param = data_type.clone()
        .then(ident.clone())
        .map(|(data_type, name)| Rc::new(VariableSymbol::new(name, data_type)));

    just(Token::Function)
        .ignore_then(ident)
        .then(
        param.clone()
            .separated_by(just(Token::Semicolon)) //TODO
            .collect::<Vec<Rc<VariableSymbol<UntypedAST>>>>()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        )
        .then(just(Token::Return).ignore_then(data_type).or_not())
        .then(statement)
        .map(|(((name, params), return_type), implementation)|
            Function::new(Rc::new(FunctionSymbol::new(name, return_type, params)), implementation)
        )
}
#[cfg(test)]
mod tests
{
    use std::rc::Rc;
    use lexer::Token;
    use chumsky::Parser;
    use ast::block::CodeBlock;
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
    use ast::statement::{Statement, VariableDeclaration};
    use ast::symbol::{FunctionCall, FunctionSymbol, VariableSymbol};
    use ast::top_level::{Function, TopLevelElement};
    use ast::UntypedAST;
    use crate::top_level::top_level_parser;

    #[test]
    fn parse()
    {
        let to_parse = vec![
            Token::Function,
            Token::Identifier("func".to_string()),
            Token::OpenParen,
            Token::CloseParen,
            Token::OpenScope,
            Token::StatementSeparator,
            Token::Bool,
            Token::Identifier("var".to_string()),
            Token::Assign,
            Token::Identifier("test".to_string()),
            Token::OpenParen,
            Token::Integer(5), Token::As, Token::F32, Token::Semicolon, //TODO
            Token::Identifier("test2".to_string()), Token::NotEqual, Token::Decimal(5.0), Token::Multiplication, Token::Decimal(10.0),
            Token::CloseParen,
            Token::StatementSeparator,
            Token::CloseScope
        ];

        let parser = top_level_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        println!("{:?}", parsed);
        let expected = TopLevelElement::Function(Function::new(
            Rc::new(FunctionSymbol::new("func".to_string(), None, Vec::new())),
            Statement::Codeblock(
                CodeBlock::new(
                    vec![
                        Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                            Rc::new(VariableSymbol::new("var".to_string(), "bool".to_string())),
                            Expression::FunctionCall(
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
                            )
                        ))
                    ]
                )
            )
        ));
        assert_eq!(parsed, expected);

    }
}