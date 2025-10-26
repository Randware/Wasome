use crate::misc::{datatype_parser, identifier_parser};
use crate::statement::statement_parser;
use ast::UntypedAST;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::{Function, TopLevelElement};
use chumsky::prelude::*;
use lexer::TokenType;
use std::rc::Rc;

/** This parses a slice of tokens into an arbitiary top-level element
*/
pub(crate) fn top_level_parser<'src>()
-> impl Parser<'src, &'src [TokenType], TopLevelElement<UntypedAST>> {
    // This currently only handles functions
    function_parser().map(TopLevelElement::Function)
}

/** This parses a slice of tokens into a function
*/
fn function_parser<'src>() -> impl Parser<'src, &'src [TokenType], Function<UntypedAST>> {
    let statement = statement_parser();
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let param = data_type
        .clone()
        .then(ident.clone())
        .map(|(data_type, name)| Rc::new(VariableSymbol::new(name, data_type)));

    just(TokenType::Function)
        .ignore_then(ident)
        .then(
            param
                .clone()
                .separated_by(just(TokenType::ArgumentSeparator))
                .collect::<Vec<Rc<VariableSymbol<UntypedAST>>>>()
                .delimited_by(just(TokenType::OpenParen), just(TokenType::CloseParen)),
        )
        .then(just(TokenType::Return).ignore_then(data_type).or_not())
        .then(statement)
        .map(|(((name, params), return_type), implementation)| {
            Function::new(
                Rc::new(FunctionSymbol::new(name, return_type, params)),
                implementation,
            )
        })
}
#[cfg(test)]
mod tests {
    use crate::top_level::top_level_parser;
    use ast::UntypedAST;
    use ast::block::CodeBlock;
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
    use ast::statement::{Statement, VariableDeclaration};
    use ast::symbol::{FunctionCall, FunctionSymbol, VariableSymbol};
    use ast::top_level::{Function, TopLevelElement};
    use chumsky::Parser;
    use lexer::TokenType;
    use std::rc::Rc;

    #[test]
    fn parse() {
        let to_parse = vec![
            TokenType::Function,
            TokenType::Identifier("func".to_string()),
            TokenType::OpenParen,
            TokenType::CloseParen,
            TokenType::OpenScope,
            TokenType::StatementSeparator,
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
            TokenType::StatementSeparator,
            TokenType::CloseScope,
        ];

        let parser = top_level_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        println!("{:?}", parsed);
        let expected = TopLevelElement::Function(Function::new(
            Rc::new(FunctionSymbol::new("func".to_string(), None, Vec::new())),
            Statement::Codeblock(CodeBlock::new(vec![Statement::VariableDeclaration(
                VariableDeclaration::<UntypedAST>::new(
                    Rc::new(VariableSymbol::new("var".to_string(), "bool".to_string())),
                    Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
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
                    )),
                ),
            )])),
        ));
        assert_eq!(parsed, expected);
    }
}
