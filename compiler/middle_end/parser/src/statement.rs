use crate::expression::expression_parser;
use crate::misc::{datatype_parser, identifier_parser, statement_seperator};
use ast::UntypedAST;
use ast::block::CodeBlock;
use ast::statement::{
    Conditional, ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
    VariableDeclaration,
};
use ast::symbol::VariableSymbol;
use chumsky::prelude::*;
use lexer::Token;
use lexer::Token::CloseParen;
use std::rc::Rc;

/** This parses a slice of tokens into a statement
*/
pub(crate) fn statement_parser<'src>() -> impl Parser<'src, &'src [Token], Statement<UntypedAST>> {
    recursive(|statement| {
        let data_type = datatype_parser();
        let ident = identifier_parser();

        let expression = expression_parser();

        let variable_assignment = ident
            .clone()
            .then_ignore(just(Token::Assign))
            .then(expression.clone())
            .map(|(name, val)| VariableAssignment::<UntypedAST>::new(name, val));

        let variable_declaration = data_type
            .clone()
            .then(ident)
            .then_ignore(just(Token::Assign))
            .then(expression.clone())
            .map(|((data_type, name), val)| {
                VariableDeclaration::<UntypedAST>::new(
                    Rc::new(VariableSymbol::new(name, data_type)),
                    val,
                )
            });

        let return_statement = just(Token::Return)
            .ignore_then(expression.clone().or_not())
            .map(Return::new);

        let conditional = just(Token::If)
            .ignore_then(
                expression
                    .clone()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseScope)),
            )
            .then_ignore(maybe_statement_separator())
            .then(statement.clone())
            .then(
                maybe_statement_separator()
                    .then_ignore(just(Token::Else))
                    .then_ignore(maybe_statement_separator())
                    .ignore_then(statement.clone())
                    .or_not(),
            )
            .map(|((cond, then), else_statement)| Conditional::new(cond, then, else_statement));

        let loop_body = maybe_statement_separator().ignore_then(statement.clone());
        let loop_statement = just(Token::Loop)
            .ignore_then(just(Token::OpenParen))
            .ignore_then(choice((
                just(CloseParen)
                    .ignore_then(loop_body.clone())
                    .map(|body| (body, LoopType::Infinite)),
                expression
                    .clone()
                    .then_ignore(just(CloseParen))
                    .then(loop_body.clone())
                    .map(|(cond, body)| (body, LoopType::While(cond))),
                statement
                    .clone()
                    .then_ignore(just(Token::Semicolon))
                    .then(expression.clone().then_ignore(just(Token::Semicolon)))
                    .then(statement.clone())
                    .then_ignore(just(CloseParen))
                    .then(loop_body.clone())
                    .map(|(((init, cond), after_each), body)| {
                        (
                            body,
                            LoopType::For {
                                start: init,
                                cond,
                                after_each,
                            },
                        )
                    }),
            )))
            .map(|(body, loop_type)| Loop::new(body, loop_type));

        let code_block = statement
            .clone()
            .separated_by(statement_seperator())
            .allow_leading()
            .allow_trailing()
            .collect::<Vec<Statement<UntypedAST>>>()
            .delimited_by(just(Token::OpenScope), just(Token::CloseScope))
            .map(CodeBlock::new);

        choice((
            variable_assignment.map(Statement::VariableAssignment),
            variable_declaration.map(Statement::VariableDeclaration),
            conditional.map(|cond| {
                Statement::ControlStructure(Box::new(ControlStructure::Conditional(cond)))
            }),
            loop_statement
                .map(|lst| Statement::ControlStructure(Box::new(ControlStructure::Loop(lst)))),
            code_block.map(Statement::Codeblock),
            return_statement.map(Statement::Return),
            expression.map(Statement::Expression),
        ))
    })
}

/** This parses a statement seperator or nothing
*/
fn maybe_statement_separator<'a>() -> impl Parser<'a, &'a [Token], ()> + Clone {
    just(Token::StatementSeparator).or_not().ignored()
}
#[cfg(test)]
mod tests {
    use crate::statement::statement_parser;
    use ast::UntypedAST;
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
    use ast::statement::{Statement, VariableDeclaration};
    use ast::symbol::{FunctionCall, VariableSymbol};
    use chumsky::Parser;
    use lexer::Token;
    use std::rc::Rc;

    #[test]
    fn parse() {
        let to_parse = vec![
            Token::Bool,
            Token::Identifier("var".to_string()),
            Token::Assign,
            Token::Identifier("test".to_string()),
            Token::OpenParen,
            Token::Integer(5),
            Token::As,
            Token::F32,
            Token::ArgumentSeparator,
            Token::Identifier("test2".to_string()),
            Token::NotEqual,
            Token::Decimal(5.0),
            Token::Multiplication,
            Token::Decimal(10.0),
            Token::CloseParen,
        ];

        let parser = statement_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        let expected = Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
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
        ));
        assert_eq!(parsed, expected);
    }
}
