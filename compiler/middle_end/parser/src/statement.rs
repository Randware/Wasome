use crate::expression::expression_parser;
use crate::misc::{datatype_parser, identifier_parser, just_token, statement_seperator};
use ast::UntypedAST;
use ast::block::CodeBlock;
use ast::statement::{
    Conditional, ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
    VariableDeclaration,
};
use ast::symbol::VariableSymbol;
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use std::rc::Rc;
use ast::expression::Expression;
use lazy_static::lazy_static;
use shared::code_file;
use shared::code_file::CodeFile;
use shared::code_reference::CodeArea;
use crate::PosInfoWrapper;

fn narrow<'src, T: Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<Statement<UntypedAST>>>+Clone> (input: T) -> T {input}

/** This parses a slice of tokens into a statement
*/
pub(crate) fn statement_parser<'src>() -> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<Statement<UntypedAST>>> {
    recursive(|statement| {
        let statement = narrow(statement);
        let data_type = datatype_parser();
        let ident = identifier_parser();

        let expression = expression_parser();

        let variable_assignment = ident
            .clone()
            .then_ignore(just_token(TokenType::Assign))
            .then(expression.clone())
            .map(|(name, val)| {
                // new only returns None if start > end
                // If this is the case, then there is a bug
                // So the error is unrecoverable
                let pos = CodeArea::new(name.pos_info().start().clone(), val.pos_info().end().clone(), name.pos_info().file().clone()).unwrap();
                PosInfoWrapper::new(VariableAssignment::<UntypedAST>::new(name.inner, val.inner), pos)
            });

        let variable_declaration = data_type
            .clone()
            .then(ident)
            .then_ignore(just_token(TokenType::Assign))
            .then(expression.clone())
            .map(|((data_type, name), val)| {
                // new only returns None if start > end
                // If this is the case, then there is a bug
                // So the error is unrecoverable
                let pos = CodeArea::new(data_type.pos_info().start().clone(), val.pos_info().end().clone(), name.pos_info().file().clone()).unwrap();
                PosInfoWrapper::new(VariableDeclaration::<UntypedAST>::new(
                    Rc::new(VariableSymbol::new(name.inner, data_type.inner)),
                    val.inner,
                ), pos)
            });

        let return_statement = just_token(TokenType::Return)
            .then(expression.clone().or_not())
            .map(|(return_keyword, to_return)|
                {
                    // new only returns None if start > end
                    // If this is the case, then there is a bug
                    // So the error is unrecoverable
                    let pos = CodeArea::new(return_keyword.pos_info().start().clone(), to_return.as_ref().map(|pos| pos.pos_info.end().clone()).unwrap_or(return_keyword.pos_info().end().clone()), return_keyword.pos_info().file().clone()).unwrap();
                    PosInfoWrapper::new(Return::<UntypedAST>::new(
                        to_return.map(|to_map| to_map.inner),
                    ), pos)
                });

        let conditional = just_token(TokenType::If)
            .then(
                expression
                    .clone()
                    .delimited_by(just_token(TokenType::OpenParen), just_token(TokenType::CloseScope)),
            )
            .then_ignore(maybe_statement_separator())
            .then(statement.clone())
            .then(
                maybe_statement_separator()
                    .then_ignore(just_token(TokenType::Else))
                    .then_ignore(maybe_statement_separator())
                    .ignore_then(statement.clone())
                    .or_not(),
            )
            .map(|(((if_keyword, cond), then), else_statement)|
                {
                    // new only returns None if start > end
                    // If this is the case, then there is a bug
                    // So the error is unrecoverable
                    let pos = CodeArea::new(if_keyword.pos_info().start().clone(), else_statement.as_ref().map(|pos| pos.pos_info.end().clone()).unwrap_or(then.pos_info().end().clone()), if_keyword.pos_info().file().clone()).unwrap();
                    PosInfoWrapper::new(Conditional::new(cond.inner, then.inner, else_statement.map(|to_map| to_map.inner)), pos)
                });

        let loop_body = maybe_statement_separator().ignore_then(statement.clone());
        let loop_statement = just_token(TokenType::Loop)
            .then_ignore(just_token(TokenType::OpenParen))
            .then(choice((
                just_token(TokenType::CloseParen)
                    .ignore_then(loop_body.clone())
                    .map(|body| (body, LoopType::Infinite)),
                expression
                    .clone()
                    .then_ignore(just_token(TokenType::CloseParen))
                    .then(loop_body.clone())
                    .map(|(cond, body)| (body, LoopType::While(cond.inner))),
                statement
                    .clone()
                    .then_ignore(just_token(TokenType::Semicolon))
                    .then(expression.clone().then_ignore(just_token(TokenType::Semicolon)))
                    .then(statement.clone())
                    .then_ignore(just_token(TokenType::CloseParen))
                    .then(loop_body.clone())
                    .map(|(((init, cond), after_each), body)| {
                        (
                            body,
                            LoopType::For {
                                start: init.inner,
                                cond: cond.inner,
                                after_each: after_each.inner,
                            },
                        )
                    }),
            )))
            .map(|(loop_keyword, (body, loop_type))|
                {
                    let pos = CodeArea::new(loop_keyword.pos_info().start().clone(), body.pos_info.end().clone(), loop_keyword.pos_info().file().clone()).unwrap();

                    PosInfoWrapper::new(Loop::new(body.inner, loop_type), pos)
                });

        let code_block = just_token(TokenType::OpenScope).then(
            statement
            .clone()
            .separated_by(statement_seperator())
            .allow_leading()
            .allow_trailing()
            .collect::<Vec<PosInfoWrapper<Statement<UntypedAST>>>>())
            .then(just_token(TokenType::CloseScope))
            //.delimited_by(just_token(TokenType::OpenScope), just_token(TokenType::CloseScope))
            .map(|((open, block),close)|{
                let pos = CodeArea::new(open.pos_info().start().clone(), close.pos_info.end().clone(), close.pos_info().file().clone()).unwrap();
                PosInfoWrapper::new(CodeBlock::new(block.into_iter().map(|statement| statement.inner).collect()), pos)
            });

        choice((
            variable_assignment.map(|var_assign| var_assign.map(Statement::VariableAssignment)),
            variable_declaration.map(|var_decl| var_decl.map(Statement::VariableDeclaration)),
            conditional.map(|cond| {
                cond.map(|inner|
                Statement::ControlStructure(Box::new(ControlStructure::Conditional(inner))))
            }),
            loop_statement
                .map(|lst| lst.map(|inner| Statement::ControlStructure(Box::new(ControlStructure::Loop(inner))))),
            code_block.map(|code_block| code_block.map(Statement::Codeblock)),
            return_statement.map(|return_statement| return_statement.map(Statement::Return)),
            expression.map(|expr| expr.map(Statement::Expression)),
        ))
    })
}

/** This parses a statement seperator or nothing
*/
fn maybe_statement_separator<'a>() -> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], ()> + Clone {
    just_token(TokenType::StatementSeparator).or_not().ignored()
}
#[cfg(test)]
mod tests {
    use crate::statement::statement_parser;
    use ast::UntypedAST;
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
    use ast::statement::{Statement, VariableDeclaration};
    use ast::symbol::{FunctionCall, VariableSymbol};
    use chumsky::Parser;
    use lexer::TokenType;
    use std::rc::Rc;
    use crate::test_shared::prepare_token;

    #[test]
    fn parse() {
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
        ].map(prepare_token);

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
        assert_eq!(parsed.inner(), &expected);
    }
}
