use crate::expression::expression_parser;
use crate::misc::{datatype_parser, identifier_parser, just_token, statement_separator};
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::statement::{
    CodeBlock, Conditional, ControlStructure, Loop, LoopType, Return, Statement,
    VariableAssignment, VariableDeclaration,
};
use ast::symbol::VariableSymbol;
use ast::{ASTNode, UntypedAST};
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use std::rc::Rc;

fn narrow<
    'src,
    T: Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Statement<UntypedAST>>> + Clone,
>(
    input: T,
) -> T {
    input
}

/** This parses a slice of tokens into a statement
*/
pub(crate) fn statement_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Statement<UntypedAST>>> {
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
                let pos = combine_code_areas_succeeding(&name.pos_info, val.position());
                PosInfoWrapper::new(VariableAssignment::<UntypedAST>::new(name.inner, val), pos)
            });

        let variable_declaration = data_type
            .clone()
            .then(ident)
            .then_ignore(just_token(TokenType::Assign))
            .then(expression.clone())
            .map(|((data_type, name), val)| {
                PosInfoWrapper::new(
                    VariableDeclaration::<UntypedAST>::new(
                        Rc::new(VariableSymbol::new(name.inner, data_type.inner)),
                        val,
                    ),
                    combine_code_areas_succeeding(&name.pos_info, &data_type.pos_info),
                )
            });

        let return_statement = just_token(TokenType::Return)
            .then(expression.clone().or_not())
            .map(|(return_keyword, to_return)| {
                let pos = combine_code_areas_succeeding(
                    &return_keyword.pos_info,
                    to_return
                        .as_ref()
                        .map(|to_map| to_map.position())
                        .unwrap_or(return_keyword.pos_info()),
                );

                PosInfoWrapper::new(Return::<UntypedAST>::new(to_return), pos)
            });

        let conditional = just_token(TokenType::If)
            .then(expression.clone().delimited_by(
                just_token(TokenType::OpenParen),
                just_token(TokenType::CloseScope),
            ))
            .then_ignore(maybe_statement_separator())
            .then(statement.clone())
            .then(
                maybe_statement_separator()
                    .then_ignore(just_token(TokenType::Else))
                    .then_ignore(maybe_statement_separator())
                    .ignore_then(statement.clone())
                    .or_not(),
            )
            .map(|(((if_keyword, cond), then), else_statement)| {
                let pos = combine_code_areas_succeeding(
                    &if_keyword.pos_info,
                    else_statement
                        .as_ref()
                        .map(|to_map| to_map.position())
                        .unwrap_or(then.position()),
                );

                PosInfoWrapper::new(Conditional::new(cond, then, else_statement), pos)
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
                    .map(|(cond, body)| (body, LoopType::While(cond))),
                statement
                    .clone()
                    .then_ignore(just_token(TokenType::Semicolon))
                    .then(
                        expression
                            .clone()
                            .then_ignore(just_token(TokenType::Semicolon)),
                    )
                    .then(statement.clone())
                    .then_ignore(just_token(TokenType::CloseParen))
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
            .map(|(loop_keyword, (body, loop_type))| {
                let pos = combine_code_areas_succeeding(&loop_keyword.pos_info, body.position());
                PosInfoWrapper::new(Loop::new(body, loop_type), pos)
            });

        let code_block = just_token(TokenType::OpenScope)
            .then(
                statement
                    .clone()
                    .separated_by(statement_separator())
                    .allow_leading()
                    .allow_trailing()
                    .collect::<Vec<ASTNode<Statement<UntypedAST>>>>(),
            )
            .then(just_token(TokenType::CloseScope))
            .map(|((open, block), close)| {
                PosInfoWrapper::new(
                    CodeBlock::new(block.into_iter().collect()),
                    combine_code_areas_succeeding(&open.pos_info, &close.pos_info),
                )
            });

        choice((
            variable_assignment.map(|var_assign| var_assign.map(Statement::VariableAssignment)),
            variable_declaration.map(|var_decl| var_decl.map(Statement::VariableDeclaration)),
            conditional.map(|cond| {
                cond.map(|inner| {
                    Statement::ControlStructure(Box::new(ControlStructure::Conditional(inner)))
                })
            }),
            loop_statement.map(|lst| {
                lst.map(|inner| {
                    Statement::ControlStructure(Box::new(ControlStructure::Loop(inner)))
                })
            }),
            code_block.map(|code_block| code_block.map(Statement::Codeblock)),
            return_statement.map(|return_statement| return_statement.map(Statement::Return)),
            expression.map(|expr| -> PosInfoWrapper<Statement<UntypedAST>> {
                let pos = expr.position().clone();
                PosInfoWrapper::new(Statement::Expression(expr), pos)
            }),
        ))
        .map(|statement| statement.into_ast_node())
    })
}

/** This parses a statement seperator or nothing
*/
fn maybe_statement_separator<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], ()> + Clone {
    just_token(TokenType::StatementSeparator).or_not().ignored()
}
#[cfg(test)]
mod tests {
    use crate::statement::statement_parser;
    use crate::test_shared::wrap_token;
    use ast::statement::Statement;
    use chumsky::Parser;
    use lexer::TokenType;
    use std::ops::Deref;

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
        ]
        .map(wrap_token);

        let parser = statement_parser();

        let parsed = parser.parse(&to_parse).unwrap();

        let expected_var_name = "var";
        let var_name = {
            let Statement::VariableDeclaration(variable) = parsed.deref() else {
                panic!("We should not be here")
            };
            variable.variable().name()
        };
        assert_eq!(expected_var_name, var_name);
    }
}
