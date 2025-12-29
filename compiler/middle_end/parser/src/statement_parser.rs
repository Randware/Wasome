use crate::expression_parser::expression_parser;
use crate::misc_parsers::{
    cross_module_capable_identifier_parser, datatype_parser, identifier_parser,
    maybe_statement_separator, statement_separator, token_parser,
};
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::expression::{Expression, FunctionCall};
use ast::statement::{
    CodeBlock, Conditional, ControlStructure, Loop, LoopType, Return, Statement,
    VariableAssignment, VariableDeclaration,
};
use ast::symbol::VariableSymbol;
use ast::{ASTNode, UntypedAST};
use chumsky::prelude::*;
use lexer::TokenType;
use std::rc::Rc;

/// Ensures that T implements a specific trait
///
/// This is only used to prevent type annotation issues without specifying the entire type
fn narrow<
    'src,
    T: Parser<'src, &'src [PosInfoWrapper<TokenType>], ASTNode<Statement<UntypedAST>>> + Clone,
>(
    input: T,
) -> T {
    input
}

/// Parses a single statement
pub(crate) fn statement_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], ASTNode<Statement<UntypedAST>>> {
    recursive(|statement| {
        let statement = narrow(statement);
        let data_type = datatype_parser();
        let ident = identifier_parser();

        let expression = expression_parser();

        let call = cross_module_capable_identifier_parser()
            .clone()
            .then(
                expression
                    .clone()
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
                PosInfoWrapper::new(FunctionCall::<UntypedAST>::new(name.inner, args), pos)
            });

        let variable_assignment = ident
            .clone()
            .then_ignore(token_parser(TokenType::Assign))
            .then(expression.clone())
            .map(|(name, val)| {
                let pos = combine_code_areas_succeeding(&name.pos_info, val.position());
                PosInfoWrapper::new(VariableAssignment::<UntypedAST>::new(name.inner, val), pos)
            });

        let variable_declaration = data_type
            .clone()
            .then(ident)
            .then_ignore(token_parser(TokenType::Assign))
            .then(expression.clone())
            .map(|((data_type, name), val)| {
                PosInfoWrapper::new(
                    VariableDeclaration::<UntypedAST>::new(
                        Rc::new(VariableSymbol::new(name.inner, data_type.inner)),
                        val,
                    ),
                    combine_code_areas_succeeding(&data_type.pos_info, &name.pos_info),
                )
            });

        let return_statement = token_parser(TokenType::Return)
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

        let conditional = token_parser(TokenType::If)
            .then(expression.clone().delimited_by(
                token_parser(TokenType::OpenParen),
                token_parser(TokenType::CloseParen),
            ))
            .then_ignore(maybe_statement_separator())
            .then(statement.clone())
            .then(
                maybe_statement_separator()
                    .then_ignore(token_parser(TokenType::Else))
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
        let loop_statement = token_parser(TokenType::Loop)
            .then(choice((
                statement
                    .clone()
                    .then_ignore(token_parser(TokenType::Semicolon))
                    .then(
                        expression
                            .clone()
                            .then_ignore(token_parser(TokenType::Semicolon)),
                    )
                    .then(statement.clone())
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    )
                    .map(|((init, cond), after_each)|
                            LoopType::For {
                                start: init,
                                cond,
                                after_each,
                    }),
                expression
                    .clone()
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    )
                    .map(|cond| LoopType::While(cond)),
                // The infinite loop has to be at the bottom to not "steal" from the other types
                empty().map(|_| LoopType::Infinite),
            )))
            .then(loop_body.clone())
            .map(|((loop_keyword, loop_type), body)| {
                let pos = combine_code_areas_succeeding(&loop_keyword.pos_info, body.position());
                PosInfoWrapper::new(Loop::new(body, loop_type), pos)
            });

        let code_block = token_parser(TokenType::OpenScope)
            .then(
                statement
                    .clone()
                    .separated_by(statement_separator())
                    .allow_leading()
                    .allow_trailing()
                    .collect::<Vec<ASTNode<Statement<UntypedAST>>>>(),
            )
            .then(token_parser(TokenType::CloseScope))
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
            call.map(|var_assign| var_assign.map(Statement::VoidFunctionCall)),
        ))
        .map(|statement| statement.into_ast_node())
    })
}

#[cfg(test)]
mod tests {
    use crate::statement_parser::statement_parser;
    use crate::test_shared::{wrap_in_ast_node, wrap_token};
    use ast::expression::{
        BinaryOp, BinaryOpType, Expression, FunctionCall, Literal, Typecast, UnaryOp, UnaryOpType,
    };
    use ast::statement::{Statement, VariableDeclaration};
    use ast::symbol::VariableSymbol;
    use ast::{ASTNode, SemanticEq, TypedAST, UntypedAST};
    use chumsky::Parser;
    use lexer::TokenType;
    use std::ops::Deref;
    use std::rc::Rc;

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

        let symbol = Rc::new(VariableSymbol::new("var".to_string(), "bool".to_string()));
        let expected = wrap_in_ast_node(Statement::VariableDeclaration(VariableDeclaration::<
            UntypedAST,
        >::new(
            symbol,
            wrap_in_ast_node(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
                "test".to_string(),
                vec![
                    wrap_in_ast_node(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                        wrap_in_ast_node(Expression::Literal("5".to_string())),
                    )))),
                    wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::NotEquals,
                        wrap_in_ast_node(Expression::Variable("test2".to_string())),
                        wrap_in_ast_node(Expression::BinaryOp(Box::new(
                            BinaryOp::<UntypedAST>::new(
                                BinaryOpType::Multiplication,
                                wrap_in_ast_node(Expression::Literal("5.0".to_string())),
                                wrap_in_ast_node(Expression::Literal("10.0".to_string())),
                            ),
                        ))),
                    )))),
                ],
            ))),
        )));
        assert!(parsed.semantic_eq(&expected));
    }
}
