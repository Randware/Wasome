use crate::expression_parser::expression_parser;
use crate::input::ParserInput;
use crate::misc_parsers::{
    datatype_parser, identifier_parser, identifier_with_type_parameter_parser,
    maybe_statement_separator, statement_separator, token_parser,
};
use crate::{ParserSpan, map, unspan_vec};
use ast::statement::{
    CodeBlock, Conditional, ControlStructure, IfEnumVariant, Loop, LoopType, Return, Statement,
    StructFieldAssignment, VariableAssignment, VariableDeclaration,
};
use ast::symbol::VariableSymbol;
use ast::{ASTNode, UntypedAST};
use chumsky::extra::Full;
use chumsky::prelude::*;
use chumsky::span::WrappingSpan;
use lexer::TokenType;
use std::rc::Rc;

/// Ensures that T implements a specific trait
///
/// This is only used to prevent type annotation issues without specifying the entire type
fn narrow<
    'src,
    T: Parser<
            'src,
            ParserInput<'src>,
            ASTNode<Statement<UntypedAST>>,
            Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
        > + Clone,
>(
    input: T,
) -> T {
    input
}

/// Parses a single statement
pub(crate) fn statement_parser<'src>() -> impl Parser<
    'src,
    ParserInput<'src>,
    ASTNode<Statement<UntypedAST>>,
    Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
> {
    recursive(|statement| {
        let statement = narrow(statement);
        let data_type = datatype_parser();
        let ident = identifier_parser();

        let expression = expression_parser();

        let variable_assignment = ident
            .clone()
            .then_ignore(token_parser(TokenType::Assign))
            .then(expression.clone())
            .map(|(name, val)| {
                let pos = name.span.merge(val.position().clone().into()).unwrap();
                pos.make_wrapped(VariableAssignment::<UntypedAST>::new(name.inner, val))
            });

        let not_assign = token_parser(TokenType::Assign).not();
        let not_dot = token_parser(TokenType::Dot).not();
        let not_assign_token = any().spanned().and_is(not_assign.clone());

        let struct_field_assignment =
        // TODO:
        // Improvement: Remove the collect
        expression_parser().nested_in(
            not_assign_token
                .clone()
                .then_ignore(
                    // Don't consume the struct field in the expression
                    not_assign_token
                        .clone()
                        .and_is(not_dot)
                        .or_not()
                        .then(token_parser(TokenType::Dot))
                        .rewind(),
                )
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .to_slice())
            .then_ignore(token_parser(TokenType::Dot))
            .then(
                identifier_parser().nested_in(
                    not_assign_token
                        .repeated()
                        .at_least(1)
                        .collect::<Vec<_>>()
                        .to_slice()
                )
            )
            .then_ignore(token_parser(TokenType::Assign))
            .then(expression.clone())
            .map(|((src, field), val)| {
                let pos: ParserSpan = src.position().merge(*val.position()).unwrap().clone().into();
                pos.make_wrapped(
                    StructFieldAssignment::<UntypedAST>::new(src, field.inner, val)
                )
            });

        let variable_declaration = data_type
            .clone()
            .then(ident.clone())
            .then_ignore(token_parser(TokenType::Assign))
            .then(expression.clone())
            .map(|((data_type, name), val)| {
                data_type
                    .span
                    .merge(name.span)
                    .unwrap()
                    .make_wrapped(VariableDeclaration::<UntypedAST>::new(
                        Rc::new(VariableSymbol::new(name.inner, data_type.inner)),
                        val,
                    ))
            });

        let return_statement = token_parser(TokenType::Return)
            .then(expression.clone().or_not())
            .map(|(return_keyword, to_return)| {
                let pos = return_keyword
                    .span
                    .merge(
                        to_return
                            .as_ref()
                            .map(|to_map| to_map.position().clone().into())
                            .unwrap_or(return_keyword.span),
                    )
                    .unwrap();
                pos.make_wrapped(Return::<UntypedAST>::new(to_return))
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
                let pos = if_keyword
                    .span
                    .merge(
                        else_statement
                            .as_ref()
                            .map(|to_map| to_map.position())
                            .unwrap_or(then.position())
                            .clone()
                            .into(),
                    )
                    .unwrap();
                pos.make_wrapped(Conditional::new(cond, then, else_statement))
            });

        let if_enum_variant = token_parser(TokenType::If)
            .then(
                token_parser(TokenType::Let)
                    .ignored()
                    .then(identifier_with_type_parameter_parser())
                    .then_ignore(token_parser(TokenType::PathSeparator))
                    .then(ident.clone())
                    .then(
                        data_type
                            .clone()
                            .then(ident)
                            .separated_by(token_parser(TokenType::ArgumentSeparator))
                            .collect::<Vec<_>>()
                            .delimited_by(
                                token_parser(TokenType::OpenParen),
                                token_parser(TokenType::CloseParen),
                            ),
                    )
                    .then_ignore(token_parser(TokenType::Assign))
                    .then(expression.clone())
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    ),
            )
            .then_ignore(maybe_statement_separator())
            .then(statement.clone())
            .map(
                |(
                    (if_keyword, ((((_, enum_identifier), enum_variant), vars), source)),
                    then_statement,
                )| {
                    let pos = if_keyword
                        .span
                        .merge(then_statement.position().clone().into())
                        .unwrap();

                    let vars = vars
                        .into_iter()
                        .map(|var| {
                            Rc::new(VariableSymbol::<UntypedAST>::new(var.1.inner, var.0.inner))
                        })
                        .collect::<Vec<_>>();

                    pos.make_wrapped(IfEnumVariant::<UntypedAST>::new(
                        (enum_identifier.0.inner, unspan_vec(enum_identifier.1)),
                        enum_variant.inner,
                        source,
                        vars,
                        then_statement,
                    ))
                },
            );

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
                    .map(|((init, cond), after_each)| LoopType::For {
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
                    .map(LoopType::While),
                // The infinite loop has to be at the bottom to not "steal" from the other types
                empty().map(|_| LoopType::Infinite),
            )))
            .then(loop_body.clone())
            .map(|((loop_keyword, loop_type), body)| {
                let pos = loop_keyword
                    .span
                    .merge(body.position().clone().into())
                    .unwrap();
                pos.make_wrapped(Loop::new(body, loop_type))
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
                open.span
                    .merge(close.span)
                    .unwrap()
                    .make_wrapped(CodeBlock::new(block.into_iter().collect()))
            });
        choice((
            variable_assignment.map(|var_assign| map(var_assign, Statement::VariableAssignment)),
            struct_field_assignment
                .map(|str_assign| map(str_assign, Statement::StructFieldAssignment)),
            variable_declaration.map(|var_decl| map(var_decl, Statement::VariableDeclaration)),
            conditional.map(|cond| {
                map(cond, |inner| {
                    Statement::ControlStructure(Box::new(ControlStructure::Conditional(inner)))
                })
            }),
            loop_statement.map(|lst| {
                map(lst, |inner| {
                    Statement::ControlStructure(Box::new(ControlStructure::Loop(inner)))
                })
            }),
            if_enum_variant.map(|iev| {
                map(iev, |inner| {
                    Statement::ControlStructure(Box::new(ControlStructure::IfEnumVariant(inner)))
                })
            }),
            code_block.map(|code_block| map(code_block, Statement::Codeblock)),
            return_statement.map(|return_statement| map(return_statement, Statement::Return)),
            expression.map(|expr| -> Spanned<Statement<UntypedAST>, ParserSpan> {
                let pos: ParserSpan = expr.position().clone().into();
                pos.make_wrapped(Statement::Expression(expr))
            }),
        ))
        .map(|statement| ASTNode::new(statement.inner, statement.span.into()))
    })
}

#[cfg(test)]
mod tests {
    use crate::statement_parser::statement_parser;
    use crate::test_shared::{convert_nonempty_input, wrap_in_ast_node, wrap_token};
    use ast::data_type::UntypedDataType;
    use ast::expression::{
        BinaryOp, BinaryOpType, Expression, FunctionCall, Typecast, UnaryOp, UnaryOpType,
    };
    use ast::statement::{Statement, VariableDeclaration};
    use ast::symbol::VariableSymbol;
    use ast::{SemanticEq, UntypedAST};
    use chumsky::Parser;
    use lexer::TokenType;
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

        let parsed = parser.parse(convert_nonempty_input(&to_parse)).unwrap();

        let symbol = Rc::new(VariableSymbol::new(
            "var".to_string(),
            UntypedDataType::new("bool".to_string(), Vec::new()),
        ));
        let expected = wrap_in_ast_node(Statement::VariableDeclaration(VariableDeclaration::<
            UntypedAST,
        >::new(
            symbol,
            wrap_in_ast_node(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
                ("test".to_string(), Vec::new()),
                vec![
                    wrap_in_ast_node(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                            "f32".to_string(),
                            Vec::new(),
                        ))),
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
