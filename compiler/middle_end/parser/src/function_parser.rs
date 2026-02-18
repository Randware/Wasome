use crate::input::ParserInput;
use crate::misc_parsers::{
    datatype_parser, identifier_parser, token_parser, type_parameter_declaration_parser,
    visibility_parser,
};
use crate::statement_parser::statement_parser;
use crate::{ParserSpan, map_visibility, unspan_vec};
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::Function;
use ast::{ASTNode, UntypedAST};
use chumsky::IterParser;
use chumsky::Parser;
use chumsky::error::Rich;
use chumsky::extra::Full;
use chumsky::span::{Spanned, WrappingSpan};
use lexer::TokenType;
use std::rc::Rc;

/// Parses a single function
pub(crate) fn function_parser<'src>() -> impl Parser<
    'src,
    ParserInput<'src>,
    ASTNode<Function<UntypedAST>>,
    Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
> {
    let statement = statement_parser();
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let param = data_type
        .clone()
        .then(ident.clone())
        .map(|(data_type, name)| {
            data_type
                .span
                .merge(name.span)
                .unwrap()
                .make_wrapped(Rc::new(VariableSymbol::new(name.inner, data_type.inner)))
        });

    visibility_parser()
        .then(
            token_parser(TokenType::Function)
                .ignore_then(ident)
                .then(type_parameter_declaration_parser())
                .then(
                    param
                        .clone()
                        .separated_by(token_parser(TokenType::ArgumentSeparator))
                        .collect::<Vec<Spanned<Rc<VariableSymbol<UntypedAST>>, ParserSpan>>>()
                        .delimited_by(
                            token_parser(TokenType::OpenParen),
                            token_parser(TokenType::CloseParen),
                        ),
                ),
        )
        .then(
            token_parser(TokenType::Return)
                .ignore_then(data_type)
                .or_not(),
        )
        .then(statement)
        .map(
            |(((visibility, ((name, type_parameters), params)), return_type), implementation)| {
                let pos = visibility
                    .as_ref()
                    .map(|vis| vis.span)
                    .unwrap_or(name.span)
                    .merge(implementation.position().clone().into())
                    .unwrap()
                    .into();
                let visibility = map_visibility(visibility.as_ref());
                ASTNode::new(
                    Function::new(
                        Rc::new(FunctionSymbol::new(
                            name.inner,
                            return_type.map(|to_map| to_map.inner),
                            params.into_iter().map(|param| param.inner).collect(),
                            unspan_vec(type_parameters),
                        )),
                        implementation,
                        visibility,
                    ),
                    pos,
                )
            },
        )
        .boxed()
}
