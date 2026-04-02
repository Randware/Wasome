use crate::error::ParserError;
use crate::input::ParserInput;
use crate::misc_parsers::{
    datatype_parser, identifier_parser, token_parser, type_parameter_declaration_parser,
    visibility_parser,
};
use crate::statement_parser::statement_parser;
use crate::{ParserSpan, map_visibility, unspan_vec};
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::{Function, FunctionType};
use ast::{ASTNode, UntypedAST};
use chumsky::IterParser;
use chumsky::Parser;
use chumsky::extra::Full;
use chumsky::span::{Spanned, WrappingSpan};
use lexer::TokenType;
use std::rc::Rc;

/// Parses a single function
// This is a purely functional parser
// It being long does not really increase complexity
#[allow(clippy::too_many_lines)]
pub fn function_parser<'src>()
-> impl Parser<'src, ParserInput<'src>, ASTNode<Function<UntypedAST>>, Full<ParserError, (), ()>> {
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

    // While there is some code duplication, forcing an abstracting that might fail in the future
    // seems like a bad idea
    // No abstraction > bad abstraction
    let external = visibility_parser()
        .then_ignore(token_parser(TokenType::Extern))
        .then(
            token_parser(TokenType::Function)
                .ignore_then(ident.clone())
                .then(type_parameter_declaration_parser())
                .then_ignore(token_parser(TokenType::OpenParen))
                .then(
                    param
                        .clone()
                        .separated_by(token_parser(TokenType::ArgumentSeparator))
                        .collect::<Vec<Spanned<Rc<VariableSymbol<UntypedAST>>, ParserSpan>>>()
                        .then(token_parser(TokenType::CloseParen)),
                ),
        )
        .then(
            token_parser(TokenType::Return)
                .ignore_then(data_type.clone())
                .or_not(),
        )
        .map(
            |((visibility, ((name, type_parameters), (params, params_close))), return_type)| {
                let pos = visibility
                    .as_ref()
                    .map_or(name.span, |vis| vis.span)
                    .merge(return_type.as_ref().map_or(params_close.span, |dt| dt.span))
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
                        FunctionType::External,
                        visibility,
                    ),
                    pos,
                )
            },
        )
        .boxed();

    let regular = visibility_parser()
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
                    .map_or(name.span, |vis| vis.span)
                    .merge((*implementation.position()).into())
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
                        FunctionType::Regular(implementation),
                        visibility,
                    ),
                    pos,
                )
            },
        )
        .boxed();
    external.or(regular)
}
