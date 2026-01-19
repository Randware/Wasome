use crate::function_parser::function_parser;
use crate::misc_parsers::{
    datatype_parser, identifier_parser, maybe_statement_separator, statement_separator,
    token_parser, visibility_parser,
};
use crate::{combine_code_areas_succeeding, PosInfoWrapper};
use ast::composite::{Enum, EnumVariant, Struct, StructField};
use ast::symbol::{EnumSymbol, EnumVariantSymbol, StructFieldSymbol, StructSymbol};
use ast::visibility::Visibility;
use ast::{ASTNode, UntypedAST};
use chumsky::{IterParser, Parser};
use lexer::TokenType;
use std::rc::Rc;

/// Parses a struct
pub(crate) fn struct_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], ASTNode<Struct<UntypedAST>>> {
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let function = function_parser();
    let field = data_type.then(identifier_parser());
    visibility_parser()
        .then(
            token_parser(TokenType::Struct)
                .ignore_then(ident)
                .then_ignore(maybe_statement_separator())
                .then_ignore(token_parser(TokenType::OpenScope))
                .then(
                    field
                        .separated_by(statement_separator())
                        .allow_leading()
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(maybe_statement_separator())
                .then(
                    function
                        .separated_by(statement_separator())
                        .allow_leading()
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then(token_parser(TokenType::CloseScope)),
        )
        .map(|(visibility, (((name, fields), functions), end))| {
            let fields = fields
                .into_iter()
                .map(|(data_type, name)| {
                    ASTNode::new(
                        StructField::new(Rc::new(StructFieldSymbol::<UntypedAST>::new(
                            name.inner,
                            data_type.inner,
                        ))),
                        // This will never panic as data_type is before name
                        combine_code_areas_succeeding(&data_type.pos_info, &name.pos_info),
                    )
                })
                .collect::<Vec<_>>();

            //This will never panic as the start is before the closing bracket
            let pos = combine_code_areas_succeeding(
                visibility
                    .as_ref()
                    .map(|vis| vis.pos_info())
                    .unwrap_or(name.pos_info()),
                &end.pos_info,
            );
            let visibility = visibility
                .map(|_| Visibility::Public)
                .unwrap_or(Visibility::Private);
            let symbol = Rc::new(StructSymbol::new(name.inner));
            ASTNode::new(Struct::new(symbol, functions, fields, visibility), pos)
        })
}

/// Parses an enum
pub(crate) fn enum_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], ASTNode<Enum<UntypedAST>>> {
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let variant = ident.clone().then(
        token_parser(TokenType::OpenParen).ignore_then(
            data_type
                .separated_by(token_parser(TokenType::ArgumentSeparator))
                .collect::<Vec<_>>()
                .then(token_parser(TokenType::CloseParen)),
        ),
    );
    visibility_parser()
        .then(
            token_parser(TokenType::Enum)
                .ignore_then(ident)
                .then_ignore(maybe_statement_separator())
                .then_ignore(token_parser(TokenType::OpenScope))
                .then(
                    variant
                        .separated_by(statement_separator())
                        .allow_leading()
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then(token_parser(TokenType::CloseScope)),
        )
        .map(|(visibility, ((name, variants), end))| {
            let variants = variants
                .into_iter()
                .map(|(name, (fields, end))| {
                    ASTNode::new(
                        EnumVariant::new(Rc::new(EnumVariantSymbol::<UntypedAST>::new(
                            name.inner,
                            fields.into_iter().map(|field| field.inner).collect(),
                        ))),
                        // This will never panic as name is before end
                        combine_code_areas_succeeding(&name.pos_info, &end.pos_info),
                    )
                })
                .collect::<Vec<_>>();

            //This will never panic as the start is before the closing bracket
            let pos = combine_code_areas_succeeding(
                visibility
                    .as_ref()
                    .map(|vis| vis.pos_info())
                    .unwrap_or(name.pos_info()),
                &end.pos_info,
            );
            let visibility = visibility
                .map(|_| Visibility::Public)
                .unwrap_or(Visibility::Private);
            let symbol = Rc::new(EnumSymbol::new(name.inner));
            ASTNode::new(Enum::new(symbol, variants, visibility), pos)
        })
}
