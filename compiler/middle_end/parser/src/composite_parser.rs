use crate::function_parser::function_parser;
use crate::misc_parsers::{datatype_parser, identifier_parser, map_visibility, maybe_statement_separator, statement_separator, token_parser, type_parameter_declaration_parser, visibility_parser};
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
    let field = visibility_parser()
        .then(data_type)
        .then(identifier_parser());
    visibility_parser()
        .then(
            token_parser(TokenType::Struct)
                .then(ident)
                .then(type_parameter_declaration_parser())
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
        .map(
            |(visibility, (((((struct_token, name), type_parameters), fields), functions), end))| {
                let fields = fields
                    .into_iter()
                    .map(|((visibility, data_type), name)| {
                        let start = visibility
                            .as_ref()
                            .map(|vis| vis.pos_info())
                            .unwrap_or(data_type.0.pos_info());
                        let pos = combine_code_areas_succeeding(start, &name.pos_info);
                        ASTNode::new(
                            StructField::new(
                                Rc::new(StructFieldSymbol::<UntypedAST>::new(
                                    name.inner,
                                    (data_type.0.inner, data_type.1),
                                )),
                                map_visibility(visibility.as_ref()),
                            ),
                            // This will never panic as data_type is before name
                            pos,
                        )
                    })
                    .collect::<Vec<_>>();

                //This will never panic as the start is before the closing bracket
                let pos = combine_code_areas_succeeding(
                    visibility
                        .as_ref()
                        .map(|vis| vis.pos_info())
                        .unwrap_or(struct_token.pos_info()),
                    &end.pos_info,
                );
                let visibility = map_visibility(visibility.as_ref());
                let symbol = Rc::new(StructSymbol::new(name.inner, type_parameters));
                ASTNode::new(Struct::new(symbol, functions, fields, visibility), pos)
            },
        )
}

/// Parses an enum
pub(crate) fn enum_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], ASTNode<Enum<UntypedAST>>> {
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let variant = ident.clone().then(
        token_parser(TokenType::OpenParen)
            .ignore_then(
                data_type
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .collect::<Vec<_>>()
                    .then(token_parser(TokenType::CloseParen)),
            )
            .or_not(),
    );
    visibility_parser()
        .then(
            token_parser(TokenType::Enum)
                .then(ident)
                .then(type_parameter_declaration_parser())
                .then_ignore(maybe_statement_separator())
                .then_ignore(token_parser(TokenType::OpenScope))
                .then(
                    variant
                        .separated_by(statement_separator())
                        // Don't allow e.g.: `Monday()`
                        .at_least(1)
                        .allow_leading()
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then(token_parser(TokenType::CloseScope)),
        )
        .map(|(visibility, ((((enum_token, name), type_parameters), variants), end))| {
            let variants = variants
                .into_iter()
                .map(|(name, opt_fields)| {
                    let (fields, end_pos) = match opt_fields {
                        Some((f, end)) => (f, end.pos_info),
                        None => (Vec::new(), name.pos_info.clone()),
                    };
                    ASTNode::new(
                        EnumVariant::new(Rc::new(EnumVariantSymbol::<UntypedAST>::new(
                            name.inner,
                            fields.into_iter().map(|field| (field.0.inner, field.1)).collect(),
                        ))),
                        // This will never panic as name is before end
                        combine_code_areas_succeeding(&name.pos_info, &end_pos),
                    )
                })
                .collect::<Vec<_>>();

            //This will never panic as the start is before the closing bracket
            let pos = combine_code_areas_succeeding(
                visibility
                    .as_ref()
                    .map(|vis| vis.pos_info())
                    .unwrap_or(enum_token.pos_info()),
                &end.pos_info,
            );
            let visibility = visibility
                .map(|_| Visibility::Public)
                .unwrap_or(Visibility::Private);
            let symbol = Rc::new(EnumSymbol::new(name.inner, type_parameters));
            ASTNode::new(Enum::new(symbol, variants, visibility), pos)
        })
}
