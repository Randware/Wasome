use crate::{map, ParserSpan};
use ast::data_type::UntypedDataType;
use ast::symbol::UntypedTypeParameterSymbol;
use ast::type_parameter::UntypedTypeParameter;
use chumsky::prelude::*;
use lexer::TokenType;
use std::rc::Rc;
use chumsky::extra::Full;
use chumsky::input::MappedInput;

/// Parses data types
pub(crate) fn datatype_parser<'src>()
-> impl Parser<'src, MappedInput<'src, TokenType, ParserSpan, &'src [Spanned<TokenType, ParserSpan>]>, Spanned<UntypedDataType, ParserSpan>, Full<Rich<'src, TokenType, ParserSpan>, (), ()>> + Clone {
    let identifier_with_type_parameter = identifier_with_type_parameter_parser();
    datatype_parser_internal(identifier_with_type_parameter)
}

fn datatype_parser_internal<'src>(
    identifier_with_type_parameter: impl Parser<
        'src,
        MappedInput<'src, TokenType, ParserSpan, &'src [Spanned<TokenType, ParserSpan>]>,
        (Spanned<String, ParserSpan>, Vec<Spanned<UntypedDataType, ParserSpan>>), Full<Rich<'src, TokenType, ParserSpan>, (), ()>
    > + Clone,
) -> impl Parser<'src, MappedInput<'src, TokenType, ParserSpan, &'src [Spanned<TokenType, ParserSpan>]>, Spanned<UntypedDataType, ParserSpan>, Full<Rich<'src, TokenType, ParserSpan>, (), ()>> + Clone {
    choice((
        token_parser(TokenType::F32).map(|to_map| map(to_map, |_| "f32".to_string())),
        token_parser(TokenType::F64).map(|to_map| map(to_map, |_| "f64".to_string())),
        token_parser(TokenType::S8).map(|to_map| map(to_map, |_| "s8".to_string())),
        token_parser(TokenType::U8).map(|to_map| map(to_map, |_| "u8".to_string())),
        token_parser(TokenType::S16).map(|to_map| map(to_map, |_| "s16".to_string())),
        token_parser(TokenType::U16).map(|to_map| map(to_map, |_| "u16".to_string())),
        token_parser(TokenType::S32).map(|to_map| map(to_map, |_| "s32".to_string())),
        token_parser(TokenType::U32).map(|to_map| map(to_map, |_| "u21".to_string())),
        token_parser(TokenType::S64).map(|to_map| map(to_map, |_| "s64".to_string())),
        token_parser(TokenType::U64).map(|to_map| map(to_map, |_| "u64".to_string())),
        token_parser(TokenType::Bool).map(|to_map| map(to_map, |_| "bool".to_string())),
        token_parser(TokenType::Char).map(|to_map| map(to_map, |_| "char".to_string())),
    ))
    .map(|dt| map(dt, |inner| UntypedDataType::new(inner, Vec::new())))
    .or(identifier_with_type_parameter.map(|(ident, type_params)| {
        let pos = ident
            .span
            .merge(
                type_params
                    .last()
                    .map(|last| last.span)
                    .unwrap_or(ident.span),
            )
            .unwrap();
        Spanned {
            inner: UntypedDataType::new(
                ident.inner,
                type_params
                    .into_iter()
                    .map(|param| param.inner)
                    .collect::<Vec<_>>(),
            ),
            span: pos,
        }
    }))
}

/// Parses identifiers with possible type parameters
pub(crate) fn identifier_with_type_parameter_parser<'src>() -> impl Parser<
    'src,
    MappedInput<'src, TokenType, ParserSpan, &'src [Spanned<TokenType, ParserSpan>]>,
    (Spanned<String, ParserSpan>, Vec<Spanned<UntypedDataType, ParserSpan>>), Full<Rich<'src, TokenType, ParserSpan>, (), ()>
> + Clone {
    let type_parameter_usage = type_parameter_usage_parser().boxed();
    identifier_with_type_parameter_parser_internal(type_parameter_usage)
}

fn identifier_with_type_parameter_parser_internal<'src>(
    type_parameter_usage: impl Parser<
        'src,
        MappedInput<'src, TokenType, ParserSpan, &'src [Spanned<TokenType, ParserSpan>]>,
        Vec<Spanned<UntypedDataType, ParserSpan>>,
        Full<Rich<'src, TokenType, ParserSpan>, (), ()>
    > + Clone,
) -> impl Parser<
    'src,
    MappedInput<'src, TokenType, ParserSpan, &'src [Spanned<TokenType, ParserSpan>]>,
    (Spanned<String, ParserSpan>, Vec<Spanned<UntypedDataType, ParserSpan>>), Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
> + Clone {
    cross_module_capable_identifier_parser().then(type_parameter_usage)
}

/// Parses identifiers
pub(crate) fn identifier_parser<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Spanned<String, ParserSpan>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    select_ref! { TokenType::Identifier(x) => x.to_string() }
        .or(select_ref! { TokenType::SelfType => "self".to_string() }).spanned()
}

/// Parses cross-module-capable identifiers
///
/// A cross-module identifier consists of the following format:
///
/// `<Identifier>.<Identifier>`
///
/// They are used when the identified element was imported.
///
/// This can be either a cross-module identifier or a regular one.
///
pub(crate) fn cross_module_capable_identifier_parser<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Spanned<String, ParserSpan>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    identifier_parser()
        .then(
            token_parser(TokenType::Dot)
                .then(identifier_parser())
                .or_not(),
        )
        .map(|(lhs, rhs)| match rhs {
            None => lhs,
            Some(inner) => {
                Spanned {
                    inner: format!("{}.{}", lhs.inner, inner.1.inner),
                    span: lhs.span.merge(inner.1.span).unwrap(),
                }
            },
        })
}

/// Parses one or multiple statement separators
pub(crate) fn statement_separator<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, (), Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    token_parser(TokenType::StatementSeparator)
        .repeated()
        .at_least(1)
        .ignored()
}

/// Either parses a statementSeparator or nothing
pub(crate) fn maybe_statement_separator<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, (), Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    token_parser(TokenType::StatementSeparator)
        .or_not()
        .ignored()
}

/// Parses a single token
pub(crate) fn token_parser<'a>(
    token: TokenType,
) -> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Spanned<TokenType, ParserSpan>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    just(token).spanned()
}

/// Parses a single string
pub(crate) fn string_parser<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Spanned<String, ParserSpan>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    select_ref! { TokenType::String(x) => x.to_string() }.spanned()
}

pub(crate) fn visibility_parser<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Option<Spanned<TokenType, ParserSpan>>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    token_parser(TokenType::Public).or_not()
}

/// Parses type parameters on functions, structs and enums
///
/// Also allows no type parameters to be present
pub(crate) fn type_parameter_declaration_parser<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Vec<Spanned<UntypedTypeParameter, ParserSpan>>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone
{
    identifier_parser()
        .separated_by(token_parser(TokenType::ArgumentSeparator))
        .at_least(1)
        .collect::<Vec<_>>()
        .delimited_by(
            token_parser(TokenType::LessThan),
            token_parser(TokenType::GreaterThan),
        )
        .or_not()
        .map(|parameters| {
            parameters
                .map(|parameters| {
                    parameters
                        .into_iter()
                        .map(|parameter| {
                            map(parameter, (|inner| {
                                UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new(
                                    inner,
                                )))
                            }))
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or(Vec::new())
        })
}

/// Parses a single type parameter usage
pub(crate) fn type_parameter_usage_parser<'a>()
-> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Vec<Spanned<UntypedDataType, ParserSpan>>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone {
    // Allow indirect recursion as datatype_parser calls type_parameter_usage_parser through even more indirection
    let mut identifier_with_type = Recursive::declare();
    let mut datatype = Recursive::declare();
    datatype.define(datatype_parser_internal(identifier_with_type.clone()));

    let type_parameter_usage = type_parameter_usage_parser_internal(datatype);
    identifier_with_type.define(identifier_with_type_parameter_parser_internal(
        type_parameter_usage.clone(),
    ));
    type_parameter_usage
}

fn type_parameter_usage_parser_internal<'a>(
    datatype: impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Spanned<UntypedDataType, ParserSpan>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone,
) -> impl Parser<'a, MappedInput<'a, TokenType, ParserSpan, &'a [Spanned<TokenType, ParserSpan>]>, Vec<Spanned<UntypedDataType, ParserSpan>>, Full<Rich<'a, TokenType, ParserSpan>, (), ()>> + Clone
{
    datatype
        .separated_by(token_parser(TokenType::ArgumentSeparator))
        .at_least(1)
        .collect::<Vec<_>>()
        .delimited_by(
            token_parser(TokenType::LessThan),
            token_parser(TokenType::GreaterThan),
        )
        .or_not()
        .map(|parameters| parameters.unwrap_or(Vec::new()))
}

#[cfg(test)]
mod tests {
    use crate::misc_parsers::datatype_parser;
    use crate::test_shared::wrap_token;
    use ast::SemanticEq;
    use ast::data_type::UntypedDataType;
    use chumsky::Parser;
    use lexer::TokenType;
    use crate::convert_nonempty_input;

    #[test]
    fn parse_nested_datatype() {
        let tokens = vec![
            wrap_token(TokenType::Identifier("Box".to_string())),
            wrap_token(TokenType::LessThan),
            wrap_token(TokenType::Identifier("Option".to_string())),
            wrap_token(TokenType::LessThan),
            wrap_token(TokenType::U32),
            wrap_token(TokenType::GreaterThan),
            wrap_token(TokenType::GreaterThan),
        ];
        let res = datatype_parser().parse(convert_nonempty_input(&tokens)).unwrap();
        assert!(res.inner.semantic_eq(&UntypedDataType::new(
            "Box".to_string(),
            vec![UntypedDataType::new(
                "Option".to_string(),
                vec![UntypedDataType::new("u32".to_string(), vec![])]
            )]
        )))
    }
}
