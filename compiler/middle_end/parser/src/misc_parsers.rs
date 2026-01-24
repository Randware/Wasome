use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::data_type::UntypedDataType;
use ast::symbol::UntypedTypeParameterSymbol;
use ast::type_parameter::UntypedTypeParameter;
use chumsky::prelude::*;
use lexer::TokenType;
use std::fmt::Debug;
use std::rc::Rc;

/// Parses data types
pub(crate) fn datatype_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], PosInfoWrapper<UntypedDataType>> + Clone {
    let identifier_with_type_parameter = identifier_with_type_parameter_parser();
    datatype_parser_internal(identifier_with_type_parameter)
}

fn datatype_parser_internal<'src>(
    identifier_with_type_parameter: impl Parser<
        'src,
        &'src [PosInfoWrapper<TokenType>],
        (PosInfoWrapper<String>, Vec<PosInfoWrapper<UntypedDataType>>),
    > + Clone,
) -> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], PosInfoWrapper<UntypedDataType>> + Clone {
    choice((
        token_parser(TokenType::F32).map(|to_map| to_map.map(|_| "f32".to_string())),
        token_parser(TokenType::F64).map(|to_map| to_map.map(|_| "f64".to_string())),
        token_parser(TokenType::S8).map(|to_map| to_map.map(|_| "s8".to_string())),
        token_parser(TokenType::U8).map(|to_map| to_map.map(|_| "u8".to_string())),
        token_parser(TokenType::S16).map(|to_map| to_map.map(|_| "s16".to_string())),
        token_parser(TokenType::U16).map(|to_map| to_map.map(|_| "u16".to_string())),
        token_parser(TokenType::S32).map(|to_map| to_map.map(|_| "s32".to_string())),
        token_parser(TokenType::U32).map(|to_map| to_map.map(|_| "u32".to_string())),
        token_parser(TokenType::S64).map(|to_map| to_map.map(|_| "s64".to_string())),
        token_parser(TokenType::U64).map(|to_map| to_map.map(|_| "u64".to_string())),
        token_parser(TokenType::Bool).map(|to_map| to_map.map(|_| "bool".to_string())),
        token_parser(TokenType::Char).map(|to_map| to_map.map(|_| "char".to_string())),
    ))
    .map(|dt| dt.map(|inner| UntypedDataType::new(inner, Vec::new())))
    .or(identifier_with_type_parameter.map(|(ident, type_params)| {
        let pos = combine_code_areas_succeeding(
            &ident.pos_info,
            type_params
                .last()
                .map(|last| last.pos_info())
                .unwrap_or(&ident.pos_info),
        );
        PosInfoWrapper::new(
            UntypedDataType::new(
                ident.inner,
                type_params
                    .into_iter()
                    .map(|param| param.inner)
                    .collect::<Vec<_>>(),
            ),
            pos,
        )
    }))
}

/// Parses identifiers with possible type parameters
pub(crate) fn identifier_with_type_parameter_parser<'src>() -> impl Parser<
    'src,
    &'src [PosInfoWrapper<TokenType>],
    (PosInfoWrapper<String>, Vec<PosInfoWrapper<UntypedDataType>>),
> + Clone {
    let type_parameter_usage = type_parameter_usage_parser();
    identifier_with_type_parameter_parser_internal(type_parameter_usage)
}

fn identifier_with_type_parameter_parser_internal<'src>(
    type_parameter_usage: impl Parser<
        'src,
        &'src [PosInfoWrapper<TokenType>],
        Vec<PosInfoWrapper<UntypedDataType>>,
    > + Clone,
) -> impl Parser<
    'src,
    &'src [PosInfoWrapper<TokenType>],
    (PosInfoWrapper<String>, Vec<PosInfoWrapper<UntypedDataType>>),
> + Clone {
    cross_module_capable_identifier_parser().then(type_parameter_usage)
}

/// Parses identifiers
pub(crate) fn identifier_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], PosInfoWrapper<String>> + Clone {
    custom(|token| {
        let token: PosInfoWrapper<TokenType> = token.next().ok_or(EmptyErr::default())?;
        let (next_token, next_pos_info) = (token.inner, token.pos_info);
        match next_token {
            TokenType::Identifier(inner) => Ok(PosInfoWrapper::new(inner, next_pos_info)),
            TokenType::SelfType => Ok(PosInfoWrapper::new("self".to_string(), next_pos_info)),
            _ => Err(EmptyErr::default()),
        }
    })
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
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], PosInfoWrapper<String>> + Clone {
    identifier_parser()
        .then(
            token_parser(TokenType::Dot)
                .then(identifier_parser())
                .or_not(),
        )
        .map(|(lhs, rhs)| match rhs {
            None => lhs,
            Some(inner) => PosInfoWrapper::new(
                format!("{}.{}", lhs.inner, inner.1.inner),
                combine_code_areas_succeeding(&lhs.pos_info, &inner.1.pos_info),
            ),
        })
}

/// Parses one or multiple statement separators
pub(crate) fn statement_separator<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], ()> + Clone {
    token_parser(TokenType::StatementSeparator)
        .repeated()
        .at_least(1)
        .ignored()
}

/// Either parses a statementSeparator or nothing
pub(crate) fn maybe_statement_separator<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], ()> + Clone {
    token_parser(TokenType::StatementSeparator)
        .or_not()
        .ignored()
}

/// Parses a single token
pub(crate) fn token_parser<'a>(
    token: TokenType,
) -> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], PosInfoWrapper<TokenType>> + Clone {
    custom(move |tokens| {
        let next: PosInfoWrapper<TokenType> = tokens.next().ok_or(EmptyErr::default())?;
        let (next_token, next_pos_info) = (next.inner, next.pos_info);
        if token == next_token {
            Ok(PosInfoWrapper::new(next_token, next_pos_info))
        } else {
            Err(EmptyErr::default())
        }
    })
}

/// Parses a single string
pub(crate) fn string_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], PosInfoWrapper<String>> + Clone {
    custom(move |tokens| {
        let next: PosInfoWrapper<TokenType> = tokens.next().ok_or(EmptyErr::default())?;
        let (next_token, next_pos_info) = (next.inner, next.pos_info);
        if let TokenType::String(inner) = next_token {
            Ok(PosInfoWrapper::new(inner, next_pos_info))
        } else {
            Err(EmptyErr::default())
        }
    })
}

pub(crate) fn visibility_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], Option<PosInfoWrapper<TokenType>>> + Clone {
    token_parser(TokenType::Public).or_not()
}

/// Parses type parameters on functions, structs and enums
///
/// Also allows no type parameters to be present
pub(crate) fn type_parameter_declaration_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], Vec<PosInfoWrapper<UntypedTypeParameter>>> + Clone
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
                            parameter.map(|inner| {
                                UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new(
                                    inner,
                                )))
                            })
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or(Vec::new())
        })
}

/// Parses a single type parameter usage
pub(crate) fn type_parameter_usage_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], Vec<PosInfoWrapper<UntypedDataType>>> + Clone {
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
    datatype: impl Parser<'a, &'a [PosInfoWrapper<TokenType>], PosInfoWrapper<UntypedDataType>> + Clone,
) -> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], Vec<PosInfoWrapper<UntypedDataType>>> + Clone
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
    use chumsky::Parser;
    use lexer::TokenType;

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
        let res = datatype_parser().parse(&tokens).unwrap();
        dbg!(res);
    }
}
