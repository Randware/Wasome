use crate::{combine_code_areas_succeeding, PosInfoWrapper};
use chumsky::prelude::*;
use ast::visibility::Visibility;
use lexer::TokenType;

/// Parses data types
pub(crate) fn datatype_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], PosInfoWrapper<String>> + Clone {
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
        cross_module_capable_identifier_parser(),
    ))
}

/// Parses identifiers
pub(crate) fn identifier_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<TokenType>], PosInfoWrapper<String>> + Clone {
    custom(|token| {
        let token: PosInfoWrapper<TokenType> = token.next().ok_or(EmptyErr::default())?;
        let (next_token, next_pos_info) = (token.inner, token.pos_info);
        match next_token {
            TokenType::Identifier(inner) => Ok(PosInfoWrapper::new(inner, next_pos_info)),
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

// Technically not a parser, but there is no better place for this
pub(crate) fn map_visibility(visibility: Option<&PosInfoWrapper<TokenType>>) -> Visibility {
    visibility
        .map(|_| Visibility::Public)
        .unwrap_or(Visibility::Private)
}
