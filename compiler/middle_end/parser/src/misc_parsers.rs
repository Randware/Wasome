use crate::PosInfoWrapper;
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use shared::code_reference::{CodeArea, CodeLocation};

/// Parses data types
pub(crate) fn datatype_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<String>> + Clone {
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
}

/// Parses identifiers
pub(crate) fn identifier_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<String>> + Clone {
    custom(|token| {
        let token: PosInfoWrapper<Token, CodeFile> = token.next().ok_or(EmptyErr::default())?;
        let (next_token, next_pos_info) = (token.inner, token.pos_info);
        match next_token.kind {
            // new only returns None if start > end
            // If this is the case, then there is a bug
            // So the error is unrecoverable
            TokenType::Identifier(inner) => Ok(PosInfoWrapper::new(
                inner,
                CodeArea::new(
                    CodeLocation::new(next_token.line, next_token.span.start),
                    CodeLocation::new(next_token.line, next_token.span.end),
                    next_pos_info,
                )
                .unwrap(),
            )),
            _ => Err(EmptyErr::default()),
        }
    })
}

/// Parses one or multiple statement separators
pub(crate) fn statement_separator<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], ()> + Clone {
    token_parser(TokenType::StatementSeparator)
        .repeated()
        .at_least(1)
        .ignored()
}

/// Parses a single token
pub(crate) fn token_parser<'a>(
    token: TokenType,
) -> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<TokenType, CodeArea>> + Clone
{
    custom(move |tokens| {
        let next: PosInfoWrapper<Token, CodeFile> = tokens.next().ok_or(EmptyErr::default())?;
        let (next_token, next_pos_info) = (next.inner, next.pos_info);
        if token == next_token.kind {
            Ok(PosInfoWrapper::new(
                next_token.kind,
                CodeArea::new(
                    CodeLocation::new(next_token.line, next_token.span.start),
                    CodeLocation::new(next_token.line, next_token.span.end),
                    next_pos_info,
                )
                // new only returns None if start > end
                // If this is the case, then there is a bug
                // So the error is unrecoverable
                .unwrap(),
            ))
        } else {
            Err(EmptyErr::default())
        }
    })
}

/// Parses a single string
pub(crate) fn string_parser<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<String, CodeArea>> + Clone
{
    custom(move |tokens| {
        let next: PosInfoWrapper<Token, CodeFile> = tokens.next().ok_or(EmptyErr::default())?;
        let (next_token, next_pos_info) = (next.inner, next.pos_info);
        if let TokenType::String(inner) = next_token.kind {
            Ok(PosInfoWrapper::new(
                inner,
                CodeArea::new(
                    CodeLocation::new(next_token.line, next_token.span.start),
                    CodeLocation::new(next_token.line, next_token.span.end),
                    next_pos_info,
                )
                // new only returns None if start > end
                // If this is the case, then there is a bug
                // So the error is unrecoverable
                .unwrap(),
            ))
        } else {
            Err(EmptyErr::default())
        }
    })
}
