use crate::PosInfoWrapper;
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use shared::code_reference::{CodeArea, CodeLocation};

/** This parses arbitiary data types
*/
pub(crate) fn datatype_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<String>> + Clone {
    choice((
        just_token(TokenType::F32).map(|to_map| to_map.map(|_| "f32".to_string())),
        just_token(TokenType::F64).map(|to_map| to_map.map(|_| "f64".to_string())),
        just_token(TokenType::S8).map(|to_map| to_map.map(|_| "s8".to_string())),
        just_token(TokenType::U8).map(|to_map| to_map.map(|_| "u8".to_string())),
        just_token(TokenType::S16).map(|to_map| to_map.map(|_| "s16".to_string())),
        just_token(TokenType::U16).map(|to_map| to_map.map(|_| "u16".to_string())),
        just_token(TokenType::S32).map(|to_map| to_map.map(|_| "s32".to_string())),
        just_token(TokenType::U32).map(|to_map| to_map.map(|_| "u32".to_string())),
        just_token(TokenType::S64).map(|to_map| to_map.map(|_| "s64".to_string())),
        just_token(TokenType::U64).map(|to_map| to_map.map(|_| "u64".to_string())),
        just_token(TokenType::Bool).map(|to_map| to_map.map(|_| "bool".to_string())),
        just_token(TokenType::Char).map(|to_map| to_map.map(|_| "char".to_string())),
    ))
}

/** This parses idendifierts
*/
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

/** This parses statement seperators.
*/
pub(crate) fn statement_seperator<'a>()
-> impl Parser<'a, &'a [PosInfoWrapper<Token, CodeFile>], ()> + Clone {
    just_token(TokenType::StatementSeparator)
        .repeated()
        .at_least(1)
        .ignored()
}

pub(crate) fn just_token<'a>(
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
