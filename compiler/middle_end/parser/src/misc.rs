use chumsky::prelude::*;
use lexer::TokenType;

/** This parses arbitiary data types
*/
pub(crate) fn datatype_parser<'src>() -> impl Parser<'src, &'src [TokenType], String> + Clone {
    choice((
        just(TokenType::F32).map(|_| "f32".to_string()),
        just(TokenType::F64).map(|_| "f64".to_string()),
        just(TokenType::S8).map(|_| "s8".to_string()),
        just(TokenType::U8).map(|_| "u8".to_string()),
        just(TokenType::S16).map(|_| "s16".to_string()),
        just(TokenType::U16).map(|_| "u16".to_string()),
        just(TokenType::S32).map(|_| "s32".to_string()),
        just(TokenType::U32).map(|_| "u32".to_string()),
        just(TokenType::S64).map(|_| "s64".to_string()),
        just(TokenType::U64).map(|_| "u64".to_string()),
        just(TokenType::Bool).map(|_| "bool".to_string()),
        just(TokenType::Char).map(|_| "char".to_string()),
    ))
}

/** This parses idendifierts
*/
pub(crate) fn identifier_parser<'a>() -> impl Parser<'a, &'a [TokenType], String> + Clone {
    custom(|token| match token.next().ok_or(EmptyErr::default())? {
        TokenType::Identifier(inner) => Ok(inner),
        _ => Err(EmptyErr::default()),
    })
}

/** This parses statement seperators.
*/
pub(crate) fn statement_seperator<'a>() -> impl Parser<'a, &'a [TokenType], ()> + Clone {
    just(TokenType::StatementSeparator)
        .repeated()
        .at_least(1)
        .ignored()
}
