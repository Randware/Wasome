use chumsky::prelude::*;
use lexer::Token;

/** This parses arbitiary data types
*/
pub(crate) fn datatype_parser<'src>() -> impl Parser<'src, &'src [Token], String> + Clone {
    choice((
        just(Token::F32).map(|_| "f32".to_string()),
        just(Token::F64).map(|_| "f64".to_string()),
        just(Token::S8).map(|_| "s8".to_string()),
        just(Token::U8).map(|_| "u8".to_string()),
        just(Token::S16).map(|_| "s16".to_string()),
        just(Token::U16).map(|_| "u16".to_string()),
        just(Token::S32).map(|_| "s32".to_string()),
        just(Token::U32).map(|_| "u32".to_string()),
        just(Token::S64).map(|_| "s64".to_string()),
        just(Token::U64).map(|_| "u64".to_string()),
        just(Token::Bool).map(|_| "bool".to_string()),
        just(Token::Char).map(|_| "char".to_string()),
    ))
}

/** This parses idendifierts
*/
pub(crate) fn identifier_parser<'a>() -> impl Parser<'a, &'a [Token], String> + Clone {
    custom(|token| match token.next().ok_or(EmptyErr::default())? {
        Token::Identifier(inner) => Ok(inner),
        _ => Err(EmptyErr::default()),
    })
}

/** This parses statement seperators.
*/
pub(crate) fn statement_seperator<'a>() -> impl Parser<'a, &'a [Token], ()> + Clone {
    just(Token::StatementSeparator)
        .repeated()
        .at_least(1)
        .ignored()
}
