pub mod tokens;
use crate::tokens::LexError;
use logos::Logos;
pub use tokens::Token;
pub use tokens::TokenType;

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, LexError>> + '_ {
    let mut lexer = TokenType::lexer(input);

    std::iter::from_fn(move || {
        let next = lexer.next()?;
        match next {
            Ok(kind) => {
                Some(Ok(Token { kind, span: lexer.span() }))
            }
            Err(e) => Some(Err(e)),
        }
    })
}
