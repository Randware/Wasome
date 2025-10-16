pub mod tokens;
use crate::tokens::LexError;
use logos::Logos;
pub use tokens::Token;
pub use tokens::TokenType;

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, LexError>> {
    let mut lexer = TokenType::lexer(input);

    std::iter::from_fn(move || match lexer.next() {
        Some(Ok(kind)) => {
            let line = lexer.extras.0;
            let span = {
                let start = lexer.span().start.saturating_sub(lexer.extras.1.start);
                let end = lexer.span().end.saturating_sub(lexer.extras.1.start);
                start..end
            };
            Some(Ok(Token { kind, line, span }))
        }
        Some(Err(e)) => Some(Err(e)),
        None => None,
    })
}
