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
                let line = lexer.extras.0;
                let line_start = lexer.extras.1;

                let start = lexer.span().start - line_start;
                let end = lexer.span().end - line_start;
                let span = start..end;

                if let TokenType::StatementSeparator = kind {
                    let tok = Token { kind, line, span };
                    lexer.extras.0 += 1;
                    lexer.extras.1 = lexer.span().end;
                    return Some(Ok(tok));
                }

                Some(Ok(Token { kind, line, span }))
            }
            Err(e) => Some(Err(e)),
        }
    })
}
