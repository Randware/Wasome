pub mod tokens;
use crate::tokens::LexError;
use logos::Logos;
pub use tokens::Token;
pub use tokens::TokenType;

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, LexError>> + '_ {
    let mut lexer = TokenType::lexer(input);

    std::iter::from_fn(move || {
        // `lexer.next()` returns Option<Result<TokenType, LexError>>
        let next = lexer.next()?;
        match next {
            Ok(kind) => {
                // current line and line_start (absolute byte index)
                let line = lexer.extras.0;
                let line_start = lexer.extras.1;

                // compute span relative to line start (start..end)
                let start = lexer.span().start.saturating_sub(line_start);
                let end = lexer.span().end.saturating_sub(line_start);
                let span = start..end;

                // If this is a newline/token separator, we must advance extras AFTER
                // creating the token so the token's span is assigned to the line it terminates.
                if let TokenType::StatementSeparator = kind {
                    // prepare token to return for *current* line
                    let tok = Token { kind, line, span };
                    // Update extras for next line: increment line, set next line start to current span end (global)
                    lexer.extras.0 += 1;
                    lexer.extras.1 = lexer.span().end; // absolute byte index after newline
                    return Some(Ok(tok));
                }

                // Normal token: don't change extras here
                Some(Ok(Token { kind, line, span }))
            }
            Err(e) => Some(Err(e)),
        }
    })
}
