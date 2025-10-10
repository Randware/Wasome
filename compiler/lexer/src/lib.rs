pub mod tokens;
use crate::tokens::LexError;
use logos::Logos;
pub use tokens::Token;

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, LexError>> {
    Token::lexer(input)
}
