pub mod tokens; 
pub use tokens::Token;
use logos::Logos;
use crate::tokens::LexError;

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, LexError>> {
    Token::lexer(input)
}


