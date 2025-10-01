pub mod tokens; // <- make the module public
pub use tokens::Token;
use logos::Logos;

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, ()>> {
    Token::lexer(input)
}


