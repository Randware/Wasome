use crate::tokens::Token;
use logos::Logos;

mod tokens;

pub fn lex(input: &str ) -> impl Iterator<Item = Result<Token, ()>> {
    Token::lexer(&input)
}


