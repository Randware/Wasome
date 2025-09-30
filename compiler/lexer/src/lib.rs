use crate::tokens::Token;
use logos::Logos;

mod tokens;

pub fn lex(input: String) -> Vec<Token> {
    let mut lex = Token::lexer(&input);
    let mut tokens = Vec::new();

    while let Some(token) = lex.next() {
        tokens.push(token.unwrap());
    }
    tokens
}

