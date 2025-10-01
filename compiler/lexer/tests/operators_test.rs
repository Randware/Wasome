use lexer::{lex, Token};

#[test]
fn test_all_tokens() {
    let input = r#"
    + - * / %
    < > <= >= != == << >> | || & && !
    "#;

    let expected_tokens = vec![
        Token::Addition,
        Token::Subtraction,
        Token::Multiplication,
        Token::Division,
        Token::Modulo,
        Token::LessThan,
        Token::GreaterThan,
        Token::LessThanEqual,
        Token::GreaterThanEqual,
        Token::NotEqual,
        Token::Comparison,
        Token::LShift,
        Token::RShift,
        Token::BitOr,
        Token::Or,
        Token::BitAnd,
        Token::And,
        Token::Not,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_eq!(tokens, expected_tokens);
}
