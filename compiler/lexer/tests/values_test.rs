use lexer::{lex, Token}; 

#[test]
fn test_all_values() {
    let input = r#"
    name name_trimmed 
    0.123 123.0 123.01
    0 123
    "#;

    let expected_tokens = vec![
    Token::Identifier,
    Token::Identifier,
    Token::Decimal,
    Token::Decimal,
    Token::Decimal,
    Token::Integer,
    Token::Integer,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();
    

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_brokenformat_decimal() {
    let input = r#"
    0.1 0.1.1
    "#;

    let expected_tokens = vec![
        Token::Decimal,
        Token::Decimal,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_ne!(tokens, expected_tokens);
}

#[test]
fn test_identifier_cannot_start_with_number() {
    let input = r#"
    1name 1name_trimmed
    "#;

    let expected_tokens = vec![
        Token::Integer,
        Token::Identifier,
        Token::Integer,
        Token::Identifier,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_ne!(tokens, expected_tokens);
}