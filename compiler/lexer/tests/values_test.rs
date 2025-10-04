use lexer::{lex, Token}; 

#[test]
fn test_all_values() {
    let input = r#"
    name name_trimmed 
    0.123 123.0 123.01
    0 123
    "#;

    let expected_tokens = vec![
    Token::Identifier("name".to_string()),
    Token::Identifier("name_trimmed".to_string()),
    Token::Decimal(0.123),
    Token::Decimal(123.0),
    Token::Decimal(123.01),
    Token::Integer(0),
    Token::Integer(123),
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
        Token::Decimal(0.1),
        Token::Decimal(1.1),
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_ne!(tokens, expected_tokens);
}
