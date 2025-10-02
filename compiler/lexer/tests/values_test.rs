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
