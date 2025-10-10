use lexer::{Token, lex};

#[test]
fn test_all_values() {
    let input = r#"
    name name_trimmed 
    0.123 123.0 123.01
    0 123
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Identifier("name".to_string()),
        Token::Identifier("name_trimmed".to_string()),
        Token::StatementSeparator,
        Token::Decimal(0.123),
        Token::Decimal(123.0),
        Token::Decimal(123.01),
        Token::StatementSeparator,
        Token::Integer(0),
        Token::Integer(123),
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_brokenformat_decimal() {
    let input = r#"
    0.1 0.1.1
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Decimal(0.1),
        Token::Decimal(1.1),
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_ne!(tokens, expected_tokens);
}

#[test]
fn test_charLiteral() {
    let input = r#"
    char var1 <- 'n'
    char var2 <- '🎌'
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Char,
        Token::Identifier("var1".to_string()),
        Token::Assign,
        Token::CharLiteral('n'),
        Token::StatementSeparator,
        Token::Char,
        Token::Identifier("var2".to_string()),
        Token::Assign,
        Token::CharLiteral('🎌'),
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
