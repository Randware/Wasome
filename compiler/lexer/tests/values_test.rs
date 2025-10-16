use lexer::{TokenType, lex};

#[test]
fn test_all_values() {
    let input = r#"
    name name_trimmed 
    0.123 123.0 123.01
    0 123
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Identifier("name".to_string()),
        TokenType::Identifier("name_trimmed".to_string()),
        TokenType::StatementSeparator,
        TokenType::Decimal(0.123),
        TokenType::Decimal(123.0),
        TokenType::Decimal(123.01),
        TokenType::StatementSeparator,
        TokenType::Integer(0),
        TokenType::Integer(123),
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_broken_format_decimal() {
    let input = r#"
    0.1 0.1.1
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Decimal(0.1),
        TokenType::Decimal(0.1),
        TokenType::Dot,
        TokenType::Integer(1),
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_char_literal() {
    let input = r#"
    char var1 <- 'n'
    char var2 <- '🎌'
    char var3 <- '\n'
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Char,
        TokenType::Identifier("var1".to_string()),
        TokenType::Assign,
        TokenType::CharLiteral('n'),
        TokenType::StatementSeparator,
        TokenType::Char,
        TokenType::Identifier("var2".to_string()),
        TokenType::Assign,
        TokenType::CharLiteral('🎌'),
        TokenType::StatementSeparator,
        TokenType::Char,
        TokenType::Identifier("var3".to_string()),
        TokenType::Assign,
        TokenType::CharLiteral('\n'),
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
