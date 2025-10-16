use lexer::{TokenType, lex};

#[test]
fn test_all_brackets() {
    let input = r#"
     { } ( )
     "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::OpenScope,
        TokenType::CloseScope,
        TokenType::OpenParen,
        TokenType::CloseParen,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_all_brackets_no_spaces() {
    let input = r#"
     {}()
     "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::OpenScope,
        TokenType::CloseScope,
        TokenType::OpenParen,
        TokenType::CloseParen,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
