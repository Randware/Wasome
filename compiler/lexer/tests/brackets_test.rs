use lexer::{Token, lex};

#[test]
fn test_all_brackets() {
    let input = r#"
     { } ( )
     "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::OpenScope,
        Token::CloseScope,
        Token::OpenParen,
        Token::CloseParen,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_all_brackets_no_spaces() {
    let input = r#"
     {}()
     "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::OpenScope,
        Token::CloseScope,
        Token::OpenParen,
        Token::CloseParen,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
