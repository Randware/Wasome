use lexer::{Token, TokenType, lex};

#[test]
fn test_all_brackets() {
    let input = r#"
    { } ( )
     "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::OpenScope,
            line: 1,
            span: 4..5,
        },
        Token {
            kind: TokenType::CloseScope,
            line: 1,
            span: 6..7,
        },
        Token {
            kind: TokenType::OpenParen,
            line: 1,
            span: 8..9,
        },
        Token {
            kind: TokenType::CloseParen,
            line: 1,
            span: 10..11,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 11..12,
        },
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
