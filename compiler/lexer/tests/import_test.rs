use lexer::{Token, TokenType, lex};

#[test]
fn test_import() {
    let input = r#"
    import "math/pi"
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1
        },
        Token {
            kind: TokenType::Import,
            line: 1,
            span: 4..10,
        },
        Token {
            // raw string for readability, otherwise output would be: "\"math/pi\""
            kind: TokenType::Quote(r#""math/pi""#.to_string()),
            line: 1,
            span: 11..20
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 20..21,
        }
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
