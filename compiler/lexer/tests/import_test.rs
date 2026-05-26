use lexer::{Token, TokenType, lex};

#[test]
fn test_import() {
    let input = r#"
    import "math/pi"
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Import,
            span: 5..11,
        },
        Token {
            kind: TokenType::String(r#""math/pi""#.to_string()),
            span: 12..21,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 21..22,
        },
    ];

    // Lexing, panics if met with an error
    let actual_tokens: Vec<Token> = lex(input)
        .map(|res| res.expect("Lexer failed with error"))
        .collect();

    // Comparing
    for (i, (got, want)) in actual_tokens.iter().zip(expected_tokens.iter()).enumerate() {
        assert_eq!(
            got, want,
            "\nMismatch at Token #{}:\n   Got: {:?}\n  Want: {:?}\n",
            i, got, want
        );
    }
}
