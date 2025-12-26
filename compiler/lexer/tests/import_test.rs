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
            span: 4..10
        },
        Token {
            kind: TokenType::Quote(r#""math/pi""#.to_string()),
            line: 1,
            span: 11..20
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 20..21
        }
    ];

    // Lexing, panics if met with an error
    let actual_tokens: Vec<Token> = lex(input)
        .map(|res| res.expect("Lexer failed with error"))
        .collect();

    // Comparing
    for (i, (got, want)) in actual_tokens.iter().zip(expected_tokens.iter()).enumerate() {
        assert_eq!(
            got,
            want,
            "\nMismatch at Token #{}:\n   Got: {:?}\n  Want: {:?}\n",
            i, got, want
        );
    }
}