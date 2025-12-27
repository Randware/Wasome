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
