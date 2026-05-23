use lexer::{Token, TokenType, lex};

#[test]
fn test_all_brackets() {
    let input = r#"
    { } ( ) [ ]
     "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::OpenScope,
            span: 5..6,
        },
        Token {
            kind: TokenType::CloseScope,
            span: 7..8,
        },
        Token {
            kind: TokenType::OpenParen,
            span: 9..10,
        },
        Token {
            kind: TokenType::CloseParen,
            span: 11..12,
        },
        Token {
            kind: TokenType::OpenGeneric,
            span: 13..14,
        },
        Token {
            kind: TokenType::CloseGeneric,
            span: 15..16,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 16..17,
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
