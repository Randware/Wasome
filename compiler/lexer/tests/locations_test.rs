use lexer::{Token, TokenType, lex};

#[test]
fn test_location() {
    let input = r#"
    s32 var1 <- 10
    
    
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::S32,
            span: 5..8,
        },
        Token {
            kind: TokenType::Identifier("var1".to_string()),
            span: 9..13,
        },
        Token {
            kind: TokenType::Assign,
            span: 14..16,
        },
        Token {
            kind: TokenType::Integer(10),
            span: 17..19,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 19..20,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 24..25,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 29..30,
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
