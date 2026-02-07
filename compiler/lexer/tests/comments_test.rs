use lexer::{Token, TokenType, lex};

#[test]
fn test_comment() {
    let input = r#"
    // This is a comment
    //Wasome is the best language
    // :)
    // // oh my god, DOUBLE COMMENT
    //
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Comment("// This is a comment".to_string()),
            span: 5..25,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 25..26,
        },
        Token {
            kind: TokenType::Comment("//Wasome is the best language".to_string()),
            span: 30..59,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 59..60,
        },
        Token {
            kind: TokenType::Comment("// :)".to_string()),
            span: 64..69,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 69..70,
        },
        Token {
            kind: TokenType::Comment("// // oh my god, DOUBLE COMMENT".to_string()),
            span: 74..105,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 105..106,
        },
        Token {
            kind: TokenType::Comment("//".to_string()),
            span: 110..112,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 112..113,
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
