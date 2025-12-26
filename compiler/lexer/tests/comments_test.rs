use lexer::{lex, Token, TokenType};

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
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Comment("// This is a comment".to_string()),
            line: 1,
            span: 4..24,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 24..25,
        },
        Token {
            kind: TokenType::Comment("//Wasome is the best language".to_string()),
            line: 2,
            span: 4..33,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 33..34,
        },
        Token {
            kind: TokenType::Comment("// :)".to_string()),
            line: 3,
            span: 4..9,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 9..10,
        },
        Token {
            kind: TokenType::Comment("// // oh my god, DOUBLE COMMENT".to_string()),
            line: 4,
            span: 4..35,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 4,
            span: 35..36,
        },
        Token {
            kind: TokenType::Comment("//".to_string()),
            line: 5,
            span: 4..6,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 5,
            span: 6..7,
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