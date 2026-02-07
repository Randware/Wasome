use lexer::{Token, TokenType, lex};

#[test]
fn test_all_keywords() {
    let input = r#"
    fn if else loop
    struct -> <- enum as pub new :: . ; ,
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Function,
            span: 5..7,
        },
        Token {
            kind: TokenType::If,
            span: 8..10,
        },
        Token {
            kind: TokenType::Else,
            span: 11..15,
        },
        Token {
            kind: TokenType::Loop,
            span: 16..20,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 20..21,
        },
        Token {
            kind: TokenType::Struct,
            span: 25..31,
        },
        Token {
            kind: TokenType::Return,
            span: 32..34,
        },
        Token {
            kind: TokenType::Assign,
            span: 35..37,
        },
        Token {
            kind: TokenType::Enum,
            span: 38..42,
        },
        Token {
            kind: TokenType::As,
            span: 43..45,
        },
        Token {
            kind: TokenType::Public,
            span: 46..49,
        },
        Token {
            kind: TokenType::New,
            span: 50..53,
        },
        Token {
            kind: TokenType::PathSeparator,
            span: 54..56,
        },
        Token {
            kind: TokenType::Dot,
            span: 57..58,
        },
        Token {
            kind: TokenType::Semicolon,
            span: 59..60,
        },
        Token {
            kind: TokenType::ArgumentSeparator,
            span: 61..62,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 62..63,
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
