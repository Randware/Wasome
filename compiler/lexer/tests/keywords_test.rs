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
            line: 1,
            span: 0..0,
        },
        Token {
            kind: TokenType::Function,
            line: 1,
            span: 4..6,
        },
        Token {
            kind: TokenType::If,
            line: 1,
            span: 7..9,
        },
        Token {
            kind: TokenType::Else,
            line: 1,
            span: 10..14,
        },
        Token {
            kind: TokenType::Loop,
            line: 1,
            span: 15..19,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 0..0,
        },
        Token {
            kind: TokenType::Struct,
            line: 2,
            span: 4..10,
        },
        Token {
            kind: TokenType::Return,
            line: 2,
            span: 11..13,
        },
        Token {
            kind: TokenType::Assign,
            line: 2,
            span: 14..16,
        },
        Token {
            kind: TokenType::Enum,
            line: 2,
            span: 17..21,
        },
        Token {
            kind: TokenType::As,
            line: 2,
            span: 22..24,
        },
        Token {
            kind: TokenType::Public,
            line: 2,
            span: 25..28,
        },
        Token {
            kind: TokenType::New,
            line: 2,
            span: 29..32,
        },
        Token {
            kind: TokenType::PathSeparator,
            line: 2,
            span: 33..35,
        },
        Token {
            kind: TokenType::Dot,
            line: 2,
            span: 36..37,
        },
        Token {
            kind: TokenType::Semicolon,
            line: 2,
            span: 38..39,
        },
        Token {
            kind: TokenType::ArgumentSeparator,
            line: 2,
            span: 40..41,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 0..0,
        },
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    assert_eq!(tokens, expected_tokens);
}