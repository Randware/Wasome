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

    let tokens: Vec<Token> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
