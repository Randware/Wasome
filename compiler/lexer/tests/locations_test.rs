use lexer::{Token, TokenType, lex};

#[test]
fn test_location() {
    let input = r#"
    s32 var1 <- 10
    
    
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 0..0,
        },
        Token {
            kind: TokenType::S32,
            line: 1,
            span: 4..7,
        },
        Token {
            kind: TokenType::Identifier("var1".to_string()),
            line: 1,
            span: 8..12,
        },
        Token {
            kind: TokenType::Assign,
            line: 1,
            span: 13..15,
        },
        Token {
            kind: TokenType::Integer(10),
            line: 1,
            span: 16..18,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 0..0,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 0..0,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 4,
            span: 0..0,
        },
    ];

    let tokens: Vec<Token> = lex(input)
        .filter_map(|result| result.ok()) // keep only Ok tokens
        .collect();

    for token in &tokens {
        println!(
            "Token {:?} at line {}, ran {}",
            token.kind, token.line, token.span.start
        );
    }

    assert_eq!(tokens, expected_tokens);
}
