use lexer::{Token, TokenType, lex};

#[test]
fn test_location() {
    let input = r#"
    s32 var1 <- 10
    
    
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
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
            line: 1,
            span: 18..19,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 4..5,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 4..5,
        },
    ];

    let tokens: Vec<Token> = lex(input)
        .filter_map(|result| result.ok()) 
        .collect();

    assert_eq!(tokens, expected_tokens);
}
