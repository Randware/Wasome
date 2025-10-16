use lexer::{Token, TokenType, lex};

#[test]
fn test_all_datatypes() {
    let input = r#"
    s8 s16 s32 s64 u8 u16 u32 u64 f32 f64 bool char self
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 0..0,
        },
        Token {
            kind: TokenType::S8,
            line: 1,
            span: 4..6,
        },
        Token {
            kind: TokenType::S16,
            line: 1,
            span: 7..10,
        },
        Token {
            kind: TokenType::S32,
            line: 1,
            span: 11..14,
        },
        Token {
            kind: TokenType::S64,
            line: 1,
            span: 15..18,
        },
        Token {
            kind: TokenType::U8,
            line: 1,
            span: 19..21,
        },
        Token {
            kind: TokenType::U16,
            line: 1,
            span: 22..25,
        },
        Token {
            kind: TokenType::U32,
            line: 1,
            span: 26..29,
        },
        Token {
            kind: TokenType::U64,
            line: 1,
            span: 30..33,
        },
        Token {
            kind: TokenType::F32,
            line: 1,
            span: 34..37,
        },
        Token {
            kind: TokenType::F64,
            line: 1,
            span: 38..41,
        },
        Token {
            kind: TokenType::Bool,
            line: 1,
            span: 42..46,
        },
        Token {
            kind: TokenType::Char,
            line: 1,
            span: 47..51,
        },
        Token {
            kind: TokenType::SelfType,
            line: 1,
            span: 52..56,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 0..0,
        },
    ];

    let tokens: Vec<_> = lex(input).filter_map(|r| r.ok()).collect();
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_all_datatypes_no_spaces() {
    let input = r#"
    s8s16s32s64u8u16u32u64f32f64boolcharself
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 0..0,
        },
        Token {
            kind: TokenType::Identifier("s8s16s32s64u8u16u32u64f32f64boolcharself".to_string()),
            line: 1,
            span: 4..44,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 0..0,
        },
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();
    
    assert_eq!(tokens, expected_tokens);
}
