use lexer::{TokenType, lex};
#[test]
fn test_all_datatypes() {
    let input = r#"
    s8 s16 s32 s64 u8 u16 u32 u64 f32 f64 bool char self
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::S8,
        TokenType::S16,
        TokenType::S32,
        TokenType::S64,
        TokenType::U8,
        TokenType::U16,
        TokenType::U32,
        TokenType::U64,
        TokenType::F32,
        TokenType::F64,
        TokenType::Bool,
        TokenType::Char,
        TokenType::SelfType,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_all_datatypes_no_spaces() {
    let input = r#"
    s8s16s32s64u8u16u32u64f32f64boolcharself
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Identifier("s8s16s32s64u8u16u32u64f32f64boolcharself".to_string()),
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);

    assert_eq!(tokens, expected_tokens);
}
