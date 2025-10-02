use lexer::{lex, Token};
#[test]
fn test_all_datatypes() {
    let input = r#"
    s8 s16 s32 s64 u8 u16 u32 u64 f32 f64 bool char self
    "#;

    let expected_tokens = vec![
        Token::S8,
        Token::S16,
        Token::S32,
        Token::S64,
        Token::U8,
        Token::U16,
        Token::U32,
        Token::U64,
        Token::F32,
        Token::F64,
        Token::Bool,
        Token::Char,
        Token::SelfType,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok()) 
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_all_datatypes_no_spaces() {
    let input = r#"
    s8s16s32s64u8u16u32u64f32f64boolcharself
    "#;

    let expected_tokens = vec![
       Token::Identifier
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    assert_eq!(tokens, expected_tokens);
}
