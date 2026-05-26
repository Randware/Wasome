use lexer::{Token, TokenType, lex};

#[test]
fn test_all_datatypes() {
    let input = r#"
    s8 s16 s32 s64 u8 u16 u32 u64 f32 f64 bool char self
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::S8,
            span: 5..7,
        },
        Token {
            kind: TokenType::S16,
            span: 8..11,
        },
        Token {
            kind: TokenType::S32,
            span: 12..15,
        },
        Token {
            kind: TokenType::S64,
            span: 16..19,
        },
        Token {
            kind: TokenType::U8,
            span: 20..22,
        },
        Token {
            kind: TokenType::U16,
            span: 23..26,
        },
        Token {
            kind: TokenType::U32,
            span: 27..30,
        },
        Token {
            kind: TokenType::U64,
            span: 31..34,
        },
        Token {
            kind: TokenType::F32,
            span: 35..38,
        },
        Token {
            kind: TokenType::F64,
            span: 39..42,
        },
        Token {
            kind: TokenType::Bool,
            span: 43..47,
        },
        Token {
            kind: TokenType::Char,
            span: 48..52,
        },
        Token {
            kind: TokenType::SelfType,
            span: 53..57,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 57..58,
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

#[test]
fn test_all_datatypes_no_spaces() {
    let input = r#"
    s8s16s32s64u8u16u32u64f32f64boolcharself
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Identifier("s8s16s32s64u8u16u32u64f32f64boolcharself".to_string()),
            span: 5..45,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 45..46,
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
