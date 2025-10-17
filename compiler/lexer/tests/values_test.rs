use lexer::{Token, TokenType, lex};

#[test]
fn test_all_values() {
    let input = r#"
    name name_trimmed
    0.123 123.0 123.01
    0 123
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Identifier("name".to_string()),
            line: 1,
            span: 4..8,
        },
        Token {
            kind: TokenType::Identifier("name_trimmed".to_string()),
            line: 1,
            span: 9..21,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 21..22,
        },
        Token {
            kind: TokenType::Decimal(0.123),
            line: 2,
            span: 4..9,
        },
        Token {
            kind: TokenType::Decimal(123.0),
            line: 2,
            span: 10..15,
        },
        Token {
            kind: TokenType::Decimal(123.01),
            line: 2,
            span: 16..22,
        }, 
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 22..23,
        },
        Token {
            kind: TokenType::Integer(0),
            line: 3,
            span: 4..5,
        },
        Token {
            kind: TokenType::Integer(123),
            line: 3,
            span: 6..9,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 9..10,
        },
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_broken_format_decimal() {
    let input = r#"
    0.1 0.1.1
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Decimal(0.1),
            line: 1,
            span: 4..7,
        },
        Token {
            kind: TokenType::Decimal(0.1),
            line: 1,
            span: 8..11,
        },
        Token {
            kind: TokenType::Dot,
            line: 1,
            span: 11..12,
        },
        Token {
            kind: TokenType::Integer(1),
            line: 1,
            span: 12..13,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 13..14,
        },
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_char_literal() {
    let input = r#"
    char var1 <- 'n'
    char var2 <- '🎌'
    char var3 <- '\n'
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Char,
            line: 1,
            span: 4..8,
        },
        Token {
            kind: TokenType::Identifier("var1".to_string()),
            line: 1,
            span: 9..13,
        },
        Token {
            kind: TokenType::Assign,
            line: 1,
            span: 14..16,
        },
        Token {
            kind: TokenType::CharLiteral('n'),
            line: 1,
            span: 17..20,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 20..21,
        },
        Token {
            kind: TokenType::Char,
            line: 2,
            span: 4..8,
        },
        Token {
            kind: TokenType::Identifier("var2".to_string()),
            line: 2,
            span: 9..13,
        },
        Token {
            kind: TokenType::Assign,
            line: 2,
            span: 14..16,
        },
        Token {
            kind: TokenType::CharLiteral('🎌'),
            line: 2,
            span: 17..23,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 23..24,
        },
        Token {
            kind: TokenType::Char,
            line: 3,
            span: 4..8,
        },
        Token {
            kind: TokenType::Identifier("var3".to_string()),
            line: 3,
            span: 9..13,
        },
        Token {
            kind: TokenType::Assign,
            line: 3,
            span: 14..16,
        },
        Token {
            kind: TokenType::CharLiteral('\n'),
            line: 3,
            span: 17..21,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 21..22,
        },
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    assert_eq!(tokens, expected_tokens);
}
