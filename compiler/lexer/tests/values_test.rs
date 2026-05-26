use lexer::{Token, TokenType, lex};

#[test]
fn test_all_values() {
    let input = r#"
    name name_trimmed
    0.123 123.0 123.01
    0 123 true false
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Identifier("name".to_string()),
            span: 5..9,
        },
        Token {
            kind: TokenType::Identifier("name_trimmed".to_string()),
            span: 10..22,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 22..23,
        },
        Token {
            kind: TokenType::Decimal(0.123),
            span: 27..32,
        },
        Token {
            kind: TokenType::Decimal(123.0),
            span: 33..38,
        },
        Token {
            kind: TokenType::Decimal(123.01),
            span: 39..45,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 45..46,
        },
        Token {
            kind: TokenType::Integer(0),
            span: 50..51,
        },
        Token {
            kind: TokenType::Integer(123),
            span: 52..55,
        },
        Token {
            kind: TokenType::True,
            span: 56..60,
        },
        Token {
            kind: TokenType::False,
            span: 61..66,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 66..67,
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
fn test_broken_format_decimal() {
    let input = r#"
    0.1 0.1.1
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Decimal(0.1),
            span: 5..8,
        },
        Token {
            kind: TokenType::Decimal(0.1),
            span: 9..12,
        },
        Token {
            kind: TokenType::Dot,
            span: 12..13,
        },
        Token {
            kind: TokenType::Integer(1),
            span: 13..14,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 14..15,
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
fn test_char_literal() {
    let input = r#"
    char var1 <- 'n'
    char var2 <- '🎌'
    char var3 <- '\n'
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Char,
            span: 5..9,
        },
        Token {
            kind: TokenType::Identifier("var1".to_string()),
            span: 10..14,
        },
        Token {
            kind: TokenType::Assign,
            span: 15..17,
        },
        Token {
            kind: TokenType::CharLiteral('n'),
            span: 18..21,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 21..22,
        },
        Token {
            kind: TokenType::Char,
            span: 26..30,
        },
        Token {
            kind: TokenType::Identifier("var2".to_string()),
            span: 31..35,
        },
        Token {
            kind: TokenType::Assign,
            span: 36..38,
        },
        Token {
            kind: TokenType::CharLiteral('🎌'),
            span: 39..45,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 45..46,
        },
        Token {
            kind: TokenType::Char,
            span: 50..54,
        },
        Token {
            kind: TokenType::Identifier("var3".to_string()),
            span: 55..59,
        },
        Token {
            kind: TokenType::Assign,
            span: 60..62,
        },
        Token {
            kind: TokenType::CharLiteral('\n'),
            span: 63..67,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 67..68,
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
