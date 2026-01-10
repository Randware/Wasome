use lexer::{Token, TokenType, lex};

#[test]
fn test_infinite_loop() {
    let input = r#"
    loop { }
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Loop,
            line: 1,
            span: 4..8,
        },
        Token {
            kind: TokenType::OpenScope,
            line: 1,
            span: 9..10,
        },
        Token {
            kind: TokenType::CloseScope,
            line: 1,
            span: 11..12,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 12..13,
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
fn test_while_loop() {
    let input = r#"
    u32 count <- 0
    loop (count < 5) {
       count <- count + 1
    }
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::U32,
            line: 1,
            span: 4..7,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 1,
            span: 8..13,
        },
        Token {
            kind: TokenType::Assign,
            line: 1,
            span: 14..16,
        },
        Token {
            kind: TokenType::Integer(0),
            line: 1,
            span: 17..18,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 18..19,
        },
        Token {
            kind: TokenType::Loop,
            line: 2,
            span: 4..8,
        },
        Token {
            kind: TokenType::OpenParen,
            line: 2,
            span: 9..10,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 2,
            span: 10..15,
        },
        Token {
            kind: TokenType::LessThan,
            line: 2,
            span: 16..17,
        },
        Token {
            kind: TokenType::Integer(5),
            line: 2,
            span: 18..19,
        },
        Token {
            kind: TokenType::CloseParen,
            line: 2,
            span: 19..20,
        },
        Token {
            kind: TokenType::OpenScope,
            line: 2,
            span: 21..22,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 22..23,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 3,
            span: 7..12,
        },
        Token {
            kind: TokenType::Assign,
            line: 3,
            span: 13..15,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 3,
            span: 16..21,
        },
        Token {
            kind: TokenType::Addition,
            line: 3,
            span: 22..23,
        },
        Token {
            kind: TokenType::Integer(1),
            line: 3,
            span: 24..25,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 25..26,
        },
        Token {
            kind: TokenType::CloseScope,
            line: 4,
            span: 4..5,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 4,
            span: 5..6,
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
fn test_for_loop() {
    let input = r#"
    loop (u32 count <- 0; count < 5; count <- count + 1 ) {
       count <- count + 1
    }
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Loop,
            line: 1,
            span: 4..8,
        },
        Token {
            kind: TokenType::OpenParen,
            line: 1,
            span: 9..10,
        },
        Token {
            kind: TokenType::U32,
            line: 1,
            span: 10..13,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 1,
            span: 14..19,
        },
        Token {
            kind: TokenType::Assign,
            line: 1,
            span: 20..22,
        },
        Token {
            kind: TokenType::Integer(0),
            line: 1,
            span: 23..24,
        },
        Token {
            kind: TokenType::Semicolon,
            line: 1,
            span: 24..25,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 1,
            span: 26..31,
        },
        Token {
            kind: TokenType::LessThan,
            line: 1,
            span: 32..33,
        },
        Token {
            kind: TokenType::Integer(5),
            line: 1,
            span: 34..35,
        },
        Token {
            kind: TokenType::Semicolon,
            line: 1,
            span: 35..36,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 1,
            span: 37..42,
        },
        Token {
            kind: TokenType::Assign,
            line: 1,
            span: 43..45,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 1,
            span: 46..51,
        },
        Token {
            kind: TokenType::Addition,
            line: 1,
            span: 52..53,
        },
        Token {
            kind: TokenType::Integer(1),
            line: 1,
            span: 54..55,
        },
        Token {
            kind: TokenType::CloseParen,
            line: 1,
            span: 56..57,
        },
        Token {
            kind: TokenType::OpenScope,
            line: 1,
            span: 58..59,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 59..60,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 2,
            span: 7..12,
        },
        Token {
            kind: TokenType::Assign,
            line: 2,
            span: 13..15,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            line: 2,
            span: 16..21,
        },
        Token {
            kind: TokenType::Addition,
            line: 2,
            span: 22..23,
        },
        Token {
            kind: TokenType::Integer(1),
            line: 2,
            span: 24..25,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 2,
            span: 25..26,
        },
        Token {
            kind: TokenType::CloseScope,
            line: 3,
            span: 4..5,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 3,
            span: 5..6,
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
