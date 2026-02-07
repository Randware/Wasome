use lexer::{Token, TokenType, lex};

#[test]
fn test_infinite_loop() {
    let input = r#"
    loop { }
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Loop,
            span: 5..9,
        },
        Token {
            kind: TokenType::OpenScope,
            span: 10..11,
        },
        Token {
            kind: TokenType::CloseScope,
            span: 12..13,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 13..14,
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
            span: 0..1,
        },
        Token {
            kind: TokenType::U32,
            span: 5..8,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 9..14,
        },
        Token {
            kind: TokenType::Assign,
            span: 15..17,
        },
        Token {
            kind: TokenType::Integer(0),
            span: 18..19,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 19..20,
        },
        Token {
            kind: TokenType::Loop,
            span: 24..28,
        },
        Token {
            kind: TokenType::OpenParen,
            span: 29..30,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 30..35,
        },
        Token {
            kind: TokenType::LessThan,
            span: 36..37,
        },
        Token {
            kind: TokenType::Integer(5),
            span: 38..39,
        },
        Token {
            kind: TokenType::CloseParen,
            span: 39..40,
        },
        Token {
            kind: TokenType::OpenScope,
            span: 41..42,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 42..43,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 50..55,
        },
        Token {
            kind: TokenType::Assign,
            span: 56..58,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 59..64,
        },
        Token {
            kind: TokenType::Addition,
            span: 65..66,
        },
        Token {
            kind: TokenType::Integer(1),
            span: 67..68,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 68..69,
        },
        Token {
            kind: TokenType::CloseScope,
            span: 73..74,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 74..75,
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
            span: 0..1,
        },
        Token {
            kind: TokenType::Loop,
            span: 5..9,
        },
        Token {
            kind: TokenType::OpenParen,
            span: 10..11,
        },
        Token {
            kind: TokenType::U32,
            span: 11..14,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 15..20,
        },
        Token {
            kind: TokenType::Assign,
            span: 21..23,
        },
        Token {
            kind: TokenType::Integer(0),
            span: 24..25,
        },
        Token {
            kind: TokenType::Semicolon,
            span: 25..26,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 27..32,
        },
        Token {
            kind: TokenType::LessThan,
            span: 33..34,
        },
        Token {
            kind: TokenType::Integer(5),
            span: 35..36,
        },
        Token {
            kind: TokenType::Semicolon,
            span: 36..37,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 38..43,
        },
        Token {
            kind: TokenType::Assign,
            span: 44..46,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 47..52,
        },
        Token {
            kind: TokenType::Addition,
            span: 53..54,
        },
        Token {
            kind: TokenType::Integer(1),
            span: 55..56,
        },
        Token {
            kind: TokenType::CloseParen,
            span: 57..58,
        },
        Token {
            kind: TokenType::OpenScope,
            span: 59..60,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 60..61,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 68..73,
        },
        Token {
            kind: TokenType::Assign,
            span: 74..76,
        },
        Token {
            kind: TokenType::Identifier("count".to_string()),
            span: 77..82,
        },
        Token {
            kind: TokenType::Addition,
            span: 83..84,
        },
        Token {
            kind: TokenType::Integer(1),
            span: 85..86,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 86..87,
        },
        Token {
            kind: TokenType::CloseScope,
            span: 91..92,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 92..93,
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
