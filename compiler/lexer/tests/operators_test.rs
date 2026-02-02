use lexer::{Token, TokenType, lex};

#[test]
fn test_math_operators() {
    let input = r#"
    + - * / %
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Addition,
            line: 1,
            span: 4..5,
        },
        Token {
            kind: TokenType::Subtraction,
            line: 1,
            span: 6..7,
        },
        Token {
            kind: TokenType::Multiplication,
            line: 1,
            span: 8..9,
        },
        Token {
            kind: TokenType::Slash,
            line: 1,
            span: 10..11,
        },
        Token {
            kind: TokenType::Modulo,
            line: 1,
            span: 12..13,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
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
fn test_logic_operators() {
    let input = r#"
    < > <= >= != == << >> || | && & !
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::LessThan,
            line: 1,
            span: 4..5,
        },
        Token {
            kind: TokenType::GreaterThan,
            line: 1,
            span: 6..7,
        },
        Token {
            kind: TokenType::LessThanEqual,
            line: 1,
            span: 8..10,
        },
        Token {
            kind: TokenType::GreaterThanEqual,
            line: 1,
            span: 11..13,
        },
        Token {
            kind: TokenType::NotEqual,
            line: 1,
            span: 14..16,
        },
        Token {
            kind: TokenType::Comparison,
            line: 1,
            span: 17..19,
        },
        Token {
            kind: TokenType::LShift,
            line: 1,
            span: 20..22,
        },
        Token {
            kind: TokenType::RShift,
            line: 1,
            span: 23..25,
        },
        Token {
            kind: TokenType::Or,
            line: 1,
            span: 26..28,
        },
        Token {
            kind: TokenType::BitOr,
            line: 1,
            span: 29..30,
        },
        Token {
            kind: TokenType::And,
            line: 1,
            span: 31..33,
        },
        Token {
            kind: TokenType::BitAnd,
            line: 1,
            span: 34..35,
        },
        Token {
            kind: TokenType::Not,
            line: 1,
            span: 36..37,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 37..38,
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

/**
This Test showcases that the lexer will always tokenize the longest possible token first.
This is referred to as "greedy tokenization".
*/
#[test]
fn test_greedy_tokens() {
    let input = r#"
    ||| &&&
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            line: 0,
            span: 0..1,
        },
        Token {
            kind: TokenType::Or,
            line: 1,
            span: 4..6,
        },
        Token {
            kind: TokenType::BitOr,
            line: 1,
            span: 6..7,
        },
        Token {
            kind: TokenType::And,
            line: 1,
            span: 8..10,
        },
        Token {
            kind: TokenType::BitAnd,
            line: 1,
            span: 10..11,
        },
        Token {
            kind: TokenType::StatementSeparator,
            line: 1,
            span: 11..12,
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
