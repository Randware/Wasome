use lexer::{Token, TokenType, lex};

#[test]
fn test_math_operators() {
    let input = r#"
    + - * / %
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::Addition,
            span: 5..6,
        },
        Token {
            kind: TokenType::Subtraction,
            span: 7..8,
        },
        Token {
            kind: TokenType::Multiplication,
            span: 9..10,
        },
        Token {
            kind: TokenType::Slash,
            span: 11..12,
        },
        Token {
            kind: TokenType::Modulo,
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
fn test_logic_operators() {
    let input = r#"
    < > <= >= != == << >> || | && & !
    "#;

    let expected_tokens = vec![
        Token {
            kind: TokenType::StatementSeparator,
            span: 0..1,
        },
        Token {
            kind: TokenType::LessThan,
            span: 5..6,
        },
        Token {
            kind: TokenType::GreaterThan,
            span: 7..8,
        },
        Token {
            kind: TokenType::LessThanEqual,
            span: 9..11,
        },
        Token {
            kind: TokenType::GreaterThanEqual,
            span: 12..14,
        },
        Token {
            kind: TokenType::NotEqual,
            span: 15..17,
        },
        Token {
            kind: TokenType::Comparison,
            span: 18..20,
        },
        Token {
            kind: TokenType::LessThan,
            span: 21..22,
        },
        Token {
            kind: TokenType::LessThan,
            span: 22..23,
        },
        Token {
            kind: TokenType::GreaterThan,
            span: 24..25,
        },
        Token {
            kind: TokenType::GreaterThan,
            span: 25..26,
        },
        Token {
            kind: TokenType::Or,
            span: 27..29,
        },
        Token {
            kind: TokenType::BitOr,
            span: 30..31,
        },
        Token {
            kind: TokenType::And,
            span: 32..34,
        },
        Token {
            kind: TokenType::BitAnd,
            span: 35..36,
        },
        Token {
            kind: TokenType::Not,
            span: 37..38,
        },
        Token {
            kind: TokenType::StatementSeparator,
            span: 38..39,
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
            span: 0..1,
        },
        Token {
            kind: TokenType::Or,
            span: 5..7,
        },
        Token {
            kind: TokenType::BitOr,
            span: 7..8,
        },
        Token {
            kind: TokenType::And,
            span: 9..11,
        },
        Token {
            kind: TokenType::BitAnd,
            span: 11..12,
        },
        Token {
            kind: TokenType::StatementSeparator,
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
