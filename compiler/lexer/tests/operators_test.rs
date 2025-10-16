use lexer::{TokenType, lex};

#[test]
fn test_math_operators() {
    let input = r#"
    + - * / %
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Addition,
        TokenType::Subtraction,
        TokenType::Multiplication,
        TokenType::Division,
        TokenType::Modulo,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_logic_operators() {
    let input = r#"
    < > <= >= != == << >> || | && & !
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::LessThan,
        TokenType::GreaterThan,
        TokenType::LessThanEqual,
        TokenType::GreaterThanEqual,
        TokenType::NotEqual,
        TokenType::Comparison,
        TokenType::LShift,
        TokenType::RShift,
        TokenType::Or,
        TokenType::BitOr,
        TokenType::And,
        TokenType::BitAnd,
        TokenType::Not,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_math_operators_no_spaces() {
    let input = r#"
    +-*/%
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Addition,
        TokenType::Subtraction,
        TokenType::Multiplication,
        TokenType::Division,
        TokenType::Modulo,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_logic_operators_no_spaces() {
    let input = r#"
    <><=>=!===<<>>|||&&&!
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::LessThan,
        TokenType::GreaterThan,
        TokenType::LessThanEqual,
        TokenType::GreaterThanEqual,
        TokenType::NotEqual,
        TokenType::Comparison,
        TokenType::LShift,
        TokenType::RShift,
        TokenType::Or,
        TokenType::BitOr,
        TokenType::And,
        TokenType::BitAnd,
        TokenType::Not,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
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
        TokenType::StatementSeparator,
        TokenType::Or,
        TokenType::BitOr,
        TokenType::And,
        TokenType::BitAnd,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
