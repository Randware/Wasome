use lexer::{Token, lex};

#[test]
fn test_math_operators() {
    let input = r#"
    + - * / %
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Addition,
        Token::Subtraction,
        Token::Multiplication,
        Token::Division,
        Token::Modulo,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_logic_operators() {
    let input = r#"
    < > <= >= != == << >> || | && & !
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::LessThan,
        Token::GreaterThan,
        Token::LessThanEqual,
        Token::GreaterThanEqual,
        Token::NotEqual,
        Token::Comparison,
        Token::LShift,
        Token::RShift,
        Token::Or,
        Token::BitOr,
        Token::And,
        Token::BitAnd,
        Token::Not,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_math_operators_no_spaces() {
    let input = r#"
    +-*/%
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Addition,
        Token::Subtraction,
        Token::Multiplication,
        Token::Division,
        Token::Modulo,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
#[test]
fn test_logic_operators_no_spaces() {
    let input = r#"
    <><=>=!===<<>>|||&&&!
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::LessThan,
        Token::GreaterThan,
        Token::LessThanEqual,
        Token::GreaterThanEqual,
        Token::NotEqual,
        Token::Comparison,
        Token::LShift,
        Token::RShift,
        Token::Or,
        Token::BitOr,
        Token::And,
        Token::BitAnd,
        Token::Not,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_greedy_tokens() {
    let input = r#"
    ||| &&&
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Or,
        Token::BitOr,
        Token::And,
        Token::BitAnd,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input).filter_map(|result| result.ok()).collect();

    assert_eq!(tokens, expected_tokens);
}
