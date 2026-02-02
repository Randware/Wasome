use lexer::TokenType;
use formatter::spacing::requires_space;

#[test]
fn test_space_around_operators() {
    assert!(requires_space(&TokenType::Identifier("x".into()), &TokenType::Addition));
    assert!(requires_space(&TokenType::Addition, &TokenType::Integer(5)));
}

#[test]
fn test_no_space_in_parens() {
    assert!(!requires_space(&TokenType::OpenParen, &TokenType::Identifier("x".into())));
    assert!(!requires_space(&TokenType::Identifier("x".into()), &TokenType::CloseParen));
}

#[test]
fn test_space_after_type() {
    assert!(requires_space(&TokenType::S32, &TokenType::Identifier("x".into())));
}

#[test]
fn test_space_after_comma() {
    assert!(requires_space(&TokenType::ArgumentSeparator, &TokenType::S32));
}

#[test]
fn test_space_after_keyword() {
    assert!(requires_space(&TokenType::Function, &TokenType::Identifier("main".into())));
    assert!(requires_space(&TokenType::If, &TokenType::OpenParen));
}
