use lexer::TokenType;
use formatter::needs_space_before;

#[test]
fn test_space_around_operators() {
    assert!(needs_space_before(&TokenType::Identifier("x".into()), &TokenType::Addition));
    assert!(needs_space_before(&TokenType::Addition, &TokenType::Integer(5)));
}

#[test]
fn test_no_space_in_parens() {
    assert!(!needs_space_before(&TokenType::OpenParen, &TokenType::Identifier("x".into())));
    assert!(!needs_space_before(&TokenType::Identifier("x".into()), &TokenType::CloseParen));
}

#[test]
fn test_space_after_type() {
    assert!(needs_space_before(&TokenType::S32, &TokenType::Identifier("x".into())));
}

#[test]
fn test_space_after_comma() {
    assert!(needs_space_before(&TokenType::ArgumentSeparator, &TokenType::S32));
}

#[test]
fn test_space_after_keyword() {
    assert!(needs_space_before(&TokenType::Function, &TokenType::Identifier("main".into())));
    assert!(needs_space_before(&TokenType::If, &TokenType::OpenParen));
}
