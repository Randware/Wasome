// tests/token_values_test.rs
use lexer::{lex, Token};

#[test]
fn test_value_tokens() {
    let input = r#"
    heehee 123 0.456
    "#;

    let mut iter = lex(input);

    if let Some(Ok(Token::Identifier(name))) = iter.next() {
        assert_eq!(name, "heehee");
    } else {
        panic!("Expected Identifier token for 'myVar'");
    }


    if let Some(Ok(Token::Integer(name))) = iter.next() {
        assert_eq!(name, 123);
    } else {
        panic!("Expected Identifier token for 'myVar'");
    }

    if let Some(Ok(Token::Decimal(name))) = iter.next() {
        assert_eq!(name, 0.456);
    } else {
        panic!("Expected Identifier token for 'myVar'");
    }

    // This ensures there are no more tokens
    assert!(iter.next().is_none());
}
