use lexer::{lex, Token};

#[test]
fn test_all_brackets() {
    let input = r#"
    { } ( ) 
    "#;

    let expected_tokens = vec![
        Token::OpenScope,
        Token::CloseScope,
        Token::OpenParen,
        Token::CloseParen,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_eq!(tokens, expected_tokens);
}
