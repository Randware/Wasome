use lexer::{lex, Token};

#[test]
fn test_all_tokens() {
    let input = r#"
    fn if else loop 
    struct -> <- enum as pub new :: . ;
    "#;

    let expected_tokens = vec![
        Token::Function,
        Token::If,
        Token::Else,
        Token::Loop,
        Token::Struct,
        Token::Return,
        Token::Assign,
        Token::Enum,
        Token::As,
        Token::Public,
        Token::New,
        Token::PathSeperator,
        Token::Dot,
        Token::Semicolon,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_eq!(tokens, expected_tokens);
}
