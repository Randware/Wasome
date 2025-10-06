use lexer::{lex, Token};

#[test]
fn test_all_keywords() {
    let input = r#"
    fn if else loop 
    struct -> <- enum as pub new :: . ; ,
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Function,
        Token::If,
        Token::Else,
        Token::Loop,
        Token::StatementSeparator,
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
        Token::ArgumentSeparator,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_all_keywords_no_spaces() {
    let input = r#"
    fnifelseloopstruct-><-enumaspubnew::.;,
    "#;

    let expected_tokens = vec![
        Token::StatementSeparator,
        Token::Identifier("fnifelseloopstruct".to_string()),
        Token::Return,
        Token::Assign,
        Token::Identifier("enumaspubnew".to_string()),
        Token::PathSeperator,
        Token::Dot,
        Token::Semicolon,
        Token::ArgumentSeparator,
        Token::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();


    assert_eq!(tokens, expected_tokens);
}


