use lexer::{TokenType, lex};

#[test]
fn test_all_keywords() {
    let input = r#"
    fn if else loop 
    struct -> <- enum as pub new :: . ; ,
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Function,
        TokenType::If,
        TokenType::Else,
        TokenType::Loop,
        TokenType::StatementSeparator,
        TokenType::Struct,
        TokenType::Return,
        TokenType::Assign,
        TokenType::Enum,
        TokenType::As,
        TokenType::Public,
        TokenType::New,
        TokenType::PathSeparator,
        TokenType::Dot,
        TokenType::Semicolon,
        TokenType::ArgumentSeparator,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_all_keywords_no_spaces() {
    let input = r#"
    fnifelseloopstruct-><-enumaspubnew::.;,
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Identifier("fnifelseloopstruct".to_string()),
        TokenType::Return,
        TokenType::Assign,
        TokenType::Identifier("enumaspubnew".to_string()),
        TokenType::PathSeparator,
        TokenType::Dot,
        TokenType::Semicolon,
        TokenType::ArgumentSeparator,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_access_using_dot() {
    let input = r#"
    struct Point {
        s32 x
        s32 y
    };
    
    Point point <- Point::new()
    
    s32 x_coordinate <- point.x
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Struct,
        TokenType::Identifier("Point".to_string()),
        TokenType::OpenScope,
        TokenType::StatementSeparator,
        TokenType::S32,
        TokenType::Identifier("x".to_string()),
        TokenType::StatementSeparator,
        TokenType::S32,
        TokenType::Identifier("y".to_string()),
        TokenType::StatementSeparator,
        TokenType::CloseScope,
        TokenType::Semicolon,
        TokenType::StatementSeparator,
        TokenType::StatementSeparator,
        TokenType::Identifier("Point".to_string()),
        TokenType::Identifier("point".to_string()),
        TokenType::Assign,
        TokenType::Identifier("Point".to_string()),
        TokenType::PathSeparator,
        TokenType::New,
        TokenType::OpenParen,
        TokenType::CloseParen,
        TokenType::StatementSeparator,
        TokenType::StatementSeparator,
        TokenType::S32,
        TokenType::Identifier("x_coordinate".to_string()),
        TokenType::Assign,
        TokenType::Identifier("point".to_string()),
        TokenType::Dot,
        TokenType::Identifier("x".to_string()),
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
