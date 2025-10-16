use lexer::{TokenType, lex};

#[test]
fn test_infinite_loop() {
    let input = r#"
    loop { }
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Loop,
        TokenType::OpenScope,
        TokenType::CloseScope,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_while_loop() {
    let input = r#"
    u32 count <- 0
    loop (count < 5) {
       count <- count + 1
    }
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::U32,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Integer(0),
        TokenType::StatementSeparator,
        TokenType::Loop,
        TokenType::OpenParen,
        TokenType::Identifier("count".to_string()),
        TokenType::LessThan,
        TokenType::Integer(5),
        TokenType::CloseParen,
        TokenType::OpenScope,
        TokenType::StatementSeparator,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Identifier("count".to_string()),
        TokenType::Addition,
        TokenType::Integer(1),
        TokenType::StatementSeparator,
        TokenType::CloseScope,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_for_loop() {
    let input = r#"
    loop (u32 count <- 0; count < 5; count <- count + 1 ) {
       count <- count + 1
    }
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Loop,
        TokenType::OpenParen,
        TokenType::U32,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Integer(0),
        TokenType::Semicolon,
        TokenType::Identifier("count".to_string()),
        TokenType::LessThan,
        TokenType::Integer(5),
        TokenType::Semicolon,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Identifier("count".to_string()),
        TokenType::Addition,
        TokenType::Integer(1),
        TokenType::CloseParen,
        TokenType::OpenScope,
        TokenType::StatementSeparator,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Identifier("count".to_string()),
        TokenType::Addition,
        TokenType::Integer(1),
        TokenType::StatementSeparator,
        TokenType::CloseScope,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_for_loop_no_spaces() {
    let input = r#"
    loop(u32 count<-0;count<5;count<-count+1){
       count<-count+1
    }
    "#;

    let expected_tokens = vec![
        TokenType::StatementSeparator,
        TokenType::Loop,
        TokenType::OpenParen,
        TokenType::U32,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Integer(0),
        TokenType::Semicolon,
        TokenType::Identifier("count".to_string()),
        TokenType::LessThan,
        TokenType::Integer(5),
        TokenType::Semicolon,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Identifier("count".to_string()),
        TokenType::Addition,
        TokenType::Integer(1),
        TokenType::CloseParen,
        TokenType::OpenScope,
        TokenType::StatementSeparator,
        TokenType::Identifier("count".to_string()),
        TokenType::Assign,
        TokenType::Identifier("count".to_string()),
        TokenType::Addition,
        TokenType::Integer(1),
        TokenType::StatementSeparator,
        TokenType::CloseScope,
        TokenType::StatementSeparator,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .map(|token| token.kind)
        .collect();

    assert_eq!(tokens, expected_tokens);
}
