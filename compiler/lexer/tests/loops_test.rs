use lexer::{lex, Token};

#[test]
fn test_infinite_loop() {
    let input = r#"
    loop { }
    "#;

    let expected_tokens = vec![
        Token::Loop,
        Token::OpenScope,
        Token::CloseScope,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    println!("tokens: {:?}", tokens);
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_while_loop() {
    let input = r#"
    u32 count <- 0;
    loop (count < 5) {
       count <- count + 1
    }
    "#;

    let expected_tokens = vec![
        Token::U32,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Integer(0),
        Token::Semicolon,
        Token::Loop,
        Token::OpenParen,
        Token::Identifier("count".to_string()),
        Token::LessThan,
        Token::Integer(5),
        Token::CloseParen,
        Token::OpenScope,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Identifier("count".to_string()),
        Token::Addition,
        Token::Integer(1),
        Token::CloseScope,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    println!("tokens: {:?}", tokens);
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
        Token::Loop,
        Token::OpenParen,
        Token::U32,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Integer(0),
        Token::Semicolon,
        Token::Identifier("count".to_string()),
        Token::LessThan,
        Token::Integer(5),
        Token::Semicolon,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Identifier("count".to_string()),
        Token::Addition,
        Token::Integer(1),
        Token::CloseParen
        ,Token::OpenScope,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Identifier("count".to_string()),
        Token::Addition,
        Token::Integer(1),
        Token::CloseScope,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    println!("tokens: {:?}", tokens);
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
        Token::Loop,
        Token::OpenParen,
        Token::U32,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Integer(0),
        Token::Semicolon,
        Token::Identifier("count".to_string()),
        Token::LessThan,
        Token::Integer(5),
        Token::Semicolon,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Identifier("count".to_string()),
        Token::Addition,
        Token::Integer(1),
        Token::CloseParen
        ,Token::OpenScope,
        Token::Identifier("count".to_string()),
        Token::Assign,
        Token::Identifier("count".to_string()),
        Token::Addition,
        Token::Integer(1),
        Token::CloseScope,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    println!("tokens: {:?}", tokens);
    assert_eq!(tokens, expected_tokens);
}
