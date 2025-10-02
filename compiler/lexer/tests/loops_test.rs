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
        Token::Identifier,
        Token::Assign,
        Token::Integer,
        Token::Semicolon,
        Token::Loop,
        Token::OpenParen,
        Token::Identifier,
        Token::LessThan,
        Token::Integer,
        Token::CloseParen,
        Token::OpenScope,
        Token::Identifier,
        Token::Assign,
        Token::Identifier,
        Token::Addition,
        Token::Integer,
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
        Token::Identifier,
        Token::Assign,
        Token::Integer,
        Token::Semicolon,
        Token::Identifier,
        Token::LessThan,
        Token::Integer,
        Token::Semicolon,
        Token::Identifier,
        Token::Assign,
        Token::Identifier,
        Token::Addition,
        Token::Integer,
        Token::CloseParen
        ,Token::OpenScope,
        Token::Identifier,
        Token::Assign,
        Token::Identifier,
        Token::Addition,
        Token::Integer,
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
        Token::Identifier,
        Token::Assign,
        Token::Integer,
        Token::Semicolon,
        Token::Identifier,
        Token::LessThan,
        Token::Integer,
        Token::Semicolon,
        Token::Identifier,
        Token::Assign,
        Token::Identifier,
        Token::Addition,
        Token::Integer,
        Token::CloseParen
        ,Token::OpenScope,
        Token::Identifier,
        Token::Assign,
        Token::Identifier,
        Token::Addition,
        Token::Integer,
        Token::CloseScope,
    ];

    let tokens: Vec<_> = lex(input)
        .filter_map(|result| result.ok())
        .collect();

    println!("tokens: {:?}", tokens);
    assert_eq!(tokens, expected_tokens);
}
