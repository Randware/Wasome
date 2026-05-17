use lexer::TokenType;

// Detects primitive types and uppercase user types.
pub fn is_datatype(token: &TokenType) -> bool {
    match token {
        TokenType::S8
        | TokenType::S16
        | TokenType::S32
        | TokenType::S64
        | TokenType::U8
        | TokenType::U16
        | TokenType::U32
        | TokenType::U64
        | TokenType::F32
        | TokenType::F64
        | TokenType::Bool
        | TokenType::Char => true,
        TokenType::Identifier(s) => s.starts_with(|c: char| c.is_uppercase()),
        _ => false,
    }
}

// Determines when `-` should be treated as unary.
pub fn is_unary_minus_context(prev: Option<&TokenType>) -> bool {
    match prev {
        None => true,
        Some(token) => matches!(
            token,
            TokenType::Assign
                | TokenType::Return
                | TokenType::OpenParen
                | TokenType::ArgumentSeparator
                | TokenType::Semicolon
                | TokenType::StatementSeparator
                | TokenType::Addition
                | TokenType::Subtraction
                | TokenType::Multiplication
                | TokenType::Slash
                | TokenType::Modulo
                | TokenType::LessThan
                | TokenType::GreaterThan
                | TokenType::LessThanEqual
                | TokenType::GreaterThanEqual
                | TokenType::NotEqual
                | TokenType::Comparison
                | TokenType::And
                | TokenType::Or
                | TokenType::BitAnd
                | TokenType::BitOr
                | TokenType::LShift
                | TokenType::RShift
                | TokenType::Not
        ),
    }
}
