//! Spacing rules between token types.

use lexer::TokenType;

/// Tokens that SHOULD NOT have a space BEFORE them
fn no_space_before(token: &TokenType) -> bool {
    matches!(
        token,
        TokenType::CloseParen
            | TokenType::CloseScope
            | TokenType::ArgumentSeparator
            | TokenType::Semicolon
            | TokenType::Dot
            | TokenType::PathSeparator
    )
}

/// Tokens that SHOULD NOT have a space AFTER them
fn no_space_after(token: &TokenType) -> bool {
    matches!(
        token,
        TokenType::OpenParen
            | TokenType::Dot
            | TokenType::PathSeparator
            | TokenType::Not
    )
}

/// Tokens that SHOULD be wrapped in spaces on BOTH sides
fn space_before_after(token: &TokenType) -> bool {
    matches!(
        token,
        TokenType::Addition
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
            | TokenType::LShift
            | TokenType::RShift
            | TokenType::BitOr
            | TokenType::Or
            | TokenType::BitAnd
            | TokenType::And
            | TokenType::Assign
            | TokenType::Return
    )
}

/// Tokens that SHOULD have a space AFTER them
fn space_after(token: &TokenType) -> bool {
    matches!(
        token,
        TokenType::Function
            | TokenType::If
            | TokenType::Else
            | TokenType::Loop
            | TokenType::Struct
            | TokenType::Enum
            | TokenType::As
            | TokenType::Public
            | TokenType::New
            | TokenType::Import

            | TokenType::S8
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
            | TokenType::Char

            | TokenType::ArgumentSeparator
            | TokenType::Semicolon
    )
}

/// Determines if a space is required between two adjacent tokens.
pub fn requires_space(prev: &TokenType, current: &TokenType) -> bool {
    if no_space_before(current) || no_space_after(prev) {
        return false;
    }

    // any match results in an added space
    let space_around_operators = space_before_after(current) || space_before_after(prev);
    let space_after_keyword = space_after(prev);
    let space_before_brace = matches!(current, TokenType::OpenScope);
    let space_after_brace = matches!(prev, TokenType::CloseScope)
        && !matches!(current, TokenType::StatementSeparator);
    let space_between_identifiers = matches!(prev, TokenType::Identifier(_))
        && matches!(current, TokenType::Identifier(_));

    space_around_operators
        || space_after_keyword
        || space_before_brace
        || space_after_brace
        || space_between_identifiers
}


