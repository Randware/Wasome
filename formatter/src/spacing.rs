//! Spacing rules between token types.
//!
//! Uses token categorization for cleaner, more maintainable logic.

use lexer::TokenType;

// =============================================================================
// Token Categories
// =============================================================================

/// Tokens that forbid a space before them.
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

/// Tokens that forbid a space after them.
fn no_space_after(token: &TokenType) -> bool {
    matches!(
        token,
        TokenType::OpenParen
            | TokenType::Dot
            | TokenType::PathSeparator
            | TokenType::Not
    )
}

/// Tokens that need space on both sides (binary operators).
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

/// Tokens that need space after them (keywords, types, separators).
fn space_after(token: &TokenType) -> bool {
    matches!(
        token,
        // Keywords
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
            // Primitive types
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
            // Separators
            | TokenType::ArgumentSeparator
            | TokenType::Semicolon
    )
}

// =============================================================================
// Spacing Logic
// =============================================================================

/// Determines whether a space should be added before this token.
pub fn needs_space_before(prev: &TokenType, current: &TokenType) -> bool {
    // Rule 1: Never space before certain punctuation
    if no_space_before(current) {
        return false;
    }

    // Rule 2: Never space after certain tokens
    if no_space_after(prev) {
        return false;
    }

    // Rule 3: Space around binary operators
    if space_before_after(current) || space_before_after(prev) {
        return true;
    }

    // Rule 4: Space after keywords, types, and separators
    if space_after(prev) {
        return true;
    }

    // Rule 5: Space before opening brace
    if matches!(current, TokenType::OpenScope) {
        return true;
    }

    // Rule 6: Space after closing brace (for else)
    if matches!(prev, TokenType::CloseScope)
        && !matches!(current, TokenType::StatementSeparator)
    {
        return true;
    }

    // Rule 7: Space between identifiers
    if matches!(prev, TokenType::Identifier(_))
        && matches!(current, TokenType::Identifier(_))
    {
        return true;
    }

    false
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_space_around_operators() {
        assert!(needs_space_before(&TokenType::Identifier("x".into()), &TokenType::Addition));
        assert!(needs_space_before(&TokenType::Addition, &TokenType::Integer(5)));
    }

    #[test]
    fn test_no_space_in_parens() {
        assert!(!needs_space_before(&TokenType::OpenParen, &TokenType::Identifier("x".into())));
        assert!(!needs_space_before(&TokenType::Identifier("x".into()), &TokenType::CloseParen));
    }

    #[test]
    fn test_space_after_type() {
        assert!(needs_space_before(&TokenType::S32, &TokenType::Identifier("x".into())));
    }

    #[test]
    fn test_space_after_comma() {
        assert!(needs_space_before(&TokenType::ArgumentSeparator, &TokenType::S32));
    }

    #[test]
    fn test_space_after_keyword() {
        assert!(needs_space_before(&TokenType::Function, &TokenType::Identifier("main".into())));
        assert!(needs_space_before(&TokenType::If, &TokenType::OpenParen));
    }
}
