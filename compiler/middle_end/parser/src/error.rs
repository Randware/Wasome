//! # Parser Error Handling
//!
//! This module provides a custom error handling system for the Wasome parser,
//! built on top of the Chumsky parser framework. It defines a specialized error type
//! that provides rich, semantic error messages to help users understand what went
//! wrong during parsing.
//!
//! ## Architecture
//!
//! The error system consists of three main components:
//!
//! 1. **ExpectedItem**: An enum representing what the parser expected at a given position
//! 2. **ParserError**: The main error type implementing Chumsky's error traits
//! 3. **Error Conversion**: Integration with Chumsky's `Error` and `LabelError` traits
//!
//! ## Usage
//!
//! When writing parsers, use `map_err` to add semantic context to parsing errors:
//!
//! ```ignore
//! just(Token::Identifier)
//!     .map_err(|err: ParserError| err.with_expected(vec![ExpectedItem::Identifier]))
//! ```
//!
//! This allows the error system to provide meaningful messages like:
//! "Expected identifier, found 'if'" instead of generic parser errors.

use crate::ParserSpan;
use chumsky::error::{Error, LabelError};
use chumsky::util::Maybe;
use error::diagnostic::{Diagnostic, Snippet};
use lexer::TokenType;
use std::fmt::{Display, Formatter};
use uniquevec::UniqueVec;

/// Error code for parsing errors
pub const PARSING_CODE: &str = "E2002";

/// Represents what was expected at a particular position during parsing.
///
/// This enum provides semantic context for parser errors, allowing the error
/// system to communicate what the parser was expecting when it encountered
/// an unexpected token or end of input.
///
/// # Display
///
/// The `Display` implementation provides user-friendly descriptions suitable
/// for error messages. For example, `ExpectedItem::Identifier` displays as
/// "identifier" and `ExpectedItem::Token(Token::If)` displays as "if keyword".
#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedItem {
    /// A specific token type was expected
    Token(TokenType),
    /// An identifier was expected
    Identifier,
    /// A string literal was expected
    String,
    /// A decimal number was expected
    Decimal,
    /// An integer was expected
    Integer,
    /// A character literal was expected
    Char,
    /// A custom description of what was expected
    Custom(String),
}

/// Display implementation for ExpectedItem.
///
/// Converts each variant to a user-friendly string representation suitable
/// for error messages. Token types use their printable representation from
/// `TokenType::to_printable_string()`, while literal types use lowercase
/// descriptions.
impl Display for ExpectedItem {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedItem::Token(token) => write!(fmt, "{}", token.to_printable_string()),
            ExpectedItem::Identifier => write!(fmt, "identifier"),
            ExpectedItem::String => write!(fmt, "string"),
            ExpectedItem::Decimal => write!(fmt, "decimal"),
            ExpectedItem::Integer => write!(fmt, "integer"),
            ExpectedItem::Char => write!(fmt, "character"),
            ExpectedItem::Custom(s) => write!(fmt, "{}", s),
        }
    }
}

/// Custom parser error type for the Wasome compiler.
///
/// `ParserError` is the main error type used throughout the parser. It implements
/// Chumsky's `Error` and `LabelError` traits, allowing seamless integration with
/// the parser framework while providing rich, semantic error messages.
///
/// # Error Message Format
///
/// The error message format depends on whether expected items are present:
///
/// - Without expected items: "Unexpected {found}"
///   - Example: "Unexpected 'if'" or "Unexpected end of input"
///
/// - With expected items: "Expected {expected}, found {found}"
///   - Example: "Expected identifier, found 'if'"
///   - Example: "Expected integer, decimal, found ')'"
///
/// # Building Errors
///
/// Errors are typically created in two ways:
///
/// 1. **Via Chumsky's `LabelError` trait**: When a primitive parser fails,
///    `expected_found()` is called to create a basic error with no expected items.
///
/// 2. **Via `map_err`**: After a parser, use `Self::map_err` to add semantic context:
///
/// # Merging Errors
///
/// When multiple parsing alternatives fail, Chumsky merges the errors using
/// the `merge` method. This combines all expected items and deduplicates them
/// using `UniqueVec`, providing a comprehensive list of what was expected.
#[derive(Debug, Clone, PartialEq)]
pub struct ParserError {
    position: ParserSpan,
    /// The actually found token
    ///
    /// None means end of input
    found: Option<TokenType>,
    expected: Vec<ExpectedItem>,
}

impl ParserError {
    /// Creates a new parser error with the specified position, found token, and expected items.
    ///
    /// # Arguments
    ///
    /// - `position`: The span where the error occurred
    /// - `found`: The token type that was found, or `None` for end of input
    /// - `expected`: A vector of `ExpectedItem` values describing what was expected
    ///
    /// # Returns
    ///
    /// A new `ParserError` instance with the provided fields.
    pub fn new(
        position: ParserSpan,
        found: Option<TokenType>,
        expected: Vec<ExpectedItem>,
    ) -> Self {
        Self {
            position,
            found,
            expected,
        }
    }

    /// Gets the position (span) where the error occurred.
    ///
    /// This can be used to provide source location information in error reports,
    /// including line number, column, and source file context.
    ///
    /// # Returns
    ///
    /// A reference to the `ParserSpan` for this error.
    pub fn position(&self) -> &ParserSpan {
        &self.position
    }

    /// Gets the token type that was found when the error occurred.
    ///
    /// Returns `None` if the error was caused by unexpected end of input.
    ///
    /// # Returns
    ///
    /// An `Option` containing a reference to the `TokenType` that was found,
    /// or `None` if end of input was encountered.
    pub fn found(&self) -> Option<&TokenType> {
        self.found.as_ref()
    }

    /// Gets the list of expected items for this error.
    ///
    /// The expected items describe what the parser was expecting at the error
    /// position. This list may be empty if the error was created without
    /// semantic context (e.g., from Chumsky's default error handling).
    ///
    /// # Returns
    ///
    /// A slice of `ExpectedItem` values describing what was expected.
    pub fn expected(&self) -> &[ExpectedItem] {
        &self.expected
    }

    /// Adds expected items to the error, replacing any existing expected items.
    ///
    /// This method is typically used with `map_err` to add semantic context
    /// to parser errors. It takes ownership of the error and returns a new
    /// error with the updated expected items.
    ///
    /// # Arguments
    ///
    /// - `expected`: A vector of `ExpectedItem` values to set as the expected items
    ///
    /// # Returns
    ///
    /// A new `ParserError` with the updated expected items.

    pub fn with_expected(mut self, expected: Vec<ExpectedItem>) -> Self {
        self.expected = expected;
        self
    }
}

/// Chumsky `Error` trait implementation for ParserError.
///
/// This implementation enables `ParserError` to work with Chumsky's parser combinators.
/// The `merge` method is called when multiple parsing alternatives fail, combining
/// their errors into a single comprehensive error.
///
/// # Merging Behavior
///
/// When merging two errors:
///
/// 1. All expected items from both errors are combined
/// 2. Duplicate expected items are removed
/// 3. The position and found token from the first error are preserved
///
/// This allows Chumsky to provide helpful error messages like:
/// "Expected '{', ';', '(', '[', found '}'" when multiple alternatives fail.
/// ```
impl<'a> Error<'a, crate::input::ParserInput<'a>> for ParserError {
    fn merge(mut self, mut other: Self) -> Self {
        self.expected.append(&mut other.expected);

        self.expected = UniqueVec::from(self.expected).into_iter().collect();
        self
    }
}

/// Chumsky `LabelError` trait implementation for ParserError.
///
/// This trait is implemented for `ParserError` to handle errors from Chumsky's
/// primitive parsers. The `expected_found` method is called when a parser fails
/// to match, creating a basic error with an empty expected vec.
///     - Note that due to chumsky limitations, this can't be changed
///
/// # Behavior
///
/// The `expected_found` method:
///
/// 1. Extracts the found token from the `Maybe` wrapper (handling both owned and
///    borrowed token references)
/// 2. Creates a `ParserError` with the span and found token
/// 3. Sets the expected list to empty (semantic context is added via `map_err`)
/// ```
impl<'a> LabelError<'a, crate::input::ParserInput<'a>, chumsky::DefaultExpected<'a, TokenType>>
    for ParserError
{
    fn expected_found<Iter: IntoIterator<Item = chumsky::DefaultExpected<'a, TokenType>>>(
        _expected: Iter,
        found: Option<Maybe<TokenType, &'a TokenType>>,
        span: ParserSpan,
    ) -> Self {
        let found_token = found.map(|maybe_ref| match maybe_ref {
            Maybe::Val(t) => t,
            Maybe::Ref(t) => t.clone(),
        });

        ParserError {
            position: span,
            found: found_token,
            expected: Vec::new(),
        }
    }
}

/// Convert `ParserError` into a `Diagnostic`.
///
/// This implementation transforms a parser error into a diagnostic with
/// formatted error messages, source location information, and helpful hints.
///
/// # Error Message Format
///
/// The message format depends on the expected items:
///
/// - No expected items: "Unexpected {found}"
/// - Single expected item: "Expected {expected}, found {found}"
impl Into<Diagnostic> for ParserError {
    fn into(self) -> Diagnostic {
        let span = *self.position();
        let found_str = self
            .found()
            .map(|t| t.to_printable_string())
            .unwrap_or_else(|| "end of input".to_string());

        let msg = if self.expected().is_empty() {
            format!("Unexpected {}", found_str)
        } else {
            let expected_str = self
                .expected()
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("Expected {}, found {}", expected_str, found_str)
        };

        Diagnostic::builder()
            .message("Token mismatch")
            .code(PARSING_CODE)
            .snippet(
                Snippet::builder()
                    .file(span.0.file_id)
                    .primary(span.0.start..span.0.end, msg)
                    .build(),
            )
            .help("Provide a valid token".to_string())
            .build()
    }
}
