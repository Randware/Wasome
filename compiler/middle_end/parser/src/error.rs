use crate::ParserSpan;
use chumsky::error::{Error, LabelError};
use chumsky::util::Maybe;
use lexer::TokenType;
use std::fmt::{Display, Formatter};
use uniquevec::UniqueVec;

/// Represents what was expected at a particular position during parsing
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
}

impl Display for ExpectedItem {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedItem::Token(token) => write!(fmt, "{}", token.to_printable_string()),
            ExpectedItem::Identifier => write!(fmt, "identifier"),
            ExpectedItem::String => write!(fmt, "string"),
            ExpectedItem::Decimal => write!(fmt, "decimal"),
            ExpectedItem::Integer => write!(fmt, "integer"),
            ExpectedItem::Char => write!(fmt, "character"),
        }
    }
}

/// Custom parser error type
#[derive(Debug, Clone, PartialEq)]
pub struct ParserError {
    position: ParserSpan,
    found: Option<TokenType>,
    expected: Vec<ExpectedItem>,
}

impl ParserError {
    /// Creates a new parser error
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

    /// Gets the position where the error occurred
    pub fn position(&self) -> &ParserSpan {
        &self.position
    }

    /// Gets the token that was found (None if end of input)
    pub fn found(&self) -> Option<&TokenType> {
        self.found.as_ref()
    }

    /// Gets the list of expected items
    pub fn expected(&self) -> &[ExpectedItem] {
        &self.expected
    }

    /// Adds expected items to the error
    pub fn with_expected(mut self, expected: Vec<ExpectedItem>) -> Self {
        self.expected = expected;
        self
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let found_str = self
            .found
            .as_ref()
            .map(|t| t.to_printable_string())
            .unwrap_or_else(|| "end of input".to_string());

        if self.expected.is_empty() {
            write!(f, "Unexpected {}", found_str)
        } else {
            let expected_str = self
                .expected
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "Expected {}, found {}", expected_str, found_str)
        }
    }
}

impl<'a> Error<'a, crate::input::ParserInput<'a>> for ParserError {
    fn merge(mut self, mut other: Self) -> Self {
        self.expected.append(&mut other.expected);

        self.expected = UniqueVec::from(self.expected).into_iter().collect();
        self
    }
}

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
