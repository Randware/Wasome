use crate::ParserSpan;
use chumsky::input::{BorrowInput, ExactSizeInput, SliceInput, ValueInput};
use chumsky::prelude::*;
use lexer::TokenType;
use source::types::{BytePos, FileID};
use std::ops::{Range, RangeFrom};

#[derive(Copy, Clone, Debug)]
// This is a false positive
// Implementing it would cause errors
#[allow(clippy::redundant_pub_crate)]
pub(crate) struct ParserInput<'src> {
    tokens: &'src [Spanned<TokenType, ParserSpan>],
    file_id: FileID,
}

impl<'src> ParserInput<'src> {
    #[must_use]
    pub const fn new(tokens: &'src [Spanned<TokenType, ParserSpan>], file_id: FileID) -> Self {
        Self { tokens, file_id }
    }
}

impl<'src> Input<'src> for ParserInput<'src> {
    type Span = ParserSpan;
    type Token = TokenType;
    type MaybeToken = &'src TokenType;
    type Cursor = usize;
    type Cache = Self;

    fn begin(self) -> (Self::Cursor, Self::Cache) {
        (0, self)
    }

    fn cursor_location(cursor: &Self::Cursor) -> usize {
        *cursor
    }

    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursor
    /// doesn't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, but excluding panics and UB
    unsafe fn next_maybe(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<Self::MaybeToken> {
        // While the map_or suggestion is theoretically possible,
        // it feels hacky to have side effects in there.
        #[allow(clippy::option_if_let_else)]
        if let Some(tok) = cache.tokens.get(*cursor) {
            *cursor += 1;
            Some(&tok.inner)
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursors
    /// don't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn span(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Span {
        if cache.tokens.is_empty() {
            return ParserSpan(cache.file_id.span(0, 0));
        }

        let mut start = cache.tokens[(*range.start).min(cache.tokens.len() - 1)].span;
        // The upper end of the range is exclusive
        // -1 is there to not include the end token (exclusive)
        let end = match range.end {
            0 => BytePos(0),
            end if end == range.start => start.end(),
            end => cache.tokens[(*end - 1).min(cache.tokens.len() - 1)]
                .span
                .end(),
        };

        start.set_end(end);
        start
    }
}

impl<'src> ExactSizeInput<'src> for ParserInput<'src> {
    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursor
    /// doesn't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn span_from(cache: &mut Self::Cache, range: RangeFrom<&Self::Cursor>) -> Self::Span {
        unsafe {
            // Safety:
            // This function is only unsafe due to trait requirements and never has UB
            Self::span(cache, range.start..&cache.tokens.len())
        }
    }
}

impl<'src> SliceInput<'src> for ParserInput<'src> {
    type Slice = Self;

    fn full_slice(cache: &mut Self::Cache) -> Self::Slice {
        *cache
    }

    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursors
    /// don't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn slice(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Slice {
        ParserInput {
            tokens: &cache.tokens[*range.start..*range.end],
            file_id: cache.file_id,
        }
    }

    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursor
    /// doesn't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn slice_from(cache: &mut Self::Cache, from: RangeFrom<&Self::Cursor>) -> Self::Slice {
        ParserInput {
            tokens: &cache.tokens[*from.start..],
            file_id: cache.file_id,
        }
    }
}

impl<'src> ValueInput<'src> for ParserInput<'src> {
    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursor
    /// doesn't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn next(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<Self::Token> {
        unsafe {
            // Safety:
            // This function is only unsafe due to trait requirements and never has UB
            Self::next_maybe(cache, cursor).cloned()
        }
    }
}
impl<'src> BorrowInput<'src> for ParserInput<'src> {
    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursor
    /// doesn't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn next_ref(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<&'src Self::Token> {
        unsafe {
            // Safety:
            // This function is only unsafe due to trait requirements and never has UB
            Self::next_maybe(cache, cursor)
        }
    }
}
