use std::ops::{Range, RangeFrom};
use chumsky::input::{BorrowInput, ExactSizeInput, SliceInput, ValueInput};
use chumsky::prelude::*;
use lexer::TokenType;
use crate::ParserSpan;

#[derive(Copy, Clone, Debug)]
pub(crate) struct ParserInput<'src> {
    tokens: &'src [Spanned<TokenType, ParserSpan>]
}

impl<'src> ParserInput<'src> {
    pub fn new(tokens: &'src [Spanned<TokenType, ParserSpan>]) -> Self {
        Self { tokens }
    }
}

impl<'src> Input<'src> for ParserInput<'src> {
    type Span = ParserSpan;
    type Token = TokenType;
    type MaybeToken = &'src TokenType;
    type Cursor = usize;
    type Cache = &'src [Spanned<TokenType, ParserSpan>];

    fn begin(self) -> (Self::Cursor, Self::Cache) {
        (0, self.tokens)
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
    unsafe fn next_maybe(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<Self::MaybeToken> {
        if let Some(tok) = cache.get(*cursor) {
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
        let start = cache[(*range.start).min(cache.len()-1)].span;
        let end = cache[(*range.end).min(cache.len()-1)].span;

        // This will panic if the range is reversed
        // Chumsky has no documentation regarding this
        // So let's hope that it will never happen...
        start.merge(end).expect("Range is reversed")
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
            Self::span(cache, range.start..&cache.len())
        }
    }
}

impl<'src> SliceInput<'src> for ParserInput<'src> {
    type Slice = ParserInput<'src>;

    fn full_slice(cache: &mut Self::Cache) -> Self::Slice {
        ParserInput { tokens: cache }
    }

    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursors
    /// don't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn slice(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Slice {
        ParserInput { tokens: &cache[*range.start..*range.end] }
    }

    /// # Safety
    ///
    /// The trait mandates that this is unsafe and states that UB will happen if the cursor
    /// doesn't belong to cache
    ///
    /// This implementation can produce arbitrary behavior, including panics, but **never** UB
    unsafe fn slice_from(cache: &mut Self::Cache, from: RangeFrom<&Self::Cursor>) -> Self::Slice {
        ParserInput { tokens: &cache[*from.start..] }
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
    unsafe fn next_ref(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<&'src Self::Token> {
        unsafe {
            // Safety:
            // This function is only unsafe due to trait requirements and never has UB
            Self::next_maybe(cache, cursor)
        }
    }
}