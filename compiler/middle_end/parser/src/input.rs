use std::ops::Range;
use chumsky::prelude::*;
use lexer::TokenType;
use crate::ParserSpan;

pub(crate) struct ParserInput {

}

impl<'src> Input<'src> for &'src ParserInput {
    type Span = ParserSpan;
    type Token = TokenType;
    type MaybeToken = &'src TokenType;
    type Cursor = usize;
    type Cache = &'src [(ParserSpan, TokenType)];

    fn begin(self) -> (Self::Cursor, Self::Cache) {
        todo!()
    }

    fn cursor_location(cursor: &Self::Cursor) -> usize {
        todo!()
    }

    unsafe fn next_maybe(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<Self::MaybeToken> {
        todo!()
    }

    unsafe fn span(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Span {
        todo!()
    }
}