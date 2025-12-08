use std::{
    cmp,
    ops::{Add, AddAssign, Deref, Mul, Sub, SubAssign},
};

use crate::source::SourceFile;

/// The byte offset of the file's content
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct BytePos(pub u32);

impl Deref for BytePos {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Add<u32> for BytePos {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        BytePos(self.0 + rhs)
    }
}

impl Sub<u32> for BytePos {
    type Output = Self;

    fn sub(self, rhs: u32) -> Self::Output {
        BytePos(self.0 - rhs)
    }
}

impl Sub<Self> for BytePos {
    type Output = u32;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 - rhs.0
    }
}

impl AddAssign<u32> for BytePos {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

/// A handle to a [`SourceFile`]
///
/// # Warning
///
/// The [`FileID`] is dependent on the [`SourceMap`] and *not universally usable*
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileID(pub u32);

impl FileID {
    /// Creates a Span inside this file from [`BytePos`]
    ///
    /// # Arguments
    ///
    /// * `start` - The [pos](BytePos) where the span begins (0-Based)
    /// * `end` - The [pos](BytePos) where the span ends (0-Based *AND EXCLUSIVE*)
    pub fn span(self, start: u32, end: u32) -> Span {
        Span {
            file_id: self,
            start: BytePos(start),
            end: BytePos(end),
        }
    }
}
/// A handle to a file which is incredibly cheap to copy, store and pass around
///
/// * `start` - is inclusive
/// * `end`  - is exclusive
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub file_id: FileID,
    pub start: BytePos,
    pub end: BytePos,
}

impl Span {
    /// Gets the start [`BytePos`] of the [`Span`]
    /// _(0-Based indexing)_
    pub fn start(&self) -> BytePos {
        self.start
    }

    /// Gets the end [`BytePos`] of the [`Span`]
    /// _(0-Based indexing)_
    pub fn end(&self) -> BytePos {
        self.end
    }

    /// Gets the length of the span
    pub fn len(&self) -> u32 {
        self.end.0 - self.start.0
    }

    /// Checks if the span is a zero-width "cursor" position
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Checks if this [`Span`] contains the given one
    pub fn contains(&self, other: Span) -> bool {
        self.file_id == other.file_id && self.start <= other.start && self.end >= other.end
    }

    /// Merges two spans to cover the entire __range__ from the start of the first
    /// to the end of the second.
    ///
    /// # Returns
    ///
    /// * `Some(Span)` - The merged span covering the union of both ranges.
    /// * `None` - If the spans are from different files (They have different FileIds)
    pub fn merge(self, other: Span) -> Option<Span> {
        if self.file_id != other.file_id {
            return None;
        }

        Some(Span {
            file_id: self.file_id,
            start: cmp::min(self.start, other.start),
            end: cmp::max(self.end, other.end),
        })
    }
}

/// Meta data about a [`MultiByteChar`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiByteChar {
    /// The byte offset from the start of the line
    pub(crate) pos_on_line: u32,

    /// The number of bytes this char occupies (2, 3, or 4)
    pub(crate) byte_len: u8,

    // Sum of all (byte_len - 1) for this and previous chars on the line
    pub(crate) accumulated_gap: u32,
}

/// A per-line info struct that holds the starting byte and all mutli byte chars
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineInfo {
    pub(crate) line_start: BytePos,
    pub(crate) multi_byte_chars: Vec<MultiByteChar>,
}

// A human-readable location
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location<'a> {
    pub file: &'a SourceFile,
    pub line: u32, // 1-based
    pub col: u32,  // 1-based
}

impl<'a> Location<'a> {
    /// 1 based
    pub fn new(file: &'a SourceFile, line: u32, col: u32) -> Self {
        Self { file, line, col }
    }
}
