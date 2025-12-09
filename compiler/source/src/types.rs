use std::{
    cmp,
    ops::{Add, AddAssign, Deref, Sub},
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
pub struct FileID(pub(crate) u32);

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
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    /// Creates a new Span that covers the **convex hull** of both spans.
    ///
    /// This includes the start of the first span, the end of the second span,
    /// and **any gap** in between. This is useful for creating spans that cover
    /// entire expressions (e.g., merging `lhs` and `rhs` to cover `lhs + rhs`).
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

/// A per-line info struct that holds the starting byte and all multi byte chars
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Ordering;

    // --- BytePos Tests ---

    #[test]
    fn test_bytepos_deref() {
        let bp = BytePos(10);
        assert_eq!(*bp, 10);
    }

    #[test]
    fn test_bytepos_arithmetic() {
        let mut bp = BytePos(10);

        assert_eq!(bp + 5, BytePos(15));

        assert_eq!(bp - 5, BytePos(5));

        assert_eq!(BytePos(20) - BytePos(5), 15);

        bp += 10;
        assert_eq!(bp, BytePos(20));
    }

    #[test]
    #[should_panic]
    fn test_bytepos_sub_overflow() {
        // Rust's default u32 behavior panics in debug on overflow
        let _ = BytePos(0) - 1;
    }

    #[test]
    fn test_bytepos_ordering() {
        assert!(BytePos(5) < BytePos(10));
        assert!(BytePos(10) > BytePos(5));
        assert_eq!(BytePos(5), BytePos(5));
    }

    // --- FileID Tests ---

    #[test]
    fn test_fileid_creation_and_span_factory() {
        let fid = FileID(42);
        let span = fid.span(10, 20);

        assert_eq!(span.file_id, fid);
        assert_eq!(span.start, BytePos(10));
        assert_eq!(span.end, BytePos(20));
    }

    // --- Span Tests ---

    #[test]
    fn test_span_accessors() {
        let span = Span {
            file_id: FileID(1),
            start: BytePos(10),
            end: BytePos(20),
        };
        assert_eq!(span.start(), BytePos(10));
        assert_eq!(span.end(), BytePos(20));
    }

    #[test]
    fn test_span_len_and_empty() {
        let fid = FileID(1);

        let normal = fid.span(10, 20);
        assert_eq!(normal.len(), 10);
        assert!(!normal.is_empty());

        let empty = fid.span(10, 10);
        assert_eq!(empty.len(), 0);
        assert!(empty.is_empty());
    }

    #[test]
    fn test_span_contains() {
        let fid = FileID(1);
        let outer = fid.span(10, 30);

        // Valid containment cases
        assert!(outer.contains(fid.span(10, 30))); // Exact match
        assert!(outer.contains(fid.span(15, 25))); // Middle
        assert!(outer.contains(fid.span(10, 15))); // Start boundary
        assert!(outer.contains(fid.span(25, 30))); // End boundary

        // Invalid cases
        assert!(!outer.contains(fid.span(0, 5))); // Before
        assert!(!outer.contains(fid.span(35, 40))); // After
        assert!(!outer.contains(fid.span(9, 30))); // Starts too early
        assert!(!outer.contains(fid.span(10, 31))); // Ends too late

        // Different FileID
        let other_fid = FileID(2);
        assert!(!outer.contains(other_fid.span(15, 25)));
    }

    #[test]
    fn test_span_merge() {
        let fid = FileID(1);
        let s1 = fid.span(10, 20);
        let s2 = fid.span(30, 40); // Gap in middle
        let s3 = fid.span(15, 35); // Overlapping

        // Disjoint merge
        let m1 = s1.merge(s2).unwrap();
        assert_eq!(m1.start, BytePos(10));
        assert_eq!(m1.end, BytePos(40)); // Covers the gap (20-30)

        // Overlapping merge
        let m2 = s1.merge(s3).unwrap();
        assert_eq!(m2.start, BytePos(10));
        assert_eq!(m2.end, BytePos(35));

        // Different file merge
        let other_file = FileID(99).span(10, 20);
        assert_eq!(s1.merge(other_file), None);
    }

    #[test]
    fn test_span_ordering() {
        let fid1 = FileID(1);
        let fid2 = FileID(2);

        // Different Files: FileID is primary sort key
        assert_eq!(fid1.span(100, 200).cmp(&fid2.span(0, 5)), Ordering::Less);

        // Same File: Start pos is secondary key
        assert_eq!(fid1.span(10, 20).cmp(&fid1.span(11, 20)), Ordering::Less);

        // Same Start: End pos is tertiary key
        assert_eq!(fid1.span(10, 20).cmp(&fid1.span(10, 21)), Ordering::Less);

        // Identity
        assert_eq!(fid1.span(10, 20).cmp(&fid1.span(10, 20)), Ordering::Equal);
    }
}
