//! Indentation management for the formatter.

use crate::constants::INDENT;

/// Tracks and generates indentation strings.
#[derive(Debug, Default)]
pub struct IndentTracker {
    level: usize,
}

impl IndentTracker {
    /// Creates a new indent tracker at level 0.
    pub fn new() -> Self {
        Self { level: 0 }
    }

    /// Increases indentation by one level.
    pub fn increase(&mut self) {
        self.level += 1;
    }

    /// Decreases indentation by one level (minimum 0).
    pub fn decrease(&mut self) {
        self.level = self.level.saturating_sub(1);
    }

    /// Returns the current indentation as a string.
    pub fn as_str(&self) -> String {
        INDENT.repeat(self.level)
    }

    /// Returns true if at top level (indent level 0).
    pub fn is_at_top_level(&self) -> bool {
        self.level == 0
    }
}
