//! Wasome Code Formatter
//!
//! A token-preserving code formatter for the Wasome language.
//! Enforces a universal style with no configuration options.
//!
//! # Usage
//!
//! ```rust
//! use formatter::format_source;
//!
//! let formatted = format_source("fn main(){s32 x<-10}");
//! ```

mod constants;
mod engine;
mod indent;
pub mod reorder;
pub mod spacing;

pub use engine::format_source;
pub use reorder::{categorize_keyword, ItemCategory};
pub use spacing::needs_space_before;

use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;

/// Formats a Wasome source file and returns the formatted content.
pub fn format_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut content = String::new();
    fs::File::open(path)?.read_to_string(&mut content)?;
    Ok(format_source(&content))
}

/// Formats a Wasome source file in place.
pub fn format_file_in_place<P: AsRef<Path>>(path: P) -> io::Result<()> {
    let formatted = format_file(&path)?;
    fs::File::create(path)?.write_all(formatted.as_bytes())?;
    Ok(())
}
