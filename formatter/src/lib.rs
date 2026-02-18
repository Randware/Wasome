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
mod formatter;
mod indent;
pub mod reorder;
pub mod spacing;

pub use formatter::format_source;
pub use reorder::{categorize_keyword, ItemCategory};
pub use spacing::requires_space;
pub use source::SourceMap;

use std::fs;
use std::io;
use std::path::Path;

/// Formats a Wasome source file and returns the formatted content.
/// 
/// Requires a mutable reference to a `SourceMap` for file loading.
pub fn format_file<P: AsRef<Path>>(sm: &mut SourceMap, path: P) -> io::Result<String> {
    let id = sm.load_file(path)?;
    let file = sm.get_file(&id).expect("File must exist after load");
    Ok(format_source(file.content()))
}

/// Formats a Wasome source file in place.
/// 
/// Requires a mutable reference to a `SourceMap`.
pub fn format_file_in_place<P: AsRef<Path>>(sm: &mut SourceMap, path: P) -> io::Result<()> {
    let formatted = format_file(sm, &path)?;
    fs::write(path, formatted)?;
    Ok(())
}
