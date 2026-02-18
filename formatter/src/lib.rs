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

use source::SourceMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Formats a Wasome source file and returns the formatted content.
pub fn format_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut sm: SourceMap = SourceMap::new(PathBuf::from("."));
    // load_file returns io::Result<FileID>
    let id = sm.load_file(path)?;
    // get_file must succeed if load_file succeeded (single threaded)
    let file = sm.get_file(&id).expect("File must exist after load");
    Ok(format_source(file.content()))
}

/// Formats a Wasome source file in place.
pub fn format_file_in_place<P: AsRef<Path>>(path: P) -> io::Result<()> {
    let formatted = format_file(&path)?;
    fs::write(path, formatted)?;
    Ok(())
}
