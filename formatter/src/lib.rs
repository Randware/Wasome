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

pub use reorder::{categorize_keyword, ItemCategory};
pub use spacing::requires_space;

use lexer::{lex, Token};
use reorder::{parse_top_level_items, reorder_items};
use formatter::format_tokens;

/// Formats Wasome source code and returns the formatted string.
pub fn format_source(input: &str) -> String {
    let tokens: Vec<Token> = lex(input).filter_map(|r| r.ok()).collect();

    if tokens.is_empty() {
        return String::new();
    }

    // Parse and reorder top-level items
    let items = parse_top_level_items(tokens);
    let reordered = reorder_items(items);

    // Flatten back to tokens
    let tokens: Vec<Token> = reordered.into_iter().flat_map(|item| item.tokens).collect();

    format_tokens(&tokens)
}
