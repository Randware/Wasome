//! Reordering logic for file structure.
//!
//! Introduces an absolute order between top-level items:
//! 1. Imports
//! 2. Structs
//! 3. Enums
//! 4. Functions

use lexer::{Token, TokenType};

/// Categories for top-level items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ItemCategory {
    Import = 0,
    Struct = 1,
    Enum = 2,
    Function = 3,
    Other = 4,
}

/// A top-level item with its tokens and category.
#[derive(Debug)]
pub struct TopLevelItem {
    pub category: ItemCategory,
    pub tokens: Vec<Token>,
}

/// Identifies the category of a top-level item based on its first keyword.
pub fn categorize_keyword(token: &TokenType) -> Option<ItemCategory> {
    match token {
        TokenType::Import => Some(ItemCategory::Import),
        TokenType::Struct => Some(ItemCategory::Struct),
        TokenType::Enum => Some(ItemCategory::Enum),
        TokenType::Function => Some(ItemCategory::Function),
        TokenType::Public => None, // Need to look at next token
        _ => Some(ItemCategory::Other),
    }
}

/// Parses tokens into top-level items.
pub fn parse_top_level_items(tokens: Vec<Token>) -> Vec<TopLevelItem> {
    let mut items = Vec::new();
    let mut current_tokens = Vec::new();
    let mut current_category = ItemCategory::Other;
    let mut brace_depth: usize = 0;
    let mut in_item = false;
    
    let mut iter = tokens.into_iter().peekable();

    while let Some(token) = iter.next() {
        match &token.kind {
            TokenType::Import | TokenType::Struct | TokenType::Enum | TokenType::Function => {
                if !in_item && brace_depth == 0 {
                    // Start of a new top-level item
                    if !current_tokens.is_empty() {
                        items.push(TopLevelItem {
                            category: current_category,
                            tokens: std::mem::take(&mut current_tokens),
                        });
                    }
                    current_category =
                        categorize_keyword(&token.kind).unwrap_or(ItemCategory::Other);
                    in_item = true;
                }
                current_tokens.push(token);
            }
            TokenType::Public => {
                if !in_item && brace_depth == 0 {
                    // Public modifier - start collecting but category determined by next token
                    if !current_tokens.is_empty() {
                        items.push(TopLevelItem {
                            category: current_category,
                            tokens: std::mem::take(&mut current_tokens),
                        });
                    }
                    current_category = ItemCategory::Other;
                    in_item = true;
                }
                current_tokens.push(token);
            }
            TokenType::OpenScope => {
                brace_depth += 1;
                current_tokens.push(token);
            }
            TokenType::CloseScope => {
                brace_depth = brace_depth.saturating_sub(1);
                current_tokens.push(token);
                if brace_depth == 0 && in_item {
                    // Check if next token is a semicolon - include it with this item
                    // This handles struct Foo { ... }; correctly
                    if let Some(next) = iter.peek() {
                        if matches!(next.kind, TokenType::Semicolon) {
                            current_tokens.push(iter.next().unwrap());
                        }
                    }
                    // End of top-level item
                    items.push(TopLevelItem {
                        category: current_category,
                        tokens: std::mem::take(&mut current_tokens),
                    });
                    in_item = false;
                    current_category = ItemCategory::Other;
                }
            }
            _ => {
                // Update category if we see a defining keyword right after `pub`.
                //
                // Example: For `pub struct Point { ... }`, at first this turns into `Other`,
                // by iterating to the next token the actual Item is discovered
                if in_item && current_category == ItemCategory::Other {
                    if let Some(cat) = categorize_keyword(&token.kind) {
                        current_category = cat;
                    }
                }
                current_tokens.push(token);
            }
        }
    }

    // Don't forget remaining tokens
    if !current_tokens.is_empty() {
        items.push(TopLevelItem {
            category: current_category,
            tokens: current_tokens,
        });
    }

    items
}

/// Reorders items according to the canonical order, while preserving the relative order between Items of the same kind
pub fn reorder_items(mut items: Vec<TopLevelItem>) -> Vec<TopLevelItem> {
    items.sort_by_key(|item| item.category);
    items
}


