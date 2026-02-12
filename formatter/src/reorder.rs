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
pub fn categorize_keyword(token: &TokenType) -> ItemCategory {
    match token {
        TokenType::Import => ItemCategory::Import,
        TokenType::Struct => ItemCategory::Struct,
        TokenType::Enum => ItemCategory::Enum,
        TokenType::Function => ItemCategory::Function,
        _ => ItemCategory::Other,
    }
}

struct TopLevelParser {
    items: Vec<TopLevelItem>,
    current_tokens: Vec<Token>,
    current_category: ItemCategory,
    brace_depth: usize,
    in_item: bool,
}

impl TopLevelParser {
    fn new() -> Self {
        Self {
            items: Vec::new(),
            current_tokens: Vec::new(),
            current_category: ItemCategory::Other,
            brace_depth: 0,
            in_item: false,
        }
    }

    /// Pushes the current token buffer as a finished item and resets state.
    fn finish_current_item(&mut self) {
        if !self.current_tokens.is_empty() {
            self.items.push(TopLevelItem {
                category: self.current_category,
                tokens: std::mem::take(&mut self.current_tokens),
            });
        }
        self.in_item = false;
        self.current_category = ItemCategory::Other;
    }

    /// Begins a new top-level item, flushing any buffered tokens first.
    fn start_new_item(&mut self, category: ItemCategory) {
        self.finish_current_item();
        self.current_category = category;
        self.in_item = true;
    }

    /// Updates category from `Other` when encountering the real keyword after `pub`.
    fn try_update_category(&mut self, token: &TokenType) {
        if self.in_item && self.current_category == ItemCategory::Other {
            let cat = categorize_keyword(token);
            if cat != ItemCategory::Other {
                self.current_category = cat;
            }
        }
    }

    fn handle_keyword(&mut self, token: Token) {
        if !self.in_item && self.brace_depth == 0 {
            self.start_new_item(categorize_keyword(&token.kind));
        } else {
            self.try_update_category(&token.kind);
        }
        self.current_tokens.push(token);
    }

    fn handle_public(&mut self, token: Token) {
        if !self.in_item && self.brace_depth == 0 {
            // Category is unknown until we see the next keyword
            // Example: For `pub struct Point { ... }`, at first this turns into `Other`,
            // by iterating to the next token the actual Item is discovered
            self.start_new_item(ItemCategory::Other);
        }
        self.current_tokens.push(token);
    }

    fn handle_open_scope(&mut self, token: Token) {
        self.brace_depth += 1;
        self.current_tokens.push(token);
    }

    fn handle_close_scope(&mut self, token: Token) {
        self.brace_depth = self.brace_depth.saturating_sub(1);
        self.current_tokens.push(token);
        if self.brace_depth == 0 && self.in_item {
            self.finish_current_item();
        }
    }

    fn handle_other(&mut self, token: Token) {
        self.try_update_category(&token.kind);
        self.current_tokens.push(token);
    }
}

/// Parses tokens into top-level items.
pub fn parse_top_level_items(tokens: Vec<Token>) -> Vec<TopLevelItem> {
    let mut parser = TopLevelParser::new();

    for token in tokens {
        match &token.kind {
            TokenType::Import | TokenType::Struct | TokenType::Enum | TokenType::Function => {
                parser.handle_keyword(token);
            }
            TokenType::Public => parser.handle_public(token),
            TokenType::OpenScope => parser.handle_open_scope(token),
            TokenType::CloseScope => parser.handle_close_scope(token),
            _ => parser.handle_other(token),
        }
    }

    // Flush remaining tokens
    parser.finish_current_item();
    parser.items
}

/// Reorders items according to the canonical order, while preserving the relative order between Items of the same kind
pub fn reorder_items(mut items: Vec<TopLevelItem>) -> Vec<TopLevelItem> {
    items.sort_by_key(|item| item.category);
    items
}
