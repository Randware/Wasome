//! Core formatting logic.
//!
//! Processes tokens and produces formatted output.

use crate::indent::IndentTracker;
use crate::reorder::{categorize_keyword, ItemCategory};
use crate::spacing::requires_space;
use lexer::{Token, TokenType};

/// Checks if a token starts a top-level item.
fn is_top_level_start(token: &TokenType) -> bool {
    matches!(
        categorize_keyword(token),
        ItemCategory::Function | ItemCategory::Struct | ItemCategory::Enum | ItemCategory::Import
    ) || *token == TokenType::Public
}

/// Formatting state for a token stream.
struct TokenFormatter {
    output: String,
    indent: IndentTracker,
    at_line_start: bool,
}

impl TokenFormatter {
    fn new() -> Self {
        Self {
            output: String::new(),
            indent: IndentTracker::new(),
            at_line_start: true,
        }
    }

    /// Formats the given tokens and returns the result.
    fn format(mut self, tokens: &[Token]) -> String {
        for (i, token) in tokens.iter().enumerate() {
            let prev = if i > 0 { Some(&tokens[i - 1]) } else { None };
            let next = tokens.get(i + 1);

            if self.handle_statement_separator(&token.kind) {
                continue;
            }

            self.handle_close_scope_pre(&token.kind);
            self.insert_blank_line_before_top_level(&token.kind, prev);

            let just_indented = self.write_indentation();
            self.write_spacing(just_indented, prev, &token.kind);
            self.write_token(&token.kind);
            self.handle_post_token(&token.kind, prev, next);
        }

        self.ensure_trailing_newline();
        self.output
    }

    /// Emits a newline for statement separators. Returns true if consumed.
    fn handle_statement_separator(&mut self, kind: &TokenType) -> bool {
        if *kind != TokenType::StatementSeparator {
            return false;
        }
        if !self.at_line_start {
            self.push_newline();
        }
        true
    }

    /// Dedents and starts a new line before `}`.
    fn handle_close_scope_pre(&mut self, kind: &TokenType) {
        if *kind != TokenType::CloseScope {
            return;
        }
        self.indent.decrease();
        if !self.at_line_start {
            self.push_newline();
        }
    }

    /// Blank line between top-level items.
    fn insert_blank_line_before_top_level(&mut self, kind: &TokenType, prev: Option<&Token>) {
        if !self.indent.is_at_top_level() || !is_top_level_start(kind) {
            return;
        }
        let Some(prev_token) = prev else { return };
        // Only skip blank line if the previous token is a modifier (e.g. `pub`) or a comment (doc comment).
        // Otherwise, we enforce separation to handle missing newlines (e.g. chaos test).
        if matches!(prev_token.kind, TokenType::Public | TokenType::Comment(_)) {
            return;
        }
        if self.output.is_empty() || self.output.ends_with("\n\n") {
            return;
        }

        while self.output.ends_with(' ') || self.output.ends_with('\t') {
            self.output.pop();
        }
        if !self.output.ends_with('\n') {
            self.output.push('\n');
        }
        self.output.push('\n');
        self.at_line_start = true;
    }

    /// Indents at line start. Returns true if indentation was written.
    fn write_indentation(&mut self) -> bool {
        if !self.at_line_start {
            return false;
        }
        self.output.push_str(&self.indent.as_str());
        self.at_line_start = false;
        true
    }

    /// Applies spacing rules between adjacent tokens.
    fn write_spacing(&mut self, just_indented: bool, prev: Option<&Token>, kind: &TokenType) {
        if just_indented {
            return;
        }
        if let Some(prev_token) = prev {
            if requires_space(&prev_token.kind, kind) {
                self.output.push(' ');
            }
        }
    }

    /// Writes the current token.
    fn write_token(&mut self, kind: &TokenType) {
        self.output.push_str(&kind.to_text());
    }

    /// Newlines and indent changes after a token.
    fn handle_post_token(
        &mut self,
        kind: &TokenType,
        prev: Option<&Token>,
        next: Option<&Token>,
    ) {
        match kind {
            TokenType::OpenScope => {
                self.indent.increase();
                self.push_newline();
            }
            TokenType::CloseScope => {
                match next.map(|t| &t.kind) {
                    Some(TokenType::Else) | Some(TokenType::Semicolon) => {}
                    _ => self.push_newline(),
                }
            }
            TokenType::Semicolon => {
                if let Some(p) = prev {
                    if matches!(p.kind, TokenType::CloseScope) {
                        self.push_newline();
                    }
                }
            }
            TokenType::Comment(_) => {
                self.push_newline();
            }
            _ => {}
        }
    }

    fn push_newline(&mut self) {
        self.output.push('\n');
        self.at_line_start = true;
    }

    fn ensure_trailing_newline(&mut self) {
        if !self.output.ends_with('\n') && !self.output.is_empty() {
            self.output.push('\n');
        }
    }
}

/// Formats a sequence of tokens.
pub fn format_tokens(tokens: &[Token]) -> String {
    TokenFormatter::new().format(tokens)
}
