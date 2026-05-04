use lexer::{Token, TokenType};

use crate::classify;
use crate::config::*;

#[derive(Debug, Clone, PartialEq)]
enum ScopeKind {
    Block,
    StructDef,
    EnumDef,
    StructInit,
    StructInitExpanded,
}

pub(crate) struct Formatter {
    output: String,
    indent_level: usize,
    current_line_len: usize,
    pending_empty_lines: usize,
    at_line_start: bool,
    just_opened_scope: bool,
    suppress_newlines: bool,
    prev_kind: Option<TokenType>,
    prev_was_unary_minus: bool,
    scope_stack: Vec<ScopeKind>,
    next_scope: ScopeKind,
    paren_depth: usize,
    generic_depth: usize,
    paren_wrap_mode: bool,
    paren_wrap_depth: usize,
    // True once we emitted a real token.
    has_content: bool,
}

impl Formatter {
    pub(crate) fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            current_line_len: 0,
            pending_empty_lines: 0,
            at_line_start: true,
            just_opened_scope: false,
            suppress_newlines: false,
            prev_kind: None,
            prev_was_unary_minus: false,
            scope_stack: Vec::new(),
            next_scope: ScopeKind::Block,
            paren_depth: 0,
            generic_depth: 0,
            paren_wrap_mode: false,
            paren_wrap_depth: 0,
            has_content: false,
        }
    }

    pub(crate) fn format(mut self, tokens: &[Token]) -> String {
        for i in 0..tokens.len() {
            self.process(tokens, i);
        }
        self.finish()
    }

    fn process(&mut self, tokens: &[Token], index: usize) {
        let curr = tokens[index].kind.clone();

        if matches!(curr, TokenType::StatementSeparator) {
            if !self.just_opened_scope && !self.suppress_newlines {
                if self.at_line_start
                    && matches!(
                        self.current_scope(),
                        Some(ScopeKind::StructDef) | Some(ScopeKind::EnumDef)
                    )
                {
                    return;
                }
                self.emit_newline();
                self.prev_kind = Some(TokenType::StatementSeparator);
            }
            return;
        }

        self.just_opened_scope = false;
        self.suppress_newlines = false;

        if matches!(self.current_scope(), Some(ScopeKind::StructDef)) {
            self.ensure_blank_line_before_method(&curr, tokens, index);
        }

        match &curr {
            TokenType::CloseScope => self.handle_close_scope(tokens, index),
            TokenType::OpenScope => self.handle_open_scope(tokens, index),
            TokenType::OpenParen => self.handle_open_paren(tokens, index),
            TokenType::CloseParen => self.handle_close_paren(),
            TokenType::OpenGeneric => {
                self.emit_token("[");
                self.generic_depth += 1;
            }
            TokenType::CloseGeneric => {
                self.emit_token("]");
                self.generic_depth = self.generic_depth.saturating_sub(1);
            }
            TokenType::ArgumentSeparator => self.handle_argument_separator(tokens, index),
            TokenType::Subtraction => self.handle_subtraction(),
            other => {
                if self.space_before(other) {
                    self.emit_space();
                }
                let text = other.as_text();
                self.emit_token(&text);
            }
        }

        self.update_scope_context(&curr);
        self.prev_kind = Some(curr.clone());
        if !matches!(curr, TokenType::Subtraction) {
            self.prev_was_unary_minus = false;
        }
    }

    fn emit_token(&mut self, text: &str) {
        if self.at_line_start {
            let empty = if self.has_content {
                self.pending_empty_lines.min(MAX_CONSECUTIVE_EMPTY_LINES)
            } else {
                0
            };
            for _ in 0..empty {
                self.output.push('\n');
            }
            self.pending_empty_lines = 0;

            let indent = INDENT_SIZE * self.indent_level;
            for _ in 0..indent {
                self.output.push(' ');
            }
            self.current_line_len = indent;
            self.at_line_start = false;
        }
        self.output.push_str(text);
        self.current_line_len += text.chars().count();
        self.has_content = true;
    }

    fn emit_space(&mut self) {
        if !self.at_line_start {
            self.output.push(' ');
            self.current_line_len += 1;
        }
    }

    fn emit_newline(&mut self) {
        if self.at_line_start {
            self.pending_empty_lines += 1;
        } else {
            self.output.push('\n');
            self.at_line_start = true;
            self.current_line_len = 0;
        }
    }

    fn ensure_newline(&mut self) {
        if !self.at_line_start {
            self.output.push('\n');
            self.at_line_start = true;
            self.current_line_len = 0;
        }
    }

    // Pull `{` back onto the previous line.
    fn pull_back_to_previous_line(&mut self) {
        self.pending_empty_lines = 0;
        while self.output.ends_with('\n') {
            self.output.pop();
        }
        self.at_line_start = false;
        self.current_line_len = match self.output.rfind('\n') {
            Some(pos) => self.output.len() - pos - 1,
            None => self.output.len(),
        };
    }

    // Decide whether to print a space before the token.
    fn space_before(&self, curr: &TokenType) -> bool {
        let prev = match &self.prev_kind {
            Some(p) => p,
            None => return false,
        };

        if matches!(curr, TokenType::Dot | TokenType::PathSeparator) {
            return false;
        }
        if matches!(prev, TokenType::Dot | TokenType::PathSeparator) {
            return false;
        }

        if matches!(prev, TokenType::OpenParen | TokenType::OpenGeneric) {
            return false;
        }
        if matches!(curr, TokenType::CloseParen | TokenType::CloseGeneric) {
            return false;
        }

        if matches!(curr, TokenType::ArgumentSeparator | TokenType::Semicolon) {
            return false;
        }

        if matches!(prev, TokenType::Not) {
            return false;
        }

        if self.prev_was_unary_minus {
            return false;
        }

        if matches!(curr, TokenType::OpenGeneric) {
            return false;
        }

        if matches!(curr, TokenType::OpenParen) {
            if matches!(prev, TokenType::If | TokenType::Loop) {
                return true;
            }
            if matches!(
                prev,
                TokenType::Assign
                    | TokenType::Return
                    | TokenType::Addition
                    | TokenType::Subtraction
                    | TokenType::Multiplication
                    | TokenType::Slash
                    | TokenType::Modulo
                    | TokenType::LessThan
                    | TokenType::GreaterThan
                    | TokenType::LessThanEqual
                    | TokenType::GreaterThanEqual
                    | TokenType::NotEqual
                    | TokenType::Comparison
                    | TokenType::And
                    | TokenType::Or
                    | TokenType::BitAnd
                    | TokenType::BitOr
                    | TokenType::LShift
                    | TokenType::RShift
                    | TokenType::As
            ) {
                return true;
            }
            return false;
        }

        true
    }

    fn handle_close_scope(&mut self, tokens: &[Token], index: usize) {
        let scope = self.scope_stack.pop();
        match scope.as_ref() {
            Some(ScopeKind::StructInit) => {
                self.emit_space();
                self.emit_token("}");
            }
            _ => {
                if matches!(
                    scope,
                    Some(ScopeKind::Block)
                        | Some(ScopeKind::StructDef)
                        | Some(ScopeKind::EnumDef)
                        | Some(ScopeKind::StructInitExpanded)
                ) {
                    self.indent_level = self.indent_level.saturating_sub(1);
                }
                self.pending_empty_lines = 0;
                self.ensure_newline();
                self.emit_token("}");

                if let Some(TokenType::Else) = self.next_non_separator(tokens, index) {
                    self.suppress_newlines = true;
                } else if self.indent_level == 0 {
                    self.ensure_newline();
                    self.pending_empty_lines = self.pending_empty_lines.max(1);
                }
            }
        }
    }

    fn handle_open_scope(&mut self, tokens: &[Token], index: usize) {
        let scope_kind = self.next_scope.clone();
        self.next_scope = ScopeKind::Block;

        if scope_kind == ScopeKind::StructInit {
            let content_len = self.measure_group_content(tokens, index);
            if self.current_line_len + content_len + 4 <= MAX_LINE_LENGTH {
                self.emit_space();
                self.emit_token("{");
                self.scope_stack.push(ScopeKind::StructInit);
                return;
            }
            self.emit_space();
            self.emit_token("{");
            self.indent_level += 1;
            self.scope_stack.push(ScopeKind::StructInitExpanded);
            self.ensure_newline();
            self.just_opened_scope = true;
            return;
        }

        if self.at_line_start {
            self.pull_back_to_previous_line();
        }
        self.emit_space();
        self.emit_token("{");
        self.indent_level += 1;
        self.scope_stack.push(scope_kind);
        self.ensure_newline();
        self.just_opened_scope = true;
    }

    fn handle_open_paren(&mut self, tokens: &[Token], index: usize) {
        if self.space_before(&TokenType::OpenParen) {
            self.emit_space();
        }
        self.paren_depth += 1;
        self.emit_token("(");

        if !self.paren_wrap_mode && self.should_wrap_parens(tokens, index) {
            self.paren_wrap_mode = true;
            self.paren_wrap_depth = self.paren_depth;
            self.indent_level += 1;
            self.ensure_newline();
        }
    }

    fn handle_close_paren(&mut self) {
        if self.paren_wrap_mode && self.paren_depth == self.paren_wrap_depth {
            self.indent_level = self.indent_level.saturating_sub(1);
            self.ensure_newline();
            self.emit_token(")");
            self.paren_wrap_mode = false;
        } else {
            self.emit_token(")");
        }
        self.paren_depth = self.paren_depth.saturating_sub(1);
    }

    fn handle_argument_separator(&mut self, tokens: &[Token], index: usize) {
        self.emit_token(",");

        if self.should_force_newline_after_comma() {
            self.ensure_newline();
        } else if self.paren_wrap_mode && self.paren_depth == self.paren_wrap_depth {
            let next_len = self.measure_next_chunk(tokens, index + 1);
            if self.current_line_len + 1 + next_len > MAX_LINE_LENGTH {
                self.ensure_newline();
            }
        }
    }

    fn handle_subtraction(&mut self) {
        let is_unary = classify::is_unary_minus_context(self.prev_kind.as_ref());
        if is_unary {
            if self.space_before(&TokenType::Subtraction) {
                self.emit_space();
            }
            self.emit_token("-");
            self.prev_was_unary_minus = true;
        } else {
            self.emit_space();
            self.emit_token("-");
            self.prev_was_unary_minus = false;
        }
    }

    fn update_scope_context(&mut self, curr: &TokenType) {
        match curr {
            TokenType::Struct => self.next_scope = ScopeKind::StructDef,
            TokenType::Enum => self.next_scope = ScopeKind::EnumDef,
            TokenType::New => self.next_scope = ScopeKind::StructInit,
            TokenType::Function | TokenType::If | TokenType::Else | TokenType::Loop => {
                self.next_scope = ScopeKind::Block;
            }
            _ => {}
        }
    }

    // Add a blank line before methods in structs.
    fn ensure_blank_line_before_method(
        &mut self,
        curr: &TokenType,
        tokens: &[Token],
        index: usize,
    ) {
        let is_method_start = match curr {
            TokenType::Function => true,
            TokenType::Public => self
                .next_non_separator(tokens, index)
                .is_some_and(|t| matches!(t, TokenType::Function)),
            _ => return,
        };
        if !is_method_start {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::OpenScope)) {
            return;
        }
        if self.at_line_start && self.pending_empty_lines == 0 {
            self.pending_empty_lines = 1;
        }
    }

    fn current_scope(&self) -> Option<&ScopeKind> {
        self.scope_stack.last()
    }

    // Force newlines for enum and expanded struct-init entries.
    fn should_force_newline_after_comma(&self) -> bool {
        if self.generic_depth > 0 || self.paren_depth > 0 {
            return false;
        }
        matches!(
            self.current_scope(),
            Some(ScopeKind::EnumDef) | Some(ScopeKind::StructInitExpanded)
        )
    }

    // Find the next non-newline token.
    fn next_non_separator<'a>(&self, tokens: &'a [Token], index: usize) -> Option<&'a TokenType> {
        tokens[index + 1..]
            .iter()
            .map(|t| &t.kind)
            .find(|k| !matches!(k, TokenType::StatementSeparator))
    }

    // Rough size estimate for bracket group content.
    fn measure_group_content(&self, tokens: &[Token], open_index: usize) -> usize {
        let open = &tokens[open_index].kind;
        let is_close = |k: &TokenType| match open {
            TokenType::OpenParen => matches!(k, TokenType::CloseParen),
            TokenType::OpenScope => matches!(k, TokenType::CloseScope),
            TokenType::OpenGeneric => matches!(k, TokenType::CloseGeneric),
            _ => false,
        };
        let is_open = |k: &TokenType| std::mem::discriminant(k) == std::mem::discriminant(open);

        let mut depth: usize = 1;
        let mut len: usize = 0;
        let mut counted = false;

        for t in &tokens[open_index + 1..] {
            let k = &t.kind;
            if is_open(k) {
                depth += 1;
            }
            if is_close(k) {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            if matches!(k, TokenType::StatementSeparator) {
                continue;
            }
            if counted {
                len += 1;
            }
            len += k.as_text().len();
            counted = true;
        }
        len
    }

    // Estimate line length from token index onward.
    fn measure_to_end_of_line(&self, tokens: &[Token], from: usize) -> usize {
        let mut len: usize = 0;
        let mut first = true;
        for t in &tokens[from..] {
            let k = &t.kind;
            if matches!(k, TokenType::StatementSeparator) {
                break;
            }
            if matches!(k, TokenType::OpenScope) {
                len += 2;
                break;
            }
            if !first {
                len += 1;
            }
            len += k.as_text().len();
            first = false;
        }
        len
    }

    // Wrap parens when the line is long and comma-separated.
    fn should_wrap_parens(&self, tokens: &[Token], open_index: usize) -> bool {
        if !self.has_commas_at_depth(tokens, open_index, 1) {
            return false;
        }
        let rest = self.measure_to_end_of_line(tokens, open_index);
        self.current_line_len + rest > MAX_LINE_LENGTH
    }

    fn has_commas_at_depth(
        &self,
        tokens: &[Token],
        open_index: usize,
        target_depth: usize,
    ) -> bool {
        let mut depth: usize = 0;
        for t in &tokens[open_index..] {
            match &t.kind {
                TokenType::OpenParen => depth += 1,
                TokenType::CloseParen => {
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                TokenType::ArgumentSeparator if depth == target_depth => return true,
                _ => {}
            }
        }
        false
    }

    // Measure until next comma/close at current depth.
    fn measure_next_chunk(&self, tokens: &[Token], from: usize) -> usize {
        let mut len: usize = 0;
        let mut depth: usize = 0;
        let mut first = true;
        for t in &tokens[from..] {
            let k = &t.kind;
            match k {
                TokenType::OpenParen | TokenType::OpenGeneric | TokenType::OpenScope => {
                    depth += 1;
                }
                TokenType::CloseParen | TokenType::CloseGeneric | TokenType::CloseScope => {
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                }
                TokenType::ArgumentSeparator if depth == 0 => break,
                TokenType::StatementSeparator => continue,
                _ => {}
            }
            if !first {
                len += 1;
            }
            len += k.as_text().len();
            first = false;
        }
        len
    }

    fn finish(mut self) -> String {
        let end = self.output.trim_end().len();
        self.output.truncate(end);
        if !self.output.is_empty() {
            self.output.push('\n');
        }
        self.output
    }
}
