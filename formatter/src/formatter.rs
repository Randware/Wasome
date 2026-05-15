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
    prev_non_separator_kind: Option<TokenType>,
    prev_prev_non_separator_kind: Option<TokenType>,
    prev_was_unary_minus: bool,
    scope_stack: Vec<ScopeKind>,
    next_scope: ScopeKind,
    paren_depth: usize,
    generic_depth: usize,
    paren_wrap_mode: bool,
    paren_wrap_depth: usize,
    // Tracks whether any non-separator token has been emitted.
    has_content: bool,
    // Skips the next CloseScope because it was emitted inline.
    skip_next_close_scope: bool,
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
            prev_non_separator_kind: None,
            prev_prev_non_separator_kind: None,
            prev_was_unary_minus: false,
            scope_stack: Vec::new(),
            next_scope: ScopeKind::Block,
            paren_depth: 0,
            generic_depth: 0,
            paren_wrap_mode: false,
            paren_wrap_depth: 0,
            has_content: false,
            skip_next_close_scope: false,
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
            // Collapses newlines inside parens or generics to keep tokens together.
            // Note: prev_kind remains unchanged to preserve spacing rules.
            if self.paren_depth > 0 && !self.paren_wrap_mode {
                return;
            }
            if self.generic_depth > 0 {
                return;
            }
            if self.just_opened_scope {
                // Consumes the first newline after `{` without creating a blank line.
                if self.at_line_start {
                    self.just_opened_scope = false;
                    return;
                }
                if matches!(self.current_scope(), Some(ScopeKind::StructDef | ScopeKind::EnumDef))
                {
                    self.just_opened_scope = false;
                    return;
                }
                self.just_opened_scope = false;
            }
            if !self.suppress_newlines {
                if self.should_collapse_separator(tokens, index) {
                    return;
                }
                self.emit_newline();
                self.prev_kind = Some(TokenType::StatementSeparator);
            }
            return;
        }

        // Drops comments that would break a forced join between tokens.
        // Note: This runs before resetting suppress_newlines.
        if matches!(curr, TokenType::Comment(_)) && self.should_drop_comment(tokens, index) {
            return;
        }

        self.just_opened_scope = false;

        self.suppress_newlines = false;

        if matches!(self.current_scope(), Some(ScopeKind::StructDef)) {
            self.ensure_newline_before_struct_field(&curr, tokens, index);
            self.ensure_blank_line_before_method(&curr, tokens, index);
        }
        if matches!(self.current_scope(), Some(ScopeKind::EnumDef)) {
            self.ensure_newline_before_enum_variant(&curr);
        }
        self.ensure_newline_before_control_flow_statement(&curr);

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
            TokenType::Comment(_) => {
                self.handle_comment(&curr);
            }
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
        if !matches!(curr, TokenType::StatementSeparator) {
            self.prev_prev_non_separator_kind = self.prev_non_separator_kind.clone();
            self.prev_non_separator_kind = Some(curr.clone());
        }
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

    // Pulls `{` back onto the previous line.
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

    // Determines whether to print a space before the token.
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
        if self.skip_next_close_scope {
            self.skip_next_close_scope = false;
            return;
        }
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

                // Uses next_significant to see past dropped comments.
                if let Some(TokenType::Else) = self.next_significant(tokens, index) {
                    self.suppress_newlines = true;
                } else if matches!(self.next_significant(tokens, index), Some(TokenType::Return))
                    && matches!(self.prev_non_separator_kind, Some(TokenType::CloseParen))
                {
                    // Keeps `fn ... ) -> T` on one line.
                    self.suppress_newlines = true;
                } else if matches!(self.next_significant(tokens, index), Some(TokenType::Return)) {
                    // Keeps `-> value` on the next line without extra blanks.
                    self.ensure_newline();
                    self.suppress_newlines = true;
                } else if matches!(self.next_immediate(tokens, index), Some(TokenType::Comment(_))) {
                    // Keeps an immediate comment inline with `}`.
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

        let next_token = self.next_non_separator(tokens, index);
        let has_inline_comment = matches!(next_token, Some(TokenType::Comment(_)));
        let has_inline_close = matches!(next_token, Some(TokenType::CloseScope));

        if scope_kind == ScopeKind::StructInit {
            // Pulls `{` back onto the previous line if separated by newlines.
            if self.at_line_start {
                self.pull_back_to_previous_line();
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
            // Avoids pulling `{` into a `//` comment line.
            if !matches!(self.prev_non_separator_kind, Some(TokenType::Comment(_))) {
                self.pull_back_to_previous_line();
            } else {
                // Prevents blank lines between the comment and `{`.
                self.pending_empty_lines = 0;
            }
        }
        self.emit_space();

        // Emits `{}` inline only when the block is truly empty.
        // Note: Comment-only blocks stay expanded.
        if matches!(self.next_non_separator(tokens, index), Some(TokenType::CloseScope)) {
            if self.current_scope_allows_empty_inline(tokens, index) {
                self.emit_token("{");
                self.emit_token("}");
                self.skip_next_close_scope = true;
                // Inserts a blank line after top-level empty items.
                if self.indent_level == 0 {
                    self.ensure_newline();
                    self.pending_empty_lines = self.pending_empty_lines.max(1);
                }
                return;
            }
        }

        self.emit_token("{");
        self.indent_level += 1;
        self.scope_stack.push(scope_kind);
        if has_inline_comment && !has_inline_close {
            self.ensure_newline();
            self.just_opened_scope = true;
            return;
        }
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
            // Suppresses separators after commas inside expanded struct inits.
            self.suppress_newlines = true;
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

    fn handle_comment(&mut self, curr: &TokenType) {
        if !self.at_line_start {
            let has_space = self
                .output
                .chars()
                .last()
                .is_some_and(|c| c == ' ');
            if !has_space {
                self.emit_space();
            }
        }
        let text = curr.as_text();
        self.emit_token(&text);
        // Ends the current line after `//` comments and preserves a single newline.
        self.ensure_newline();
        self.just_opened_scope = true;
    }

    /// Drops comments that would split tokens that must be joined.
    /// Note: Comments survive only at natural line endings.
    fn should_drop_comment(&self, tokens: &[Token], index: usize) -> bool {
        // Drops comments inside parens or generics where newlines are suppressed.
        if self.paren_depth > 0 || self.generic_depth > 0 {
            return true;
        }

        let prev = self.prev_non_separator_kind.as_ref();
        let next = self.next_significant(tokens, index);
        let next_immediate = self.next_immediate(tokens, index);

        // Drops comments between `}` and `else` to keep `} else {` on one line.
        if matches!(prev, Some(TokenType::CloseScope))
            && matches!(next, Some(TokenType::Else))
        {
            return true;
        }

        // Drops comments between `}` and a following `->` return.
        if matches!(prev, Some(TokenType::CloseScope))
            && matches!(next, Some(TokenType::Return))
        {
            return true;
        }

        // Drops comments between `else` and `{` to keep `} else {` on one line.
        if matches!(prev, Some(TokenType::Else))
            && matches!(next, Some(TokenType::OpenScope))
        {
            return true;
        }

        // Drops comments between `->` and its value.
        if matches!(prev, Some(TokenType::Return))
            && !matches!(next, Some(TokenType::CloseScope))
        {
            return true;
        }

        // Drops comments between tokens that must join.
        if Self::should_join_tokens(prev, next, self.prev_prev_non_separator_kind.as_ref()) {
            return true;
        }

        // Keeps an inline comment immediately after `import`.
        if matches!(prev, Some(TokenType::Import))
            && matches!(next_immediate, Some(TokenType::Comment(_)))
        {
            return false;
        }

        // Drops inline comments after `}` when a return follows later.
        if matches!(prev, Some(TokenType::CloseScope))
            && matches!(next, Some(TokenType::Return))
            && matches!(next_immediate, Some(TokenType::Comment(_)))
        {
            return true;
        }

        false
    }

    fn should_collapse_separator(&mut self, tokens: &[Token], index: usize) -> bool {
        let prev = self.prev_non_separator_kind.as_ref();
        let next = self.next_significant(tokens, index);

        if self.at_line_start {
            // Skips leading blank lines entirely.
            if !self.has_content {
                return true;
            }
            // Skips blank lines immediately after `{` or before `}`.
            if matches!(self.prev_kind, Some(TokenType::OpenScope))
                || matches!(prev, Some(TokenType::OpenScope))
            {
                return true;
            }
            if matches!(next, Some(TokenType::CloseScope)) {
                return true;
            }
        }

        if Self::should_join_tokens(prev, next, self.prev_prev_non_separator_kind.as_ref()) {
            return true;
        }

        false
    }

    /// Returns whether `prev` and `next` must share the same output line.
    /// Note: This is used by separator collapsing and comment dropping.
    fn should_join_tokens(
        prev: Option<&TokenType>,
        next: Option<&TokenType>,
        prev_prev: Option<&TokenType>,
    ) -> bool {
        // Joins if/loop/function with its opening paren.
        if matches!(prev, Some(TokenType::If | TokenType::Loop | TokenType::Function))
            && matches!(next, Some(TokenType::OpenParen))
        {
            return true;
        }

        // Joins fn/struct/enum with the following identifier.
        if matches!(
            prev,
            Some(TokenType::Function | TokenType::Struct | TokenType::Enum)
        ) && matches!(next, Some(TokenType::Identifier(_)))
        {
            return true;
        }

        // Joins `import` with the path or string literal.
        if matches!(prev, Some(TokenType::Import))
            && matches!(next, Some(TokenType::Identifier(_) | TokenType::String(_)))
        {
            return true;
        }

        // Joins `import "path" as name` into one line.
        if matches!(prev, Some(TokenType::String(_)))
            && matches!(prev_prev, Some(TokenType::Import))
        {
            return true;
        }

        if matches!(prev, Some(TokenType::String(_))) && matches!(next, Some(TokenType::As)) {
            return true;
        }

        // Joins `pub` with the following keyword or identifier.
        if matches!(prev, Some(TokenType::Public)) {
            return true;
        }

        // Joins `extern` with the following `fn`.
        if matches!(prev, Some(TokenType::Extern))
            && matches!(next, Some(TokenType::Function))
        {
            return true;
        }

        // Joins `]` directly to `(` for generic function signatures.
        if matches!(prev, Some(TokenType::CloseGeneric))
            && matches!(next, Some(TokenType::OpenParen))
        {
            return true;
        }

        // Joins `]` to a following identifier.
        if matches!(prev, Some(TokenType::CloseGeneric))
            && matches!(next, Some(TokenType::Identifier(_)))
        {
            return true;
        }

        // Joins `]` to a following `::`.
        if matches!(prev, Some(TokenType::CloseGeneric))
            && matches!(next, Some(TokenType::PathSeparator))
        {
            return true;
        }

        // Joins identifiers directly to their generic brackets.
        if matches!(prev, Some(TokenType::Identifier(_)))
            && matches!(next, Some(TokenType::OpenGeneric))
        {
            return true;
        }

        // Joins split declaration tokens onto one line.
        if matches!(prev, Some(p) if classify::is_datatype(p))
            && matches!(next, Some(TokenType::Identifier(_)))
        {
            return true;
        }

        // Joins split assignment tokens onto one line.
        if matches!(prev, Some(TokenType::Identifier(_))) && matches!(next, Some(TokenType::Assign)) {
            return true;
        }
        if matches!(prev, Some(TokenType::Assign)) {
            return true;
        }

        // Joins return values split across lines.
        if matches!(prev, Some(TokenType::Return)) && !matches!(next, Some(TokenType::CloseScope)) {
            return true;
        }

        // Keeps `) -> T` on the same line as the signature.
        if matches!(prev, Some(TokenType::CloseParen)) && matches!(next, Some(TokenType::Return)) {
            return true;
        }

        // Keeps `)` and `{` on the same line.
        if matches!(prev, Some(TokenType::CloseParen))
            && matches!(next, Some(TokenType::OpenScope))
        {
            return true;
        }

        // Keeps a return type and `{` on the same line.
        if matches!(prev, Some(p) if classify::is_datatype(p))
            && matches!(next, Some(TokenType::OpenScope))
        {
            return true;
        }

        // Keeps `} else {` on one line.
        if matches!(prev, Some(TokenType::CloseScope))
            && matches!(next, Some(TokenType::Else))
        {
            return true;
        }
        if matches!(prev, Some(TokenType::Else))
            && matches!(next, Some(TokenType::OpenScope))
        {
            return true;
        }

        // Keeps `as` cast chains together.
        if matches!(prev, Some(TokenType::As)) {
            return true;
        }
        if matches!(next, Some(TokenType::As)) {
            return true;
        }

        // Keeps expression continuations with binary operators on one line.
        if matches!(prev, Some(p) if Self::is_binary_operator(p)) {
            return true;
        }
        if matches!(next, Some(n) if Self::is_binary_operator_except_subtraction(n)) {
            return true;
        }

        // Joins split expressions and calls onto one line.
        if matches!(prev, Some(TokenType::New | TokenType::Dot | TokenType::PathSeparator)) {
            return true;
        }
        if matches!(next, Some(TokenType::Dot | TokenType::PathSeparator)) {
            return true;
        }
        if matches!(prev, Some(TokenType::Identifier(_))) && matches!(next, Some(TokenType::OpenParen)) {
            return true;
        }
        if matches!(prev, Some(TokenType::Identifier(_)))
            && matches!(prev_prev, Some(TokenType::New))
            && matches!(next, Some(TokenType::OpenScope))
        {
            return true;
        }

        // Keeps `,` on the same line as the preceding value.
        if matches!(next, Some(TokenType::ArgumentSeparator)) {
            return true;
        }

        false
    }

    fn is_binary_operator(token: &TokenType) -> bool {
        matches!(
            token,
            TokenType::Addition
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
        )
    }

    fn is_binary_operator_except_subtraction(token: &TokenType) -> bool {
        matches!(
            token,
            TokenType::Addition
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
        )
    }

    fn ensure_newline_before_control_flow_statement(&mut self, curr: &TokenType) {
        if self.generic_depth > 0 || self.paren_depth > 0 {
            return;
        }
        if !matches!(curr, TokenType::If | TokenType::Loop) {
            return;
        }
        if self.at_line_start {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::StatementSeparator | TokenType::OpenScope)) {
            return;
        }
        if matches!(self.prev_non_separator_kind, Some(TokenType::Else)) {
            return;
        }
        self.ensure_newline();
    }

    fn is_struct_field_start(curr: &TokenType) -> bool {
        classify::is_datatype(curr)
    }

    fn ensure_newline_before_struct_field(
        &mut self,
        curr: &TokenType,
        tokens: &[Token],
        index: usize,
    ) {
        // Never fire inside a generic bracket list `[…]` or paren list `(…)`.
        if self.generic_depth > 0 || self.paren_depth > 0 {
            return;
        }
        if !Self::is_struct_field_start(curr) {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::OpenScope)) {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::StatementSeparator)) {
            if self.pending_empty_lines > 0 {
                return;
            }
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::Comment(_))) {
            return;
        }
        if matches!(self.next_significant(tokens, index), Some(TokenType::OpenParen)) {
            return;
        }
        if matches!(self.prev_non_separator_kind, Some(TokenType::Return)) {
            return;
        }
        if matches!(self.prev_non_separator_kind, Some(TokenType::Identifier(_))) {
            if matches!(
                self.prev_prev_non_separator_kind.as_ref(),
                Some(prev_prev) if classify::is_datatype(prev_prev)
            ) || matches!(self.prev_prev_non_separator_kind, Some(TokenType::CloseGeneric))
            {
                if !self.at_line_start {
                    self.ensure_newline();
                }
                return;
            }
        }
        if !self.at_line_start {
            self.ensure_newline();
        }
    }

    fn ensure_newline_before_enum_variant(&mut self, curr: &TokenType) {
        // Never fire inside generic brackets or parens.
        if self.generic_depth > 0 || self.paren_depth > 0 {
            return;
        }
        if !matches!(curr, TokenType::Identifier(_)) {
            return;
        }
        // Enum variants are never separated by blank lines — always discard any
        // accumulated blank lines before emitting the next variant.
        self.pending_empty_lines = 0;
        if matches!(self.prev_kind, Some(TokenType::OpenScope)) {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::StatementSeparator)) {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::Comment(_))) {
            return;
        }
        if !self.at_line_start {
            self.ensure_newline();
        }
    }

    fn current_scope_allows_empty_inline(&self, tokens: &[Token], index: usize) -> bool {
        if matches!(self.current_scope(), Some(ScopeKind::StructInit)) {
            return true;
        }
        if matches!(
            self.prev_kind,
            Some(TokenType::Loop | TokenType::If | TokenType::Else | TokenType::Function)
        ) {
            return false;
        }
        // Allow inline empty blocks only when not at top-level items.
        if self.indent_level > 0 {
            return true;
        }
        if matches!(
            self.prev_kind,
            Some(TokenType::Struct | TokenType::Enum | TokenType::Function)
        ) {
            return false;
        }
        // If next is a close scope and we're inside parentheses, do not inline.
        if self.paren_depth > 0 && matches!(self.next_significant(tokens, index), None) {
            return false;
        }
        false
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
        // Never fire inside generic brackets or parens.
        if self.generic_depth > 0 || self.paren_depth > 0 {
            return;
        }
        let is_method_start = match curr {
            TokenType::Function => true,
            TokenType::Public => self
                .next_significant(tokens, index)
                .is_some_and(|t| matches!(t, TokenType::Function)),
            _ => return,
        };
        if !is_method_start {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::OpenScope)) {
            return;
        }
        if matches!(self.prev_kind, Some(TokenType::StatementSeparator)) {
            if self.pending_empty_lines > 0 {
                return;
            }
        }
        if matches!(self.prev_kind, Some(TokenType::Comment(_))) {
            return;
        }
        if self.at_line_start && self.pending_empty_lines == 0 {
            self.pending_empty_lines = 1;
        }
    }

    fn current_scope(&self) -> Option<&ScopeKind> {
        self.scope_stack.last()
    }

    // Forces newlines for enum entries and expanded struct inits.
    fn should_force_newline_after_comma(&self) -> bool {
        if self.generic_depth > 0 || self.paren_depth > 0 {
            return false;
        }
        if matches!(self.current_scope(), Some(ScopeKind::StructInit)) {
            return true;
        }
        matches!(
            self.current_scope(),
            Some(ScopeKind::EnumDef) | Some(ScopeKind::StructInitExpanded)
        )
    }

    // Finds the next non-newline token, including comments.
    fn next_non_separator<'a>(&self, tokens: &'a [Token], index: usize) -> Option<&'a TokenType> {
        tokens[index + 1..]
            .iter()
            .map(|t| &t.kind)
            .find(|k| !matches!(k, TokenType::StatementSeparator))
    }

    // Finds the next non-newline, non-comment token.
    fn next_significant<'a>(&self, tokens: &'a [Token], index: usize) -> Option<&'a TokenType> {
        tokens[index + 1..]
            .iter()
            .map(|t| &t.kind)
            .find(|k| !matches!(k, TokenType::StatementSeparator | TokenType::Comment(_)))
    }

    // Returns the next token only if it is on the same line.
    fn next_immediate<'a>(&self, tokens: &'a [Token], index: usize) -> Option<&'a TokenType> {
        let next = tokens.get(index + 1).map(|t| &t.kind)?;
        if matches!(next, TokenType::StatementSeparator) {
            None
        } else {
            Some(next)
        }
    }

    // Estimates line length from a token index onward.
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

    // Wraps parens when the line is long and comma-separated.
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

    // Measures until the next comma or close at the current depth.
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
