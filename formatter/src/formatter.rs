use lexer::{Token, TokenType};

use crate::classify;
use crate::config::*;

#[derive(Debug, Clone, PartialEq)]
enum ScopeKind {
    Block,
    StructDef,
    EnumDef,
    StructInit,
}

#[derive(Debug, Clone, PartialEq)]
struct OutputState {
    output: String,
    current_line_len: usize,
    pending_empty_lines: usize,
    at_line_start: bool,
    has_content: bool,
    pending_inline_comment: Option<PendingComment>,
}

#[derive(Debug, Clone, PartialEq)]
struct PendingComment {
    text: String,
    needs_space: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct ScopeState {
    indent_level: usize,
    just_opened_scope: bool,
    suppress_newlines: bool,
    scope_stack: Vec<ScopeKind>,
    next_scope: ScopeKind,
    skip_next_close_scope: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct TokenState {
    prev_kind: Option<TokenType>,
    prev_non_separator_kind: Option<TokenType>,
    prev_prev_non_separator_kind: Option<TokenType>,
    prev_was_unary_minus: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct DepthState {
    paren_depth: usize,
    generic_depth: usize,
}

pub(crate) struct Formatter {
    output: OutputState,
    scope: ScopeState,
    tokens: TokenState,
    depth: DepthState,
}

impl Formatter {
    pub(crate) fn new() -> Self {
        Self {
            output: OutputState {
                output: String::new(),
                current_line_len: 0,
                pending_empty_lines: 0,
                at_line_start: true,
                has_content: false,
                pending_inline_comment: None,
            },
            scope: ScopeState {
                indent_level: 0,
                just_opened_scope: false,
                suppress_newlines: false,
                scope_stack: Vec::new(),
                next_scope: ScopeKind::Block,
                skip_next_close_scope: false,
            },
            tokens: TokenState {
                prev_kind: None,
                prev_non_separator_kind: None,
                prev_prev_non_separator_kind: None,
                prev_was_unary_minus: false,
            },
            depth: DepthState {
                paren_depth: 0,
                generic_depth: 0,
            },
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
            if self.depth.paren_depth > 0 {
                return;
            }
            if self.depth.generic_depth > 0 {
                return;
            }
            if matches!(self.current_scope(), Some(ScopeKind::StructInit)) {
                return;
            }
            if self.scope.just_opened_scope {
                // Consumes the first newline after `{` without creating a blank line.
                let should_skip = self.output.at_line_start
                    || matches!(self.current_scope(), Some(ScopeKind::StructDef | ScopeKind::EnumDef));
                self.scope.just_opened_scope = false;
                if should_skip {
                    return;
                }
            }
            if !self.scope.suppress_newlines {
                if self.should_collapse_separator(tokens, index) {
                    return;
                }
                self.emit_newline();
                self.tokens.prev_kind = Some(TokenType::StatementSeparator);
            }
            return;
        }

        // Drops comments that would break a forced join between tokens.
        // Note: This runs before resetting suppress_newlines.
        if matches!(curr, TokenType::Comment(_)) && self.should_drop_comment(tokens, index) {
            return;
        }

        self.scope.just_opened_scope = false;
        self.scope.suppress_newlines = false;

        self.prepare_layout(&curr, tokens, index);
        self.emit_current_token(&curr, tokens, index);
        self.update_scope_context(&curr);
        self.update_token_state(&curr);
    }

    fn prepare_layout(&mut self, curr: &TokenType, tokens: &[Token], index: usize) {
        if matches!(self.current_scope(), Some(ScopeKind::StructDef)) {
            self.ensure_newline_before_struct_field(curr, tokens, index);
            self.ensure_blank_line_before_method(curr, tokens, index);
        }
        if matches!(self.current_scope(), Some(ScopeKind::EnumDef)) {
            self.ensure_newline_before_enum_variant(curr);
        }
        self.ensure_newline_before_control_flow_statement(curr);
        self.ensure_newline_before_return_after_close_scope(curr);
    }

    fn ensure_newline_before_return_after_close_scope(&mut self, curr: &TokenType) {
        if !matches!(curr, TokenType::Return) {
            return;
        }
        if matches!(self.output.pending_inline_comment, Some(_)) {
            return;
        }
        if matches!(self.tokens.prev_non_separator_kind, Some(TokenType::CloseScope))
            && !self.output.at_line_start
        {
            // If the last token emitted was a comment, we shouldn't emit an extra newline.
            // But we don't have a reliable way to check that here, and if `at_line_start` is true we're safe.
            self.ensure_newline();
        }
    }

    fn emit_current_token(&mut self, curr: &TokenType, tokens: &[Token], index: usize) {
        match curr {
            TokenType::CloseScope => self.handle_close_scope(tokens, index),
            TokenType::OpenScope => self.handle_open_scope(tokens, index),
            TokenType::OpenParen => self.handle_open_paren(),
            TokenType::CloseParen => self.handle_close_paren(),
            TokenType::OpenGeneric => {
                let text = TokenType::OpenGeneric.as_text();
                self.emit_token(&text);
                self.depth.generic_depth += 1;
            }
            TokenType::CloseGeneric => {
                let text = TokenType::CloseGeneric.as_text();
                self.emit_token(&text);
                self.depth.generic_depth = self.depth.generic_depth.saturating_sub(1);
            }
            TokenType::ArgumentSeparator => self.handle_argument_separator(),
            TokenType::Subtraction => self.handle_subtraction(),
            TokenType::Comment(_) => {
                self.handle_comment(curr);
            }
            other => {
                if self.space_before(other) {
                    self.emit_space();
                }
                let text = other.as_text();
                self.emit_token(&text);
            }
        }
    }

    fn update_token_state(&mut self, curr: &TokenType) {
        if matches!(curr, TokenType::StatementSeparator) {
            self.output.pending_inline_comment = None;
        }
        self.tokens.prev_kind = Some(curr.clone());
        if !matches!(curr, TokenType::StatementSeparator) {
            self.tokens.prev_prev_non_separator_kind = self.tokens.prev_non_separator_kind.clone();
            self.tokens.prev_non_separator_kind = Some(curr.clone());
        }
        if !matches!(curr, TokenType::Subtraction) {
            self.tokens.prev_was_unary_minus = false;
        }
    }

    fn emit_token(&mut self, text: &str) {
        self.flush_pending_inline_comment();
        if self.output.at_line_start {
            let empty = if self.output.has_content {
                self.output
                    .pending_empty_lines
                    .min(MAX_CONSECUTIVE_EMPTY_LINES)
            } else {
                0
            };
            for _ in 0..empty {
                self.output.output.push('\n');
            }
            self.output.pending_empty_lines = 0;

            let indent = INDENT_SIZE * self.scope.indent_level;
            for _ in 0..indent {
                self.output.output.push(' ');
            }
            self.output.current_line_len = indent;
            self.output.at_line_start = false;
        }
        self.output.output.push_str(text);
        self.output.current_line_len += text.chars().count();
        self.output.has_content = true;
    }

    fn emit_space(&mut self) {
        self.flush_pending_inline_comment();
        if !self.output.at_line_start {
            self.output.output.push(' ');
            self.output.current_line_len += 1;
        }
    }

    fn emit_newline(&mut self) {
        let had_pending = self.output.pending_inline_comment.is_some();
        self.flush_pending_inline_comment();
        if self.output.at_line_start {
            if !had_pending {
                self.output.pending_empty_lines += 1;
            }
        } else {
            self.push_newline();
        }
    }

    fn ensure_newline(&mut self) {
        self.flush_pending_inline_comment();
        if !self.output.at_line_start {
            self.push_newline();
        }
    }

    fn flush_pending_inline_comment(&mut self) {
        if let Some(comment) = self.output.pending_inline_comment.take() {
            if comment.needs_space && !self.output.at_line_start {
                self.output.output.push(' ');
            }
            self.output.output.push_str(&comment.text);
            self.push_newline();
        }
    }

    fn push_newline(&mut self) {
        self.output.output.push('\n');
        self.output.at_line_start = true;
        self.output.current_line_len = 0;
    }

    // Pulls `{` back onto the previous line.
    fn pull_back_to_previous_line(&mut self) {
        self.output.pending_empty_lines = 0;
        while self.output.output.ends_with('\n') {
            self.output.output.pop();
        }
        self.output.at_line_start = false;
        self.output.current_line_len = match self.output.output.rfind('\n') {
            Some(pos) => self.output.output.len() - pos - 1,
            None => self.output.output.len(),
        };
    }

    // Determines whether to print a space before the token.
    fn space_before(&self, curr: &TokenType) -> bool {
        let prev = match &self.tokens.prev_kind {
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

        if self.tokens.prev_was_unary_minus {
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
        if self.scope.skip_next_close_scope {
            self.scope.skip_next_close_scope = false;
            return;
        }
        let scope = self.scope.scope_stack.pop();
        let should_dedent = matches!(
            scope,
            Some(ScopeKind::Block) | Some(ScopeKind::StructDef) | Some(ScopeKind::EnumDef)
        );
        if should_dedent {
            self.scope.indent_level = self.scope.indent_level.saturating_sub(1);
        }

        let is_struct_init = matches!(scope, Some(ScopeKind::StructInit));
        if is_struct_init {
            if self.output.at_line_start {
                // We just had a newline (likely from a comment). We must NOT emit space.
                // We just indent.
                let indent = crate::config::INDENT_SIZE * self.scope.indent_level;
                for _ in 0..indent {
                    self.output.output.push(' ');
                }
                self.output.current_line_len = indent;
                self.output.at_line_start = false;
            } else {
                self.emit_space();
            }
            self.emit_token("}");
            return;
        }

        self.output.pending_empty_lines = 0;
        self.ensure_newline();
        self.emit_token("}");

        // Uses next_significant to see past dropped comments.
        let next_significant = self.next_significant(tokens, index);
        if matches!(next_significant, Some(TokenType::Else)) {
            self.scope.suppress_newlines = true;
            return;
        }
        if matches!(self.next_immediate(tokens, index), Some(TokenType::Comment(_))) {
            // Keeps an immediate comment inline with `}`.
            self.scope.suppress_newlines = true;
            return;
        }
        if matches!(next_significant, Some(TokenType::Return)) {
            if matches!(self.next_immediate(tokens, index), Some(TokenType::Comment(_)))
                && matches!(self.tokens.prev_non_separator_kind, Some(TokenType::CloseParen))
            {
                self.scope.suppress_newlines = true;
                return;
            }
            // Keeps `-> value` on the next line without extra blanks.
            self.ensure_newline();
            self.scope.suppress_newlines = true;
            return;
        }
        if self.scope.indent_level == 0 {
            self.ensure_newline();
            self.output.pending_empty_lines = self.output.pending_empty_lines.max(1);
        }
    }

    fn handle_open_scope(&mut self, tokens: &[Token], index: usize) {
        let scope_kind = self.scope.next_scope.clone();
        self.scope.next_scope = ScopeKind::Block;

        let next_token = self.next_non_separator(tokens, index);
        let has_inline_comment = matches!(next_token, Some(TokenType::Comment(_)));
        let has_inline_close = matches!(next_token, Some(TokenType::CloseScope));

        if scope_kind == ScopeKind::StructInit {
            self.open_struct_init_scope();
            return;
        }

        if self.output.at_line_start {
            // Avoids pulling `{` into a `//` comment line.
            if !matches!(self.tokens.prev_non_separator_kind, Some(TokenType::Comment(_))) {
                self.pull_back_to_previous_line();
            } else {
                // Prevents blank lines between the comment and `{`.
                self.output.pending_empty_lines = 0;
            }
        }
        self.emit_space();

        // Emits `{}` inline only when the block is truly empty.
        // Note: Comment-only blocks stay expanded.
        if matches!(self.next_non_separator(tokens, index), Some(TokenType::CloseScope))
            && self.current_scope_allows_empty_inline(tokens, index)
        {
            self.emit_token("{");
            self.emit_token("}");
            self.scope.skip_next_close_scope = true;
            // Inserts a blank line after top-level empty items.
            if self.scope.indent_level == 0 {
                self.ensure_newline();
                self.output.pending_empty_lines = self.output.pending_empty_lines.max(1);
            }
            return;
        }

        self.emit_token("{");
        self.scope.indent_level += 1;
        self.scope.scope_stack.push(scope_kind);
        if has_inline_comment && !has_inline_close {
            self.ensure_newline();
            self.scope.just_opened_scope = true;
            return;
        }
        self.ensure_newline();
        self.scope.just_opened_scope = true;
    }

    fn open_struct_init_scope(&mut self) {
        // Pulls `{` back onto the previous line if separated by newlines.
        if self.output.at_line_start {
            self.pull_back_to_previous_line();
        }
        self.emit_space();
        self.emit_token("{");
        self.scope.scope_stack.push(ScopeKind::StructInit);
    }

    fn handle_open_paren(&mut self) {
        if self.space_before(&TokenType::OpenParen) {
            self.emit_space();
        }
        self.depth.paren_depth += 1;
        self.emit_token("(");
    }

    fn handle_close_paren(&mut self) {
        self.emit_token(")");
        self.depth.paren_depth = self.depth.paren_depth.saturating_sub(1);
    }

    fn handle_argument_separator(&mut self) {
        self.emit_token(",");

        if self.should_force_newline_after_comma() {
            self.ensure_newline();
            self.scope.suppress_newlines = true;
        }
    }

    fn handle_subtraction(&mut self) {
        let is_unary = classify::is_unary_minus_context(self.tokens.prev_kind.as_ref());
        if is_unary {
            if self.space_before(&TokenType::Subtraction) {
                self.emit_space();
            }
        } else {
            self.emit_space();
        }
        self.emit_token("-");
        self.tokens.prev_was_unary_minus = is_unary;
    }

    fn handle_comment(&mut self, curr: &TokenType) {
        let has_space = self
            .output
            .output
            .chars()
            .last()
            .is_some_and(|c| c == ' ');
        if !self.output.at_line_start && !has_space && !self.should_defer_inline_comment() {
            self.emit_space();
        }
        let text = curr.as_text();
        if self.should_defer_inline_comment() {
            let needs_space = !self.output.at_line_start && !has_space;
            self.output.pending_inline_comment = Some(PendingComment {
                text: text.to_string(),
                needs_space,
            });
        } else {
            self.emit_token(&text);
            // Ends the current line after `//` comments and preserves a single newline.
            self.ensure_newline();
            self.scope.just_opened_scope = true;
        }
    }

    fn should_defer_inline_comment(&self) -> bool {
        matches!(
            self.tokens.prev_non_separator_kind,
            Some(TokenType::CloseScope)
                | Some(TokenType::CloseParen)
                | Some(TokenType::CloseGeneric)
        )
    }

    /// Drops comments that would split tokens that must be joined.
    /// Note: Comments survive only at natural line endings.
    fn should_drop_comment(&self, tokens: &[Token], index: usize) -> bool {
        // Drops comments inside parens or generics where newlines are suppressed.
        if self.depth.paren_depth > 0 || self.depth.generic_depth > 0 {
            return true;
        }

        // Drops comments inside struct inits since they are strictly inline and newlines break syntax.
        if matches!(self.current_scope(), Some(ScopeKind::StructInit)) {
            return true;
        }

        let prev = self.tokens.prev_non_separator_kind.as_ref();
        let next = self.next_significant(tokens, index);
        let next_immediate = self.next_immediate(tokens, index);

        // Drops comments between `}` and `else` to keep `} else {` on one line.
        if matches!(prev, Some(TokenType::CloseScope))
            && matches!(next, Some(TokenType::Else))
        {
            return true;
        }

        // Drops comments between `}` and a following `->` return.
        if matches!(prev, Some(TokenType::CloseScope)) && matches!(next, Some(TokenType::Return)) {
            return !self.allow_comment_after_close_scope(tokens, index);
        }

        // Drops comments between `)` and `->` for function signatures, but not calls.
        if matches!(prev, Some(TokenType::CloseParen)) && matches!(next, Some(TokenType::Return)) {
            if self.is_function_signature(tokens, index) {
                return !self.allow_comment_after_close_scope(tokens, index);
            }
            return false;
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
        if self.should_join_tokens(prev, next, self.tokens.prev_prev_non_separator_kind.as_ref(), tokens, index) {
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
            && !self.allow_comment_after_close_scope(tokens, index)
        {
            return true;
        }

        false
    }

    fn allow_comment_after_close_scope(&self, tokens: &[Token], index: usize) -> bool {
        let prev = self.tokens.prev_non_separator_kind.as_ref();
        if !matches!(prev, Some(TokenType::CloseScope)) {
            return false;
        }
        let next = self.next_significant(tokens, index);
        if !matches!(next, Some(TokenType::Return)) {
            return false;
        }
        // Allow comment between `}` and `->` for control-flow blocks, but not for
        // function signatures where `) -> T` must remain on one line.
        if matches!(self.tokens.prev_prev_non_separator_kind, Some(TokenType::CloseParen)) {
            return false;
        }
        true
    }

    fn should_collapse_separator(&mut self, tokens: &[Token], index: usize) -> bool {
        let prev = self.tokens.prev_non_separator_kind.as_ref();
        let next = self.next_significant(tokens, index);

        if self.output.at_line_start {
            // Skips leading blank lines entirely.
            if !self.output.has_content {
                return true;
            }
            // Skips blank lines immediately after `{` or before `}`.
            if matches!(self.tokens.prev_kind, Some(TokenType::OpenScope))
                || matches!(prev, Some(TokenType::OpenScope))
            {
                return true;
            }
            if matches!(next, Some(TokenType::CloseScope)) {
                return true;
            }
        }

        if self.should_join_tokens(prev, next, self.tokens.prev_prev_non_separator_kind.as_ref(), tokens, index) {
            return true;
        }

        false
    }

    /// Returns whether `prev` and `next` must share the same output line.
    /// Note: This is used by separator collapsing and comment dropping.
    fn should_join_tokens(
        &self,
        prev: Option<&TokenType>,
        next: Option<&TokenType>,
        prev_prev: Option<&TokenType>,
        tokens: &[Token],
        index: usize,
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
            // Function returns (preceded by CloseParen and returning a type block) must stay joined.
            // Statement returns (like `call() -> 1`) may separate if they aren't part of a signature.
            // We check this by seeing if there is a pending inline comment that was deferred.
            let is_call_return = self.output.pending_inline_comment.is_some() || !self.is_function_signature(tokens, index);
            if is_call_return {
                return false;
            }
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
        if self.depth.generic_depth > 0 || self.depth.paren_depth > 0 {
            return;
        }
        if !matches!(curr, TokenType::If | TokenType::Loop) {
            return;
        }
        if self.output.at_line_start {
            return;
        }
        if matches!(
            self.tokens.prev_kind,
            Some(TokenType::StatementSeparator | TokenType::OpenScope)
        ) {
            return;
        }
        if matches!(self.tokens.prev_non_separator_kind, Some(TokenType::Else)) {
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
        if self.depth.generic_depth > 0 || self.depth.paren_depth > 0 {
            return;
        }
        if !Self::is_struct_field_start(curr) {
            return;
        }
        if matches!(self.tokens.prev_kind, Some(TokenType::OpenScope)) {
            return;
        }
        if matches!(self.tokens.prev_kind, Some(TokenType::StatementSeparator)) {
            if self.output.pending_empty_lines > 0 {
                return;
            }
            return;
        }
        if matches!(self.tokens.prev_kind, Some(TokenType::Comment(_))) {
            return;
        }
        if matches!(self.next_significant(tokens, index), Some(TokenType::OpenParen)) {
            return;
        }
        if matches!(self.tokens.prev_non_separator_kind, Some(TokenType::Return)) {
            return;
        }
        if matches!(self.tokens.prev_non_separator_kind, Some(TokenType::Identifier(_)))
            && (matches!(
                self.tokens.prev_prev_non_separator_kind.as_ref(),
                Some(prev_prev) if classify::is_datatype(prev_prev)
            ) || matches!(
                self.tokens.prev_prev_non_separator_kind,
                Some(TokenType::CloseGeneric)
            ))
        {
            if !self.output.at_line_start {
                self.ensure_newline();
            }
            return;
        }
        if !self.output.at_line_start {
            self.ensure_newline();
        }
    }

    fn ensure_newline_before_enum_variant(&mut self, curr: &TokenType) {
        // Never fire inside generic brackets or parens.
        if self.depth.generic_depth > 0 || self.depth.paren_depth > 0 {
            return;
        }
        if !matches!(curr, TokenType::Identifier(_)) {
            return;
        }
        // Enum variants are never separated by blank lines — always discard any
        // accumulated blank lines before emitting the next variant.
        self.output.pending_empty_lines = 0;
        if matches!(self.tokens.prev_kind, Some(TokenType::OpenScope)) {
            return;
        }
        if matches!(self.tokens.prev_kind, Some(TokenType::StatementSeparator)) {
            return;
        }
        if matches!(self.tokens.prev_kind, Some(TokenType::Comment(_))) {
            return;
        }
        if !self.output.at_line_start {
            self.ensure_newline();
        }
    }

    fn current_scope_allows_empty_inline(&self, tokens: &[Token], index: usize) -> bool {
        if matches!(self.current_scope(), Some(ScopeKind::StructInit)) {
            return false;
        }
        if matches!(
            self.tokens.prev_kind,
            Some(TokenType::Loop | TokenType::If | TokenType::Else | TokenType::Function)
        ) {
            return false;
        }
        // Allow inline empty blocks only when not at top-level items.
        if self.scope.indent_level > 0 {
            return true;
        }
        if matches!(
            self.tokens.prev_kind,
            Some(TokenType::Struct | TokenType::Enum | TokenType::Function)
        ) {
            return false;
        }
        // If next is a close scope and we're inside parentheses, do not inline.
        if self.depth.paren_depth > 0 && matches!(self.next_significant(tokens, index), None) {
            return false;
        }
        false
    }

    fn update_scope_context(&mut self, curr: &TokenType) {
        match curr {
            TokenType::Struct => self.scope.next_scope = ScopeKind::StructDef,
            TokenType::Enum => self.scope.next_scope = ScopeKind::EnumDef,
            TokenType::New => self.scope.next_scope = ScopeKind::StructInit,
            TokenType::Function | TokenType::If | TokenType::Else | TokenType::Loop => {
                self.scope.next_scope = ScopeKind::Block;
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
        if self.depth.generic_depth > 0 || self.depth.paren_depth > 0 {
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
        if matches!(self.tokens.prev_kind, Some(TokenType::OpenScope)) {
            return;
        }
        if matches!(self.tokens.prev_kind, Some(TokenType::StatementSeparator)) {
            if self.output.pending_empty_lines > 0 {
                return;
            }
        }
        if matches!(self.tokens.prev_kind, Some(TokenType::Comment(_))) {
            return;
        }
        if self.output.at_line_start && self.output.pending_empty_lines == 0 {
            self.output.pending_empty_lines = 1;
        }
    }

    fn current_scope(&self) -> Option<&ScopeKind> {
        self.scope.scope_stack.last()
    }

    // Forces newlines for enum entries and expanded struct inits.
    fn should_force_newline_after_comma(&self) -> bool {
        if self.depth.generic_depth > 0 || self.depth.paren_depth > 0 {
            return false;
        }
        matches!(self.current_scope(), Some(ScopeKind::EnumDef))
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

    fn finish(mut self) -> String {
        let end = self.output.output.trim_end().len();
        self.output.output.truncate(end);
        if !self.output.output.is_empty() {
            self.output.output.push('\n');
        }
        self.output.output
    }

    fn is_function_signature(&self, tokens: &[Token], index: usize) -> bool {
        // We are currently at the CloseParen before the Return token.
        // `index` points to CloseParen. We need to look past Return to the type.
        let mut i = index + 1;
        while i < tokens.len() {
            let kind = &tokens[i].kind;
            if matches!(kind, TokenType::StatementSeparator | TokenType::Comment(_) | TokenType::Return) {
                i += 1;
                continue;
            }
            if classify::is_datatype(kind) || matches!(kind, TokenType::Identifier(_)) {
                return true;
            }
            return false;
        }
        false
    }
}
