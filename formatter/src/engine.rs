//! Core formatting engine.
//!
//! Processes tokens and produces formatted output.

use crate::indent::IndentTracker;
use crate::reorder::{parse_top_level_items, reorder_items};
use crate::spacing::needs_space_before;
use lexer::{lex, Token, TokenType};

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

/// Checks if a token starts a top-level item.
fn is_top_level_start(token: &TokenType) -> bool {
    matches!(
        token,
        TokenType::Function
            | TokenType::Struct
            | TokenType::Enum
            | TokenType::Import
            | TokenType::Public
    )
}

/// Formats a sequence of tokens.
fn format_tokens(tokens: &[Token]) -> String {
    let mut output = String::new();
    let mut indent = IndentTracker::new();
    let mut at_line_start = true;

    for (i, token) in tokens.iter().enumerate() {
        let prev = if i > 0 { Some(&tokens[i - 1]) } else { None };
        let next = tokens.get(i + 1);

        // Skip statement separators - we handle newlines explicitly
        if matches!(token.kind, TokenType::StatementSeparator) {
            if !at_line_start {
                output.push('\n');
                at_line_start = true;
            }
            continue;
        }

        // Handle closing brace - decrease indent first
        if matches!(token.kind, TokenType::CloseScope) {
            indent.decrease();
            if !at_line_start {
                output.push('\n');
                at_line_start = true;
            }
        }

        // Check if we need a blank line before top-level items
        if indent.is_at_top_level() && is_top_level_start(&token.kind) {
            if let Some(prev_token) = prev {
                if matches!(
                    prev_token.kind,
                    TokenType::CloseScope | TokenType::StatementSeparator | TokenType::Semicolon
                ) {
                    if !output.is_empty() && !output.ends_with("\n\n") {
                        while output.ends_with(' ') || output.ends_with('\t') {
                            output.pop();
                        }
                        if !output.ends_with('\n') {
                            output.push('\n');
                        }
                        output.push('\n');
                        at_line_start = true;
                    }
                }
            }
        }

        // Add indentation at line start
        let just_indented = at_line_start;
        if at_line_start {
            output.push_str(&indent.as_str());
            at_line_start = false;
        }

        // Add space before token if needed (but not right after indentation)
        if !just_indented {
            if let Some(prev_token) = prev {
                if needs_space_before(&prev_token.kind, &token.kind) {
                    output.push(' ');
                }
            }
        }

        // Add the token
        output.push_str(&token_to_string(&token.kind));

        // Handle opening brace
        if matches!(token.kind, TokenType::OpenScope) {
            indent.increase();
            output.push('\n');
            at_line_start = true;
        }
        // Handle closing brace
        else if matches!(token.kind, TokenType::CloseScope) {
            match next.map(|t| &t.kind) {
                Some(TokenType::Else) | Some(TokenType::Semicolon) => {}
                _ => {
                    output.push('\n');
                    at_line_start = true;
                }
            }
        }
        // Handle semicolon after closing brace
        else if matches!(token.kind, TokenType::Semicolon) {
            if let Some(prev_token) = prev {
                if matches!(prev_token.kind, TokenType::CloseScope) {
                    output.push('\n');
                    at_line_start = true;
                }
            }
        }
        // Handle comments
        else if matches!(token.kind, TokenType::Comment(_)) {
            output.push('\n');
            at_line_start = true;
        }
    }

    if !output.ends_with('\n') && !output.is_empty() {
        output.push('\n');
    }

    output
}

/// Converts a token to its string representation.
fn token_to_string(token: &TokenType) -> String {
    use TokenType::*;

    match token {
        S8 => "s8".into(),
        S16 => "s16".into(),
        S32 => "s32".into(),
        S64 => "s64".into(),
        U8 => "u8".into(),
        U16 => "u16".into(),
        U32 => "u32".into(),
        U64 => "u64".into(),
        F32 => "f32".into(),
        F64 => "f64".into(),
        Bool => "bool".into(),
        Char => "char".into(),
        SelfType => "self".into(),
        Identifier(s) => s.clone(),
        String(s) => s.clone(),
        Decimal(f) => f.to_string(),
        Integer(i) => i.to_string(),
        CharLiteral(c) => format!("'{}'", escape_char(*c)),
        True => "true".into(),
        False => "false".into(),
        Addition => "+".into(),
        Subtraction => "-".into(),
        Multiplication => "*".into(),
        Modulo => "%".into(),
        Slash => "/".into(),
        LessThan => "<".into(),
        GreaterThan => ">".into(),
        LessThanEqual => "<=".into(),
        GreaterThanEqual => ">=".into(),
        NotEqual => "!=".into(),
        Comparison => "==".into(),
        LShift => "<<".into(),
        RShift => ">>".into(),
        BitOr => "|".into(),
        Or => "||".into(),
        BitAnd => "&".into(),
        And => "&&".into(),
        Not => "!".into(),
        OpenScope => "{".into(),
        CloseScope => "}".into(),
        OpenParen => "(".into(),
        CloseParen => ")".into(),
        Function => "fn".into(),
        If => "if".into(),
        Else => "else".into(),
        Loop => "loop".into(),
        Struct => "struct".into(),
        Enum => "enum".into(),
        As => "as".into(),
        Public => "pub".into(),
        New => "new".into(),
        Import => "import".into(),
        Return => "->".into(),
        Assign => "<-".into(),
        PathSeparator => "::".into(),
        Dot => ".".into(),
        Semicolon => ";".into(),
        StatementSeparator => "".into(),
        ArgumentSeparator => ",".into(),
        Comment(s) => s.clone(),
    }
}

fn escape_char(c: char) -> String {
    match c {
        '\n' => "\\n".into(),
        '\t' => "\\t".into(),
        '\r' => "\\r".into(),
        '\0' => "\\0".into(),
        '\\' => "\\\\".into(),
        '\'' => "\\'".into(),
        _ => c.to_string(),
    }
}
