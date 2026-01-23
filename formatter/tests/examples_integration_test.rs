//! Integration tests for the Wasome formatter.
//!
//! Tests idempotency: formatting twice should produce the same result.
//! Tests lexer validity: formatted code should tokenize without errors.

use formatter::format_source;
use lexer::{lex, TokenType};
use std::fs;
use std::path::PathBuf;

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("docs/examples/single_file")
}

/// Expands whitespace to simulate messy formatting.
fn corrupt(source: &str) -> String {
    let mut result = String::new();
    let mut i = 0;
    for c in source.chars() {
        i += 1;
        match c {
            ' ' => match i % 4 {
                0 => result.push_str("     "),
                1 => result.push_str("   "),
                _ => result.push(' '),
            },
            '\n' => match i % 3 {
                0 => result.push_str("\n\n\n"),
                1 => result.push_str("\n\n"),
                _ => result.push('\n'),
            },
            _ => result.push(c),
        }
    }
    result
}

fn test_file(name: &str) {
    let path = examples_dir().join(name);
    let original = fs::read_to_string(&path).expect("read file");
    let corrupted = corrupt(&original);
    let formatted = format_source(&corrupted);
    let formatted_again = format_source(&formatted);

    println!("\n=== {} ===\n", name);
    println!("CORRUPTED:\n{}", corrupted);
    println!("FORMATTED:\n{}", formatted);

    assert_eq!(
        formatted, formatted_again,
        "Formatter is not idempotent for {}", name
    );
}

#[test]
fn test_fibonacci() {
    test_file("fibonacci.waso");
}

#[test]
fn test_if() {
    test_file("if.waso");
}

#[test]
fn test_operator() {
    test_file("operator.waso");
}

// =============================================================================
// Line Break Tests
// =============================================================================

/// Checks that the lexer produces no errors for the given code.
fn lexes_ok(code: &str) -> bool {
    lex(code).all(|r| r.is_ok())
}

/// Checks that a specific token type appears in the output.
fn has_token(code: &str, expected: &TokenType) -> bool {
    lex(code)
        .filter_map(|r| r.ok())
        .any(|t| std::mem::discriminant(&t.kind) == std::mem::discriminant(expected))
}

#[test]
fn test_linebreak_lexer_validity() {
    // Format code with various constructs and verify it lexes without errors
    let examples = ["fibonacci.waso", "if.waso", "operator.waso"];
    
    println!("\n=== Line Break Lexer Validity ===\n");
    
    for name in examples {
        let path = examples_dir().join(name);
        let original = fs::read_to_string(&path).expect("read file");
        let formatted = format_source(&original);
        
        let lexes = lexes_ok(&formatted);
        println!("{}: lexer {}", name, if lexes { "OK" } else { "FAILED" });
        
        assert!(lexes, "Formatted {} produces lexer errors", name);
    }
}

#[test]
fn test_linebreak_multichar_tokens() {
    // These inputs contain multi-character tokens that should NOT be split
    let test_cases = [
        ("x <- 5", TokenType::Assign, "<- (assign)"),
        ("-> x", TokenType::Return, "-> (return)"),
        ("x <= 5", TokenType::LessThanEqual, "<= (less or equal)"),
        ("x >= 5", TokenType::GreaterThanEqual, ">= (greater or equal)"),
        ("x == 5", TokenType::Comparison, "== (comparison)"),
        ("x != 5", TokenType::NotEqual, "!= (not equal)"),
        ("x && y", TokenType::And, "&& (and)"),
        ("x || y", TokenType::Or, "|| (or)"),
        ("x << 1", TokenType::LShift, "<< (left shift)"),
        ("x >> 1", TokenType::RShift, ">> (right shift)"),
        ("a::b", TokenType::PathSeparator, ":: (path separator)"),
    ];
    
    println!("\n=== Multi-Character Token Integrity ===\n");
    
    for (input, expected_token, desc) in test_cases {
        let formatted = format_source(input);
        let has = has_token(&formatted, &expected_token);
        
        println!("{}: {}", desc, if has { "preserved" } else { "BROKEN" });
        
        assert!(
            has,
            "Token {} was broken by formatting.\nInput: {}\nFormatted: {}",
            desc, input, formatted
        );
    }
}

#[test]
fn test_linebreak_no_split_keywords() {
    // Verify keywords aren't split across lines
    let test_cases = [
        "fn main() {}",
        "if (true) {}",
        "loop {}",
        "struct Foo {}",
        "enum Bar {}",
        "import foo",
        "pub fn test() {}",
    ];
    
    println!("\n=== Keyword Preservation ===\n");
    
    for input in test_cases {
        let formatted = format_source(input);
        
        // Check that the formatted output lexes correctly
        assert!(lexes_ok(&formatted), "Failed to lex: {}", formatted);
        
        println!("{}: OK", input.split_whitespace().next().unwrap_or(input));
    }
}

