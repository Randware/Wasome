//! Tests for indentation logic.

use formatter::format_source;

#[test]
fn test_basic_indentation() {
    let input = "fn foo() {x}";
    let formatted = format_source(input);
    
    // Should have newline after { and before }, with indented content
    assert!(formatted.contains("{\n"), "Missing newline after {{");
    assert!(formatted.contains("\n}"), "Missing newline before }}");
    assert!(formatted.contains("    x"), "Content should be indented with 4 spaces");
}

#[test]
fn test_nested_indentation() {
    let input = "fn foo() {if (x) {y}}";
    let formatted = format_source(input);
    
    // Inner block should have 8 spaces (2 levels)
    assert!(formatted.contains("        y"), "Nested content should have 8 spaces");
}

#[test]
fn test_else_stays_with_brace() {
    let input = "fn foo() {if (x) {a} else {b}}";
    let formatted = format_source(input);
    
    // "} else {" should be on one line
    assert!(formatted.contains("} else {"), "else should stay with closing brace");
}

#[test]
fn test_trailing_newline() {
    let input = "fn foo() {}";
    let formatted = format_source(input);
    
    assert!(formatted.ends_with('\n'), "File should end with newline");
}
