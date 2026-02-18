use lexer::{lex, TokenType};
use formatter::format_source;
use formatter::reorder::{ItemCategory, categorize_keyword, parse_top_level_items};

#[test]
fn test_categorize_keyword() {
    assert_eq!(categorize_keyword(&TokenType::Import), ItemCategory::Import);
    assert_eq!(categorize_keyword(&TokenType::Struct), ItemCategory::Struct);
    assert_eq!(categorize_keyword(&TokenType::Enum), ItemCategory::Enum);
    assert_eq!(categorize_keyword(&TokenType::Function), ItemCategory::Function);
}

#[test]
fn test_category_ordering() {
    assert!(ItemCategory::Import < ItemCategory::Struct);
    assert!(ItemCategory::Struct < ItemCategory::Enum);
    assert!(ItemCategory::Enum < ItemCategory::Function);
}

#[test]
fn test_parse_top_level_items() {
    let code = "fn foo() {}\nstruct Bar {}\nimport baz";
    let tokens = lex(code).filter_map(|r| r.ok()).collect();
    let items = parse_top_level_items(tokens);

    // Filter out separator/whitespace items
    let meaningful: Vec<_> = items.iter()
        .filter(|i| i.category != ItemCategory::Other)
        .collect();

    assert_eq!(meaningful.len(), 3, "Should have 3 meaningful items");
    assert_eq!(meaningful[0].category, ItemCategory::Function);
    assert_eq!(meaningful[1].category, ItemCategory::Struct);
    assert_eq!(meaningful[2].category, ItemCategory::Import);
}

#[test]
fn test_parse_pub_fn() {
    let code = "pub fn main() {}";
    let tokens = lex(code).filter_map(|r| r.ok()).collect();
    let items = parse_top_level_items(tokens);

    assert_eq!(items.len(), 1);
    assert_eq!(items[0].category, ItemCategory::Function);
}

#[test]
fn test_reorder_output() {
    let code = "fn foo() {}\nimport bar\nstruct Baz {}";
    let formatted = format_source(code);

    let lines: Vec<&str> = formatted.lines().collect();
    // After reordering: imports, structs, then functions
    assert!(lines[0].starts_with("import"), "First item should be import, got: {}", lines[0]);
}
