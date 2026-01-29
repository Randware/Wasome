use lexer::TokenType;
use formatter::{ItemCategory, categorize_keyword};

#[test]
fn test_categorize_keyword() {
    assert_eq!(
        categorize_keyword(&TokenType::Import),
        Some(ItemCategory::Import)
    );
    assert_eq!(
        categorize_keyword(&TokenType::Struct),
        Some(ItemCategory::Struct)
    );
    assert_eq!(
        categorize_keyword(&TokenType::Enum),
        Some(ItemCategory::Enum)
    );
    assert_eq!(
        categorize_keyword(&TokenType::Function),
        Some(ItemCategory::Function)
    );
}

#[test]
fn test_category_ordering() {
    assert!(ItemCategory::Import < ItemCategory::Struct);
    assert!(ItemCategory::Struct < ItemCategory::Enum);
    assert!(ItemCategory::Enum < ItemCategory::Function);
}
