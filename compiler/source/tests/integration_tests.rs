use io::WasomeLoader;
use source::SourceMap;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use tempfile::{TempDir, tempdir};

// --- HELPER FUNCTION ---

fn setup_file(name: &str, content: &str) -> (TempDir, PathBuf) {
    let dir = tempfile::tempdir().unwrap();
    let file_path = dir.path().join(name);
    let mut file = File::create(&file_path).unwrap();
    write!(file, "{}", content).unwrap();
    (dir, file_path)
}

// --- INTEGRATION TESTS ---

#[test]
fn test_workflow_load_and_lookup() {
    let content = "fn main() {\n    let x = 10;\n}";
    let (dir, _path) = setup_file("main.waso", content);

    let mut sm: SourceMap = SourceMap::new(dir.path().to_path_buf());

    let id = sm
        .load_file("main.waso")
        .expect("Failed to load existing file");

    let file = sm.get_file(&id).expect("FileID should exist");
    assert_eq!(file.content(), content);

    // Lookup 'x' in "let x = 10;"
    // Offset: 20
    let span = id.span(20, 21);

    let loc = sm.lookup_location(span).expect("Should resolve location");

    assert_eq!(loc.line, 2);
    assert_eq!(loc.col, 9);
    assert!(loc.file.path().ends_with("main.waso"));
}

#[test]
fn test_unicode_handling_sparkles() {
    // Line 1: 'a'(1) + '\n'(1) = 2 bytes
    // Line 2: '✨'(3 bytes) + 'b'(1 byte)
    let content = "a\n✨b";
    let (dir, _) = setup_file("sparkles.waso", content);

    let mut sm: SourceMap = SourceMap::new(dir.path().to_path_buf());
    let id = sm.load_file("sparkles.waso").unwrap();

    // We want to point to 'b'
    // Byte offset calculation: 'a' (1 byte) + '\n' (1 byte) + '✨' (3 bytes) = 5 bytes; 'b' is at offset 5 (0-based)
    let span = id.span(5, 6);

    let loc = sm.lookup_location(span).unwrap();

    assert_eq!(loc.line, 2);

    assert_eq!(loc.col, 2, "Column should be 2 (after the sparkle)");

    assert_eq!(sm.get_source_slice(span), Some("b"));
}

#[test]
fn test_caching_behavior() {
    let (dir, file_path) = setup_file("cache.waso", "v1");
    let mut sm: SourceMap = SourceMap::new(dir.path().to_path_buf());

    let id1 = sm.load_file("cache.waso").unwrap();

    let mut file_handle = File::create(&file_path).unwrap();
    write!(file_handle, "v2_modified").unwrap();

    let id2 = sm.load_file("cache.waso").unwrap();
    let file2 = sm.get_file(&id2).unwrap();

    assert_eq!(id1, id2);
    assert_eq!(file2.content(), "v1");
}

#[test]
fn test_relative_path_resolution() {
    let dir = tempdir().unwrap();
    let root = dir.path();

    // root/src/lib/
    let lib_dir = root.join("src").join("lib");
    fs::create_dir_all(&lib_dir).unwrap();

    // root/src/lib/math.waso
    let file_path = lib_dir.join("math.waso");
    let mut file = File::create(&file_path).unwrap();
    write!(file, "code").unwrap();

    let mut sm: SourceMap = SourceMap::new(root.to_path_buf());

    let result = sm.load_file("src/lib/math.waso");
    assert!(result.is_ok());
}

#[test]
fn test_error_file_not_found() {
    let dir = tempdir().unwrap();
    let mut sm: SourceMap = SourceMap::new(dir.path().to_path_buf());

    let result = sm.load_file("missing.waso");

    assert!(result.is_err());
    assert_eq!(result.unwrap_err().kind(), std::io::ErrorKind::NotFound);
}

#[test]
fn test_span_slicing() {
    let (dir, _) = setup_file("slice.waso", "12345");
    let mut sm: SourceMap = SourceMap::new(dir.path().to_path_buf());
    let id = sm.load_file("slice.waso").unwrap();

    let span = id.span(1, 4); // "234"
    assert_eq!(sm.get_source_slice(span), Some("234"));
}

#[test]
fn test_cross_file_span_merging() {
    let (dir, _) = setup_file("a.waso", "");

    let file_path_b = dir.path().join("b.waso");
    File::create(&file_path_b).unwrap();

    let mut sm = SourceMap::<WasomeLoader>::new(dir.path().to_path_buf());

    let id_a = sm.load_file("a.waso").unwrap();
    let id_b = sm.load_file("b.waso").unwrap();

    let span_a = id_a.span(0, 5);
    let span_b = id_b.span(0, 5);

    assert_ne!(
        span_a.file_id, span_b.file_id,
        "SourceMap must assign different IDs"
    );
    assert!(
        span_a.merge(span_b).is_none(),
        "Merging spans from different files must fail"
    );
}
