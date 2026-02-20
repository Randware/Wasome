mod common;

use std::path::PathBuf;

use common::MockLoader;

use error::diagnostic::{Diagnostic, Level, Snippet};
use source::SourceMap;

#[test]
#[ignore]
fn test_single_file_fixture() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Expected identifier")
        .code("E0123")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .primary(88..89, "Identifier expected here")
                .build(),
        )
        .help("Assign name to variable")
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_bytepos_conversion() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let span = main_id.span(88, 89);

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Expected identifier")
        .code("E0023")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .primary(span.start()..span.end(), "Identifier expected here")
                .build(),
        )
        .help("Assign name to variable")
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_with_context() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Wrong type")
        .code("E0555")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .primary(77..80, "'num' is of type 'i64'")
                .context(52..55, "'num' was declared here")
                .build(),
        )
        .snippet(
            Snippet::builder()
                .file(main_id)
                .context(11..14, "Function expects type 'i32'")
                .build(),
        )
        .help("Change types or cast")
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_primary() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Warning)
        .message("Primary Span Test")
        .code("W8080")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .primary(52..55, "Primary")
                .context(88..89, "Secondary")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_print_with_snippets() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Info)
        .message("Print with snippets")
        .code("I2357")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .primary(52..55, "Error")
                .build(),
        )
        .build();

    error.print().unwrap();
}

#[test]
#[ignore]
fn test_multi_file() {
    MockLoader::load_from_disk("/src/a.waso", "a.waso");
    MockLoader::load_from_disk("/src/b.waso", "b.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let a_id = sm.load_file("a.waso").expect("Failed to load file");
    let b_id = sm.load_file("b.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Struct is private")
        .code("E0101")
        .snippet(
            Snippet::builder()
                .file(b_id)
                .primary(32..40, "Struct `Vector2D` is private")
                .build(),
        )
        .snippet(
            Snippet::builder()
                .file(a_id)
                .context(0..0, "Missing `pub` modifier")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_print_snippets_no_snippets() {
    let sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Failed linking with 'stdlib'")
        .code("E0101")
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_print_no_snippets() {
    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Failed linking with 'stdlib'")
        .help("Make sure you have the toolchain installed")
        .build();

    error.print().unwrap();
}

#[test]
#[ignore]
fn test_multiline() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Function already declared")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .primary(98..134, "This function was already declared")
                .context(0..36, "Already declared here")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_unicode() {
    MockLoader::load_from_disk("/src/unicode.waso", "unicode.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let unicode_id = sm.load_file("unicode.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Unicode alignment test")
        .snippet(
            Snippet::builder()
                .file(unicode_id)
                .primary(3..4, "This identifier is an emoji")
                .context(21..22, "So is this string")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_overlapping() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Warning)
        .message("Overlapping ranges")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .context(69..81, "Inner range")
                .primary(52..95, "Outer range")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_zero_length() {
    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Unexpected EOF")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .primary(134..135, "Expected semicolon here")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
#[ignore]
fn test_empty_file() {
    MockLoader::load_from_disk("/src/empty.waso", "empty.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let empty_id = sm.load_file("empty.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("File is empty")
        .snippet(
            Snippet::builder()
                .file(empty_id)
                .primary(0..0, "Nothing to see here")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}
