mod common;

use std::path::PathBuf;

use common::MockLoader;

use error::diagnostic::{Diagnostic, Level, Snippet};
use source::SourceMap;

#[test]
fn test_single_file_fixture() {
    MockLoader::reset();

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
                .annotate(88..89, "Identifier expected here")
                .build(),
        )
        .help("Assign name to variable")
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
fn test_annotate_many() {
    MockLoader::reset();

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
                .annotate_many([77..80, 52..55], "'num' is of type 'i64'")
                .build(),
        )
        .snippet(
            Snippet::builder()
                .file(main_id)
                .annotate(11..14, "Function expects type 'i32'")
                .build(),
        )
        .help("Change types or cast")
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
fn test_primary_span() {
    MockLoader::reset();

    MockLoader::load_from_disk("/src/main.waso", "main.waso");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/src"));
    let main_id = sm.load_file("main.waso").expect("Failed to load file");

    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Primary Span Test")
        .code("E8888")
        .snippet(
            Snippet::builder()
                .file(main_id)
                .annotate(88..89, "Primary")
                .annotate(52..55, "Secondary")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
fn test_multi_file() {
    MockLoader::reset();

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
                .annotate(32..40, "Struct `Vector2D` is private")
                .build(),
        )
        .snippet(
            Snippet::builder()
                .file(a_id)
                .annotate(0..0, "Missing `pub` modifier")
                .build(),
        )
        .build();

    error.print_snippets(&sm).unwrap();
}

#[test]
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
fn test_print_no_snippets() {
    let error = Diagnostic::builder()
        .level(Level::Error)
        .message("Failed linking with 'stdlib'")
        .help("Make sure you have the toolchain installed")
        .build();

    error.print().unwrap();
}
