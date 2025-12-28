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
                .annotate(11..14, "Expected 'i32'")
                .annotate_many([52..55, 77..80], "Got 'i64'")
                .build(),
        )
        .help("Change types or cast")
        .build();

    error.print_snippets(&sm).unwrap();
}
