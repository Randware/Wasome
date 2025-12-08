use std::{
    collections::HashMap,
    io::{Error, ErrorKind},
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

use source::{
    SourceFile, SourceMap,
    loader::FileLoader,
    types::{BytePos, Span},
};

static MOCK_FS: LazyLock<Mutex<HashMap<PathBuf, String>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

struct MockLoader;
impl MockLoader {
    fn save(path: &str, content: &str) {
        let p = PathBuf::from(path);
        MOCK_FS.lock().unwrap().insert(p, content.to_string());
    }
}
impl FileLoader for MockLoader {
    fn load<F: AsRef<std::path::Path>>(path: F) -> Result<source::SourceFile, std::io::Error> {
        let path = path.as_ref().to_path_buf();
        let fs = MOCK_FS.lock().unwrap();

        match fs.get(&path) {
            Some(content) => Ok(SourceFile::new(path, content.clone())),
            None => Err(Error::new(ErrorKind::NotFound, "File not found in mock FS")),
        }
    }

    fn resolve<T: AsRef<std::path::Path>, F: AsRef<std::path::Path>>(
        root_path: T,
        relative_path: F,
    ) -> Result<PathBuf, Error> {
        Ok(root_path.as_ref().join(relative_path))
    }
}

/// Tests if basic file gets loaded correctly
#[test]
fn test_load_existing_file() {
    let filename = "/happy_path.wao";
    let content = "fn main() { -> 0; }";
    MockLoader::save(filename, content);

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));

    let id = sm.load_file(filename).expect("Should load existing file");

    let file = sm.get_file(&id).expect("FileID should exist");
    assert_eq!(file.content(), content);
}

/// Tests if the SourceMap returns an error if we try to load a missing file
#[test]
fn test_load_non_existent_file() {
    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));

    let result = sm.load_file("/ghost_file.waso");

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind(), ErrorKind::NotFound);
}

/// Tests if files get cached and if the same file return the same FileID
#[test]
fn test_file_deduplication() {
    let filename = "/cached.waso";
    MockLoader::save(filename, "content");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));

    let id1 = sm.load_file(filename).unwrap();
    let id2 = sm.load_file(filename).unwrap();

    // IDs must be identical!!!
    assert_eq!(id1, id2);

    assert_eq!(sm.files().len(), 1);
}

/// Test if loading multiple files work
#[test]
fn test_interleaved_loading() {
    MockLoader::save("/a.waso", "A");
    MockLoader::save("/b.waso", "B");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));

    let id_a_1 = sm.load_file("/a.waso").unwrap();
    let id_b_1 = sm.load_file("/b.waso").unwrap();
    let id_a_2 = sm.load_file("/a.waso").unwrap();

    assert_eq!(id_a_1, id_a_2);
    assert_ne!(id_a_1, id_b_1);
}

/// Tests if \r\n get replaced with \n
#[test]
fn test_crlf_normalization() {
    let filename = "/windows.waso";
    MockLoader::save(filename, "a\r\nb");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));
    let id = sm.load_file(filename).unwrap();
    let file = sm.get_file(&id).unwrap();

    assert_eq!(file.content(), "a\nb");

    // 2. Check Line 2 lookup
    // The normalized string is "a\nb" (bytes: a=0, \n=1, b=2)
    // We want to make sure the SourceFile logic handles the lookup correctly
    let span_b = Span {
        file_id: id,
        start: BytePos(2),
        end: BytePos(3),
    };
    let loc = sm.lookup_location(span_b);
    assert!(loc.is_some());
    let loc = loc.unwrap();

    assert_eq!(loc.line, 2);
    assert_eq!(loc.col, 1);
}

/// tests if the byte-to-char column calculation work
#[test]
fn test_unicode_column_offsets() {
    let filename = "/unicode.waso";
    // Line 1: "let " (4 bytes)
    // Line 2: "✨" (3 bytes) + " = 1;"
    MockLoader::save(filename, "let \n✨ = 1;");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));
    let id = sm.load_file(filename).unwrap();

    // Target the '=' sign after the sparkles.
    // "let \n" = 5 bytes (0..5)
    // "✨" = 3 bytes (5..8)
    // " " = 1 byte (8..9)
    // "=" = 1 byte (9..10) -> This is our target
    let target_pos = BytePos(9); //Bcs it is 0 indexed so 10 - 1

    let span = Span {
        file_id: id,
        start: target_pos,
        end: target_pos + 1,
    };
    let loc = sm.lookup_location(span);
    assert!(loc.is_some());
    let loc = loc.unwrap();

    // The line is 2
    // The equals signs is the 3rd character on this specific line
    assert_eq!(loc.line, 2);
    assert_eq!(loc.col, 3);
}

/// Tests the behaviour if the file is empty
#[test]
fn test_empty_file() {
    MockLoader::save("/empty.waso", "");
    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));
    let id = sm.load_file("/empty.waso").unwrap();

    // Should verify that looking up index 0 doesn't panic and returns line 1, col 1 (or 0)
    let binding = Span {
        file_id: id,
        start: BytePos(0),
        end: BytePos(0),
    };
    let loc = sm.lookup_location(binding);

    assert!(loc.is_some());
    let loc = loc.unwrap();
    assert_eq!(loc.line, 1);
    assert_eq!(loc.col, 1);
}

/// Tests if SourceMap works even though the EOF does not have a \n
#[test]
fn test_no_newline_eof() {
    let filename = "/no_eof.waso";
    MockLoader::save(filename, "line1\nline2"); // No \n after line2

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));
    let id = sm.load_file(filename).unwrap();

    // Check 'l' in "line2"
    // "line1\n" is 6 bytes (0-5)
    // "line2" starts at 6
    let binding = Span {
        file_id: id,
        start: BytePos(6),
        end: BytePos(6) + 1,
    };
    let loc = sm.lookup_location(binding);

    assert!(loc.is_some());
    let loc = loc.unwrap();
    assert_eq!(loc.line, 2);
    assert_eq!(loc.col, 1);
}

///  tests if we can retrieve the actual code snippet
#[test]
fn test_get_source_slice() {
    let filename = "/slice.waso";
    MockLoader::save(filename, "let foo = 10;");

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));
    let id = sm.load_file(filename).unwrap();

    // "foo" is bytes 4, 5, 6 (0-based)
    let span = Span {
        file_id: id,
        start: BytePos(4),
        end: BytePos(7), // 6 + 1 becuase Span is exclusive
    };
    let slice = sm.get_source_slice(span).expect("Should find slice");

    assert_eq!(slice, "foo");
}

#[test]
fn test_empty_slice() {
    let filename = "/empty_slice.waso";
    MockLoader::save(filename, "");
    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));
    let id = sm.load_file(filename).unwrap();

    let span = Span {
        file_id: id,
        start: BytePos(210),
        end: BytePos(214), // Random span
    };

    assert_eq!(None, sm.get_source_slice(span));
}

/// Tests if larger files work correctly and perform binary search as expected
#[test]
fn test_large_line_count() {
    let filename = "/large.waso";
    // Create a string with 10k lines (to simulate our large file)
    let mut content = String::new();
    for _ in 0..10_000 {
        content.push_str("a\n");
    }
    MockLoader::save(filename, &content);

    let mut sm = SourceMap::<MockLoader>::new(PathBuf::from("/"));
    let id = sm.load_file(filename).unwrap();

    // Look up the very last 'a'
    // Each line is 2 bytes
    // Line 10_000 starts at 9999 * 2 = 19998
    let pos = BytePos(19998);
    let binding = Span {
        file_id: id,
        start: pos,
        end: pos + 1,
    };
    let loc = sm.lookup_location(binding);

    assert!(loc.is_some());
    let loc = loc.unwrap();
    assert_eq!(loc.line, 10_000);
    let slice = sm.get_source_slice(binding);
    assert!(slice.is_some());
    assert_eq!("a", slice.unwrap());
}
