use crate::types::{BytePos, FileID, LineInfo, Location, MultiByteChar, Span};
use io::{FileIO, WasomeLoader};
use std::fmt::{Debug, Formatter};
use std::{
    collections::HashMap,
    io::{Error, ErrorKind},
    marker::PhantomData,
    path::{Path, PathBuf},
};

/// The central registry for source files
///
/// The [`SourceMap`] is responsible for:
/// * Loading source files from disk with a configured [`FileLoader`]
/// * Deduplicating files to save memory (ensuring files are only loaded once)
/// * Translating low level [`Span`]s (byte offsets) into human-readable [`Location`]s
#[derive(Clone, PartialEq, Eq)]
pub struct SourceMap<Loader: FileIO = WasomeLoader> {
    root_path: PathBuf,
    file_cache: HashMap<PathBuf, FileID>,
    files: Vec<SourceFile>,
    /// The loader's purpose is to let the user define custom loading behavior
    __loader: PhantomData<Loader>,
}

// This allows [`Debug`] to be used without the derive trait bounds
impl<Loader: FileIO> Debug for SourceMap<Loader> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceMap")
            .field("root_path", &self.root_path)
            .field("file_cache", &self.file_cache)
            .field("files", &self.files)
            .finish()
    }
}

impl<Loader: FileIO> SourceMap<Loader> {
    /// Creates a new and empty [`SourceMap`] that is rooted at the given path
    pub fn new(root_path: PathBuf) -> Self {
        Self {
            root_path,
            file_cache: HashMap::new(),
            files: Vec::new(),
            __loader: PhantomData,
        }
    }

    /// Loads the file to the [`SourceMap`]
    ///
    /// Takes a relative path to a file, adds it to the [cache](SourceMap::file_cache),
    /// loads that content of the file into a [`SourceFile`] and adds the [file](SourceFile) to the [SourceMap](SourceMap::files)
    /// If the file is already loaded (present in the cache), this function
    /// returns the existing [`FileID`] immediately without performing IO.  
    ///
    /// # Returns
    ///
    /// * `Ok(FileID)`- A handle to the newly *stored* file
    /// * `Err()` - An i/o [error](std::io::Error) in case the underlying `OS` returns an [error](std::io::Error)
    pub fn load_file<F: AsRef<Path>>(&mut self, relative_path: F) -> Result<FileID, Error> {
        // Joining the path to get an absolute path
        let path = Loader::resolve(&self.root_path, relative_path)?;

        // Checks the cache
        if let Some(&id) = self.file_cache.get(&path) {
            return Ok(id);
        }

        // If not cached, calls the loader
        let source_file = Self::get_source_file(&path)?;

        // Safety check
        if self.files().len() > u32::MAX as usize {
            return Err(Error::new(
                ErrorKind::Other,
                "Too many files loaded: cannot assign FileID beyond u32::MAX",
            ));
        }

        // Determines the next file id
        let file_id = FileID(self.files.len() as u32);

        // Add the file to cache
        self.file_cache.insert(path, file_id);
        // Add the file to the SourceMap
        self.files.push(source_file);

        Ok(file_id)
    }

    /// Internal helper function for [`SourceFile`] loading
    ///
    /// It loads the [`SourceFile`] from the provided path with Loader.
    ///
    /// # Parameter
    ///
    /// * **absolute_path** - The [`SourceFile`] to load
    ///
    /// # Return
    ///
    /// * **Ok(SourceFile)** - The loading was successful
    /// + **Err(Error)** - There was an IO error
    fn get_source_file(absolute_path: &Path) -> Result<SourceFile, Error> {
        let content_string = Loader::load(absolute_path)?;
        Ok(SourceFile::new(absolute_path.to_path_buf(), content_string))
    }

    /// Retrieves the SourceFile object from a FileID handle
    ///
    /// # Returns
    ///
    /// * `Ok(&SourceFile)`- A reference to the file associated with the given ID
    /// * `None` - When the provided [`FileID`] is not found
    pub fn get_file(&self, id: &FileID) -> Option<&SourceFile> {
        self.files.get(id.0 as usize)
    }

    /// Takes a [`Span`] and converts the [`Span::start`] pos to a [location](Location).
    ///
    /// # Returns
    ///
    /// * `Some(Location)`- The 1-based [`Location`]
    /// * `None()` - May return [None](Option::None)
    ///   if the [`Span`] does not belong to the [`SourceMap`]
    ///
    /// # Warning
    ///
    /// If the [`Span`] does not belong to this [`SourceMap`] **the returned data
    /// will be arbitrary** or `None` in the best case
    pub fn lookup_location(&self, span: Span) -> Option<Location<'_>> {
        let file = self.files.get(span.file_id.0 as usize)?;
        Some(Self::location_from_pos(span.start, file))
    }

    /// Takes a [pos](BytePos) and a [file](SourceFile) and converts it to a [location](Location).
    ///
    /// # Returns
    ///
    /// * `Location`- The 1-based [`Location`]
    ///
    /// # Warning
    ///
    /// If the [`BytePos`] does not belong to [file](SourceFile) **the returned data
    /// will be arbitrary!**
    fn location_from_pos(pos: BytePos, file: &SourceFile) -> Location<'_> {
        let (line, col) = SourceFile::lookup_line_col(file, pos);
        Location { file, line, col }
    }

    /// Takes the [`Span`] and returns the referenced content
    ///
    /// # Returns
    ///
    /// * `Some(&str)` - The content which is referenced by the [`Span`]
    /// * `None` - When the [`Span`] does not belong to the [`SourceMap`]
    ///
    /// # Warning
    ///
    /// If the [`Span`] does not belong to this [`SourceMap`] **the returned data
    /// will be arbitrary** or `None` in the best case!
    pub fn get_source_slice(&self, span: Span) -> Option<&str> {
        let file = self.files.get(span.file_id.0 as usize)?;

        let start = span.start.0 as usize;
        let end = span.end.0 as usize;

        if start > end {
            return None;
        }

        file.content.get(start..end)
    }

    /// Gets the [root_path][`SourceMap::root_path`]
    pub fn root_path(&self) -> &PathBuf {
        &self.root_path
    }

    /// Gets the [files][`SourceMap::files`]
    pub fn files(&self) -> &[SourceFile] {
        &self.files
    }
}

/// A struct representing a file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    /// The [path](PathBuf) to the file in question
    path: PathBuf,
    /// A unified representation of the file's content
    /// All line breaks are `\n`
    content: String,
    /// Used for faster indexing
    lines: Vec<LineInfo>,
}

impl SourceFile {
    /// Creates a new [`SourceFile`]
    ///
    /// Takes a path to a file, does some indexing and pre-caching for later use
    ///
    /// # Panics
    ///
    /// If the content string is larger than [`u32::MAX`] the function panics, because byte index are stored as [`u32`].
    /// This means any *single* source file is limited to 4_294_967_295 bytes.
    pub fn new(path: PathBuf, mut content: String) -> Self {
        assert!(
            content.len() <= (u32::MAX as usize),
            "Source file '{}' is larger than 4GB which is not supported!",
            path.display()
        );

        // unifies line breaks
        content = content.replace("\r\n", "\n");

        // Initialize temporary variables for later use
        let mut lines = Vec::new();
        let mut multi_byte_chars = Vec::new();
        let mut current_line_start = BytePos(0);
        // tracking the gap that the multi byte chars need
        // To calc: total_bytes - accumulated_gap
        // this gives an accurate number of columns
        let mut accumulated_gap = 0u32;

        for (byte_pos, ch) in content.char_indices() {
            let ch_len = ch.len_utf8() as u8;
            let byte_pos = byte_pos as u32;
            accumulated_gap += (ch_len - 1) as u32;

            // If the char is a multi byte char, cache it
            if ch_len > 1 {
                multi_byte_chars.push(MultiByteChar {
                    pos_on_line: byte_pos - current_line_start.0,
                    byte_len: ch_len,
                    accumulated_gap,
                });
            }

            // If the char indicates a line break, cache it
            if ch == '\n' {
                lines.push(LineInfo {
                    line_start: current_line_start,
                    multi_byte_chars,
                });

                current_line_start = BytePos(byte_pos + 1);
                multi_byte_chars = Vec::new();
                accumulated_gap = 0;
            }
        }

        // push the last line at the end of the file that does not get
        // pushed to `lines` because it does not end with `\n`
        lines.push(LineInfo {
            line_start: current_line_start,
            multi_byte_chars,
        });

        Self {
            path,
            content,
            lines,
        }
    }

    /// Retrieves the 1-based `line` and `column` index
    /// from a given [`SourceFile`] and [position](BytePos)
    ///
    /// # Returns
    /// A tuple with: ([line](`u32`),[column](`u32`))
    /// Will be arbitrary when the BytePos does _not belong_ to the [`SourceFile`]
    fn lookup_line_col(file: &SourceFile, pos: BytePos) -> (u32, u32) {
        let line_index = match file
            .lines
            .binary_search_by_key(&pos, |info| info.line_start)
        {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };

        let line_info = &file.lines[line_index];

        let byte_col = pos.0 - line_info.line_start.0;

        // Finds the index of the last multi-byte character that ends before or at byte_col
        let idx = line_info
            .multi_byte_chars
            .partition_point(|mb| (mb.pos_on_line + mb.byte_len as u32) <= byte_col);

        // If we are inside a mb char we need to calculate our position
        // inside that char ("padding") and treat those extra bytes as part of the gap.
        //
        // Example: 'd' (1 byte) followed by '🦀' (4 bytes)
        //          The cursor is at Byte 2 (inside the crab).
        //
        // Bytes:   [0x64] [0xF0 0x9F 0xA6 0x80]
        // Index:      0     1    2    3    4
        //                     ^    ^
        //                     |    |
        //              MB Start    Cursor (Pos 2)
        //
        // Logic:
        // 1. Previous Gap: 0 (since 'd' is 1 byte, 1 col).
        // 2. Padding: Cursor(2) - MB Start(1) = 1 byte of internal offset.
        // 3. Total Gap to subtract: Previous(0) + Padding(1) = 1.
        let padding = line_info
            .multi_byte_chars
            .get(idx)
            .filter(|mb| byte_col > mb.pos_on_line)
            .map(|mb| byte_col - mb.pos_on_line)
            .unwrap_or(0);

        // Access to accumulated gap of the nearest mb
        let gap = if idx > 0 {
            line_info.multi_byte_chars[idx - 1].accumulated_gap + padding
        } else {
            padding
        };

        // Note: When the provided span would have a start INSIDE a multi byte char
        // this - would panic if we don't use saturating_sub because the result would be negative
        (line_index as u32 + 1, byte_col.saturating_sub(gap) + 1)
    }

    /// Gets the file's [path][`SourceFile::path`]
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Gets the file's [content][`SourceFile::content`]
    pub fn content(&self) -> &str {
        &self.content
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        SourceFile, SourceMap,
        types::{BytePos, Span},
    };
    use io::{FileLoader, PathResolver};
    use std::{
        collections::HashMap,
        io::{Error, ErrorKind},
        path::PathBuf,
        sync::{LazyLock, Mutex},
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
        fn load<F: AsRef<std::path::Path>>(path: F) -> Result<String, std::io::Error> {
            let path = path.as_ref().to_path_buf();
            let fs = MOCK_FS.lock().unwrap();

            match fs.get(&path) {
                Some(content) => Ok(content.clone()),
                None => Err(Error::new(ErrorKind::NotFound, "File not found in mock FS")),
            }
        }
    }

    impl PathResolver for MockLoader {
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
        let target_pos = BytePos(9); // Because it is 0 indexed so 10 - 1

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
            end: BytePos(7), // 6 + 1 because Span is exclusive
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

    #[test]
    fn test_lookup_col_inside_multibyte_char() {
        // "a✨b"
        // 'a' : Byte 0
        // '✨': Bytes 1-3 (3 bytes total)
        // 'b' : Byte 4. Len 1
        let f = make_file("a✨b");

        // Start of the char (Byte 1)
        // Should be Col 2
        let (_, col) = SourceFile::lookup_line_col(&f, BytePos(1));
        assert_eq!(col, 2, "Start of sparkles");

        // Inside the char (Byte 2)
        // 1 byte deep into the 3-byte char
        let (_, col) = SourceFile::lookup_line_col(&f, BytePos(2));
        assert_eq!(col, 2, "Inside sparkles (offset 1)");

        // past the char (Byte 4) -> 'b'
        // 'a'(1) + '✨'(3) = 4 bytes.
        // Now we jump to Col 3
        let (_, col) = SourceFile::lookup_line_col(&f, BytePos(4));
        assert_eq!(col, 3, "After sparkles");
    }

    #[test]
    fn test_lookup_col_consecutive_multibyte() {
        // "✨⭐"
        // Both are 3 bytes long.
        let f = make_file("✨⭐");

        // Inside first char
        let (_, col) = SourceFile::lookup_line_col(&f, BytePos(1));
        assert_eq!(col, 1);

        // Start of second char (Byte 3)
        // We passed ✨. Gap is 2 (3 bytes - 1 char).
        // Byte 3.
        // Col = 3 - 2 + 1 = 2.
        let (_, col) = SourceFile::lookup_line_col(&f, BytePos(3));
        assert_eq!(col, 2);

        // Inside second char (Byte 4)
        // We passed ✨ (Gap 2). We are inside ⭐ (Padding 1).
        // Total sub: Gap(2) + Padding(1) = 3.
        // Byte 4.
        // Col = 4 - 3 + 1 = 2.
        let (_, col) = SourceFile::lookup_line_col(&f, BytePos(4));
        assert_eq!(col, 2);
    }

    fn make_file(content: &str) -> SourceFile {
        SourceFile::new(PathBuf::from("test.waso"), content.to_string())
    }
}
