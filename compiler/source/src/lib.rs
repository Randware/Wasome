use std::{
    collections::HashMap,
    fs,
    io::Error,
    marker::PhantomData,
    path::{Path, PathBuf},
};

/// The byte offset of the file's content
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytePos(pub u32);

/// A handle to a [`SourceFile`]
///
/// # Warning
///
/// The [`FileID`] is dependent on the [`SourceMap`] and *not universally usable*
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileID(pub u32);

impl FileID {
    /// Creates a Span inside this file from [`BytePos`]
    ///
    /// # Arguments
    ///
    /// * `start` - The [pos](BytePos) where the span begins (0-Based)
    /// * `end` - The [pos](BytePos) where the span ends (0-Based)
    pub fn span(self, start: u32, end: u32) -> Span {
        Span {
            file_id: self,
            start: BytePos(start),
            end: BytePos(end),
        }
    }
}

/// A handle to a file which is incredibly cheap to copy, store and pass around
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    file_id: FileID,
    start: BytePos,
    end: BytePos,
}

impl Span {
    /// Gets the start [`BytePos`] of the [`Span`]
    /// _(0-Based indexing)_
    pub fn start(&self) -> BytePos {
        self.start
    }

    /// Gets the end [`BytePos`] of the [`Span`]
    /// _(0-Based indexing)_
    pub fn end(&self) -> BytePos {
        self.end
    }
}

/// Meta data about a [`MultiByteChar`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiByteChar {
    /// The byte offset from the start of the line
    pub pos_on_line: u32,

    /// The number of bytes this char occupies (2, 3, or 4)
    pub byte_len: u8,

    // Sum of all (byte_len - 1) for this and previous chars on the line
    pub accumulated_gap: u32,
}

/// A struct representing a file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    /// The [path](PathBuf) to the file in question
    path: PathBuf,
    /// An unified representation of the file's content
    /// All line breake are `\n`
    content: String,
    /// Used for faster indexing
    lines: Vec<LineInfo>,
}

impl SourceFile {
    /// Takes a relative path to a file, adds it to the [cache](SourceMap::file_cache),
    /// loads that content of the file into a [`SourceFile`] and adds the [file](SourceFile) to the [SourceMap](SourceMap::files)
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
        if content.contains("\r\n") {
            content = content.replace("\r\n", "\n");
        }

        // Initialize temporary variables for later use
        let mut lines = Vec::new();
        let mut multi_byte_chars = Vec::new();
        let mut current_line_start = BytePos(0);
        // Tracking the Gap that the multi byte chars need
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

    /// Retrives the 1-based `line` and `column` index
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
            Err(idx) => idx - 1,
        };

        let line_info = &file.lines[line_index];

        let byte_col = pos.0 - line_info.line_start.0;

        // Looks at the indexx of the last mb before the byte_col
        let idx = line_info
            .multi_byte_chars
            .partition_point(|mb| mb.pos_on_line < byte_col);

        // Access to accumulated gao of the nerest mb
        let gap = if idx > 0 {
            line_info.multi_byte_chars[idx - 1].accumulated_gap
        } else {
            0
        };

        // Safety: When the provided span would have a start INSIDE a multi byte char
        // this - would panic if we don't use saturating_sub because the result would be negative
        (line_index as u32 + 1, byte_col.saturating_sub(gap) + 1)
    }
}

/// A per-line info struct that holds the starting byte and all mutli byte chars
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineInfo {
    line_start: BytePos,
    multi_byte_chars: Vec<MultiByteChar>,
}

// A human-readable location
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location<'a> {
    pub file: &'a SourceFile,
    pub line: u32, // 1-based
    pub col: u32,  // 1-based
}

impl<'a> Location<'a> {
    /// 1 based
    pub fn new(file: &'a SourceFile, line: u32, col: u32) -> Self {
        Self { file, line, col }
    }
}

/// The central registry for source files
///
/// The [`SourceMap`] is responsible for:
/// * Loading source files from disk with a configured [`FileLoader`]
/// * Deduplicating files to save memory (ensuring files are only loaded once)
/// * Translating low level [`Span`]s (byte offsets) into human-readable [`Location`]s
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMap<Loader: FileLoader = WasomeLoader> {
    root_path: PathBuf,
    file_cache: HashMap<PathBuf, FileID>,
    files: Vec<SourceFile>,
    /// The loader's purpose is to let the user define custom loading behavior
    __loader: PhantomData<Loader>,
}

impl<Loader: FileLoader> SourceMap<Loader> {
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
    ///
    /// # Returns
    ///
    /// * `Ok(FileID)`- A handle to the newly *stored* file
    /// * `Err()` - An i/o [error](std::io::Error) in case the underlying `OS` returns an [error](std::io::Error)
    pub fn load_file<F: AsRef<Path>>(&mut self, relative_path: F) -> Result<FileID, Error> {
        // Joining the path to get an absolut path
        let path = Self::join(&self.root_path, relative_path)?;

        // Checks the cache
        if let Some(&id) = self.file_cache.get(&path) {
            return Ok(id);
        }

        // If not cached, calls the loader
        let source_file = Loader::load(&path)?;

        // Determines the next file id
        let file_id = FileID(self.files.len() as u32);

        // Add the file to cache
        self.file_cache.insert(path, file_id);
        // Add the file to the SourceMap
        self.files.push(source_file);

        Ok(file_id)
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
    pub fn location_from_span<'a>(&'a self, span: &'a Span) -> Option<Location<'a>> {
        let file = self.files.get(span.file_id.0 as usize)?;
        Some(Self::location_from_pos(&span.start, file))
    }

    /// Takes a [pos](BytePos) and a [file](SourceFile) and coverts it to a [location](Location).
    ///
    /// # Returns
    ///
    /// * `Location`- The 1-based [`Location`]
    ///
    /// # Warning
    ///
    /// If the [`BytePos`] does not belong to [file](SourceFile) **the returned data
    /// will be arbitrary!**
    pub fn location_from_pos<'a>(pos: &'a BytePos, file: &'a SourceFile) -> Location<'a> {
        let (line, col) = SourceFile::lookup_line_col(file, *pos);
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
    pub fn get_source_slice(&self, span: &Span) -> Option<&str> {
        let file = self.files.get(span.file_id.0 as usize)?;

        let start = span.start.0 as usize;
        let end = span.end.0 as usize;

        if start > end {
            return None;
        }

        file.content.get(start..end)
    }

    /// __Joins__ and __canonicalizes__ the provided paths
    ///
    /// # Returns
    ///
    /// * `Ok(PathBuf)`- The newly joined [path](PathBuf)
    /// * `None` - When the provided [`FileID`] is not found
    fn join<T: AsRef<Path>, F: AsRef<Path>>(
        root_path: T,
        relative_path: F,
    ) -> Result<PathBuf, Error> {
        let path = root_path.as_ref().join(relative_path);
        fs::canonicalize(path.as_path())
    }
}

/// A trait that defines the loading behaviour.
/// Future proof in case WIT components change the file loading process
pub trait FileLoader {
    fn load<F: AsRef<Path>>(path: F) -> Result<SourceFile, Error> {
        let content = fs::read_to_string(&path)?;

        Ok(SourceFile::new(path.as_ref().to_path_buf(), content))
    }
}

/// Default loader for `.wasom` files
pub struct WasomeLoader;

impl FileLoader for WasomeLoader {}
