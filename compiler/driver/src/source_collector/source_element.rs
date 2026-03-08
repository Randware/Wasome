use ordered_hash_map::OrderedHashMap;
use source::types::FileID;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};

/// The entire Wasome program being compiled.
///
/// This contains the complete source file structure of a program,
/// including all projects and their directory hierarchies. It is created
/// during the source collection phase and later converted to an AST.
///
/// # Design Rationale
///
/// Uses `OrderedHashMap` instead of `HashMap` to preserve insertion order,
/// ensuring deterministic behavior for reproducible builds and consistent
/// error messages.
///
/// This is attached to a [`SourceMap`]
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct WasomeProgram {
    location: WasomeSourceElementLocation,
    projects: OrderedHashMap<String, WasomeSourceDirectory>,
}

impl WasomeProgram {
    /// Creates a new WasomeProgram instance.
    ///
    /// # Parameters
    ///
    /// - **`location`**: Metadata about the program root
    /// - **`projects`**: Ordered mapping of project names to source directories
    pub fn new(
        location: WasomeSourceElementLocation,
        projects: OrderedHashMap<String, WasomeSourceDirectory>,
    ) -> Self {
        Self { location, projects }
    }

    /// Returns the location metadata for the program root.
    pub fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }

    /// Returns all projects in the program.
    ///
    /// Returns an ordered map where keys are project names and values
    /// are the corresponding source directories.
    pub fn projects(&self) -> &OrderedHashMap<String, WasomeSourceDirectory> {
        &self.projects
    }

    /// Destructures this program, consuming it and returning all fields.
    ///
    /// # Returns
    ///
    /// A tuple containing:
    /// - The program's location metadata
    /// - The ordered map of projects
    pub(crate) fn destructure(
        self,
    ) -> (
        WasomeSourceElementLocation,
        OrderedHashMap<String, WasomeSourceDirectory>,
    ) {
        (self.location, self.projects)
    }
}

impl HasWasomeSourceElementLocation for WasomeProgram {
    fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }
}

/// A directory containing Wasome source files and subdirectories.
///
/// This forms part of the hierarchical representation of a program's
/// source file structure. Directories can contain both source files and
/// nested subdirectories, creating a tree structure.
///
/// This is attached to a [`SourceMap`]
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct WasomeSourceDirectory {
    location: WasomeSourceElementLocation,
    /// May not contain dirs with duplicate names
    subdirs: Vec<WasomeSourceDirectory>,
    /// May not contain files with duplicate names
    files: Vec<WasomeSourceFile>,
}

impl WasomeSourceDirectory {
    /// Creates a new WasomeSourceDirectory with validation.
    ///
    /// # Parameters
    ///
    /// - **`location`**: Directory metadata (name and path)
    /// - **`subdirs`**: Child directories to include
    /// - **`files`**: Source files to include
    ///
    /// # Validation
    ///
    /// Checks that:
    /// - No two subdirectories have the same name
    /// - No two files have the same name
    ///
    /// This prevents ambiguous references during compilation.
    ///
    /// # Errors
    ///
    /// If a requirement under Validation was violated
    pub fn new(
        location: WasomeSourceElementLocation,
        subdirs: Vec<WasomeSourceDirectory>,
        files: Vec<WasomeSourceFile>,
    ) -> Result<Self, WasomeSourceDirectoryCreationError> {
        if let Some(dup) = duplicate_wasome_source_elements(subdirs.iter()) {
            return Err(WasomeSourceDirectoryCreationError::DuplicateDirectoryNames(
                dup,
            ));
        }
        if let Some(dup) = duplicate_wasome_source_elements(files.iter()) {
            return Err(WasomeSourceDirectoryCreationError::DuplicateFileNames(dup));
        }
        Ok(Self {
            location,
            subdirs,
            files,
        })
    }

    /// Returns the location metadata for this directory.
    pub fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }

    /// Returns all subdirectories contained in this directory.
    pub fn subdirs(&self) -> &[WasomeSourceDirectory] {
        &self.subdirs
    }

    /// Returns all source files contained in this directory.
    pub fn files(&self) -> &[WasomeSourceFile] {
        &self.files
    }

    /// Destructures this directory, consuming it and returning all fields.
    ///
    /// # Returns
    ///
    /// A tuple containing:
    /// - The directory's location metadata
    /// - Vector of subdirectories
    /// - Vector of source files
    pub(crate) fn destructure(
        self,
    ) -> (
        WasomeSourceElementLocation,
        Vec<WasomeSourceDirectory>,
        Vec<WasomeSourceFile>,
    ) {
        (self.location, self.subdirs, self.files)
    }
}

impl HasWasomeSourceElementLocation for WasomeSourceDirectory {
    fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }
}

/// Error type for directory construction failures.
///
/// This enum represents errors that occur when creating a [`WasomeSourceDirectory`]
#[derive(Debug, PartialEq)]
pub enum WasomeSourceDirectoryCreationError {
    /// Two or more files in the same directory share a name
    DuplicateFileNames(String),
    /// Two or more subdirectories in the same directory share a name
    DuplicateDirectoryNames(String),
}

/// A single Wasome source file.
///
/// This holds a reference to a loaded source file via its FileID
/// and contains metadata about the file's location in the filesystem.
///
/// This is attached to a [`SourceMap`]
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct WasomeSourceFile {
    /// Relative to the project root
    location: WasomeSourceElementLocation,
    file: FileID,
}

impl WasomeSourceFile {
    /// Creates a new WasomeSourceFile instance.
    ///
    /// # Parameters
    ///
    /// - **`location`**: File metadata (name and path)
    /// - **`file`**: FileID referencing the loaded file content in SourceMap
    pub fn new(location: WasomeSourceElementLocation, file: FileID) -> Self {
        Self { location, file }
    }

    /// Returns the FileID for accessing the file content in the SourceMap.
    pub fn file(&self) -> FileID {
        self.file
    }

    /// Destructures this file, consuming it and returning all fields.
    ///
    /// # Returns
    ///
    /// A tuple containing:
    /// - The file's location metadata
    /// - The FileID for accessing content
    pub fn destructure(self) -> (WasomeSourceElementLocation, FileID) {
        (self.location, self.file)
    }
}

impl HasWasomeSourceElementLocation for WasomeSourceFile {
    fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }
}

/// Location metadata for a source element (e.g.: program, directory, or file).
///
/// This encapsulates the identifying information for any element
/// in the source file hierarchy.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct WasomeSourceElementLocation {
    /// The last part of path
    ///
    /// Serves as an internal cache
    name: String,
    /// May be empty
    path: PathBuf,
}

impl WasomeSourceElementLocation {
    /// Creates a new location metadata instance.
    ///
    /// # Parameters
    ///
    /// - **`path`**: Full filesystem path to the element
    pub fn new(path: PathBuf) -> Self {
        Self {
            name: path
                .iter()
                .last()
                .unwrap_or(OsStr::new(""))
                .to_string_lossy()
                .to_string(),
            path,
        }
    }

    /// Returns the human-readable name of the element.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the filesystem path of the element.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Destructures this location, consuming it and returning all fields.
    ///
    /// # Returns
    ///
    /// A tuple containing:
    /// - The element's name
    /// - The element's filesystem path
    pub fn destructure(self) -> (String, PathBuf) {
        (self.name, self.path)
    }
}

/// Trait for types that have source element location metadata.
///
/// This trait provides a common interface for all source elements,
/// enabling generic operations on them.
pub trait HasWasomeSourceElementLocation {
    /// Returns the location metadata for this element.
    fn location(&self) -> &WasomeSourceElementLocation;
}

/// Checks for duplicate element names in an iterator of source elements.
///
/// This helper function is used during directory construction to validate
/// that no two files or subdirectories share the same name within the
/// same parent directory.
///
/// # Type Parameters
///
/// - **`Element`**: Type implementing [`HasWasomeSourceElementLocation`]
/// - **`Elements`**: Iterator yielding references to elements
///
/// # Parameters
///
/// - **`to_check`**: Iterator over elements to check for duplicates
///
/// # Returns
///
/// - **`Some(String)`**: The duplicate name if a conflict is found
/// - **`None`**: If all names are unique
fn duplicate_wasome_source_elements<
    'a,
    Element: HasWasomeSourceElementLocation + 'a,
    Elements: ExactSizeIterator<Item = &'a Element>,
>(
    to_check: Elements,
) -> Option<String> {
    let mut found = HashSet::with_capacity(to_check.len());
    for e in to_check {
        let name = e.location().name();
        if found.contains(name) {
            return Some(name.to_owned());
        }
        found.insert(name);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Helper Functions ---

    fn mock_location(path: &str) -> WasomeSourceElementLocation {
        WasomeSourceElementLocation::new(PathBuf::from(path))
    }

    fn mock_file(name: &str, id: u32) -> WasomeSourceFile {
        let location = mock_location(name);
        WasomeSourceFile::new(location, FileID::from(id))
    }

    fn mock_program(
        name: &str,
        projects: OrderedHashMap<String, WasomeSourceDirectory>,
    ) -> WasomeProgram {
        let location = mock_location(name);
        WasomeProgram::new(location, projects)
    }

    // --- WasomeSourceElementLocation Tests ---

    #[test]
    fn test_location_creation() {
        let location = mock_location("/path/to/test");

        assert_eq!(location.name(), "test");
        assert_eq!(location.path(), Path::new("/path/to/test"));
    }

    #[test]
    fn test_location_destructure() {
        let location = mock_location("/some/path");
        let (name, path) = location.destructure();

        assert_eq!(name, "path");
        assert_eq!(path, PathBuf::from("/some/path"));
    }

    #[test]
    fn test_location_equality() {
        let loc1 = mock_location("same");
        let loc2 = mock_location("same");
        let loc3 = mock_location("different");

        assert_eq!(loc1, loc2);
        assert_ne!(loc1, loc3);
    }

    // --- WasomeSourceFile Tests ---

    #[test]
    fn test_file_creation() {
        let location = mock_location("/path/myfile.waso");
        let file_id = FileID::from(42);
        let file = WasomeSourceFile::new(location, file_id);

        assert_eq!(file.location().name(), "myfile.waso");
        assert_eq!(file.file(), file_id);
    }

    #[test]
    fn test_file_destructure() {
        let location = mock_location("/path/test_file.waso");
        let file_id = FileID::from(100);
        let file = WasomeSourceFile::new(location, file_id);
        let (loc, id) = file.destructure();

        assert_eq!(loc.name(), "test_file.waso");
        assert_eq!(id, file_id);
    }

    #[test]
    fn test_file_equality() {
        let file1 = WasomeSourceFile::new(mock_location("f1"), FileID::from(1));
        let file2 = WasomeSourceFile::new(mock_location("f1"), FileID::from(1));
        let file3 = WasomeSourceFile::new(mock_location("f1"), FileID::from(2));

        assert_eq!(file1, file2);
        assert_ne!(file1, file3);
    }

    // --- WasomeSourceDirectory Tests ---

    #[test]
    fn test_directory_creation_empty() {
        let location = mock_location("/path/empty");
        let files = Vec::new();
        let subdirs = Vec::new();

        let dir = WasomeSourceDirectory::new(location, subdirs, files).unwrap();

        assert_eq!(dir.location().name(), "empty");
        assert!(dir.subdirs().is_empty());
        assert!(dir.files().is_empty());
    }

    #[test]
    fn test_directory_creation_with_content() {
        let location = mock_location("/path/full");

        let file1 = mock_file("file1", 1);
        let file2 = mock_file("file2", 2);

        let subdir = WasomeSourceDirectory::new(
            mock_location("subdir"),
            Vec::new(),
            vec![mock_file("subfile", 3)],
        )
        .unwrap();

        let dir = WasomeSourceDirectory::new(location, vec![subdir], vec![file1, file2]).unwrap();

        assert_eq!(dir.location().name(), "full");
        assert_eq!(dir.subdirs().len(), 1);
        assert_eq!(dir.files().len(), 2);
    }

    #[test]
    fn test_directory_destructure() {
        let location = mock_location("/path/dir");
        let file = mock_file("test", 1);

        let dir = WasomeSourceDirectory::new(location, Vec::new(), vec![file]).unwrap();
        let (loc, subdirs, files) = dir.destructure();

        assert_eq!(loc.name(), "dir");
        assert!(subdirs.is_empty());
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_directory_duplicate_files() {
        let location = mock_location("/path/dup");
        let file1 = mock_file("duplicate", 1);
        let file2 = mock_file("duplicate", 2);

        let result = WasomeSourceDirectory::new(location, Vec::new(), vec![file1, file2]);

        assert!(matches!(
            result,
            Err(WasomeSourceDirectoryCreationError::DuplicateFileNames(name)) if name == "duplicate"
        ));
    }

    #[test]
    fn test_directory_duplicate_subdirs() {
        let location = mock_location("/path/dup");
        let subdir1 =
            WasomeSourceDirectory::new(mock_location("duplicate"), Vec::new(), Vec::new()).unwrap();
        let subdir2 =
            WasomeSourceDirectory::new(mock_location("duplicate"), Vec::new(), Vec::new()).unwrap();

        let result = WasomeSourceDirectory::new(location, vec![subdir1, subdir2], Vec::new());

        assert!(matches!(
            result,
            Err(WasomeSourceDirectoryCreationError::DuplicateDirectoryNames(name)) if name == "duplicate"
        ));
    }

    #[test]
    fn test_directory_nested_structure() {
        let file1 = mock_file("root_file", 1);
        let file2 = mock_file("another_file", 2);

        let sub1 = WasomeSourceDirectory::new(
            mock_location("sub1"),
            Vec::new(),
            vec![mock_file("sub1_file", 3)],
        )
        .unwrap();

        let sub2 = WasomeSourceDirectory::new(
            mock_location("sub2"),
            vec![
                WasomeSourceDirectory::new(
                    mock_location("nested"),
                    Vec::new(),
                    vec![mock_file("nested_file", 4)],
                )
                .unwrap(),
            ],
            vec![mock_file("sub2_file", 5)],
        )
        .unwrap();

        let root =
            WasomeSourceDirectory::new(mock_location("root"), vec![sub1, sub2], vec![file1, file2])
                .unwrap();

        assert_eq!(root.location().name(), "root");
        assert_eq!(root.files().len(), 2);
        assert_eq!(root.subdirs().len(), 2);

        let nested = &root.subdirs()[1].subdirs()[0];
        assert_eq!(nested.location().name(), "nested");
        assert_eq!(nested.files().len(), 1);
    }

    // --- WasomeSourceDirectoryCreationError Tests ---

    #[test]
    fn test_error_duplicate_files_variant() {
        let error = WasomeSourceDirectoryCreationError::DuplicateFileNames("test.txt".to_string());

        assert!(
            matches!(error, WasomeSourceDirectoryCreationError::DuplicateFileNames(name) if name == "test.txt")
        );
    }

    #[test]
    fn test_error_duplicate_directories_variant() {
        let error =
            WasomeSourceDirectoryCreationError::DuplicateDirectoryNames("mydir".to_string());

        assert!(matches!(
            error,
            WasomeSourceDirectoryCreationError::DuplicateDirectoryNames(name) if name == "mydir"
        ));
    }

    // --- WasomeProgram Tests ---

    #[test]
    fn test_program_creation_empty() {
        let projects = OrderedHashMap::new();
        let program = mock_program("empty_program", projects);

        assert_eq!(program.location().name(), "empty_program");
        assert!(program.projects().is_empty());
    }

    #[test]
    fn test_program_creation_with_projects() {
        let mut projects = OrderedHashMap::new();

        let project1 = WasomeSourceDirectory::new(
            mock_location("project1"),
            Vec::new(),
            vec![mock_file("main", 1)],
        )
        .unwrap();

        let project2 = WasomeSourceDirectory::new(
            mock_location("project2"),
            Vec::new(),
            vec![mock_file("lib", 2)],
        )
        .unwrap();

        projects.insert("project1".to_string(), project1);
        projects.insert("project2".to_string(), project2);

        let program = mock_program("my_program", projects);

        assert_eq!(program.location().name(), "my_program");
        assert_eq!(program.projects().len(), 2);
        assert!(program.projects().contains_key("project1"));
        assert!(program.projects().contains_key("project2"));
    }

    #[test]
    fn test_program_destructure() {
        let mut projects = OrderedHashMap::new();
        projects.insert(
            "proj".to_string(),
            WasomeSourceDirectory::new(mock_location("proj"), Vec::new(), Vec::new()).unwrap(),
        );

        let program = mock_program("test_program", projects);
        let (loc, projs) = program.destructure();

        assert_eq!(loc.name(), "test_program");
        assert_eq!(projs.len(), 1);
    }

    #[test]
    fn test_program_project_ordering() {
        let mut projects = OrderedHashMap::new();

        projects.insert(
            "first".to_string(),
            WasomeSourceDirectory::new(mock_location("first"), Vec::new(), Vec::new()).unwrap(),
        );
        projects.insert(
            "second".to_string(),
            WasomeSourceDirectory::new(mock_location("second"), Vec::new(), Vec::new()).unwrap(),
        );
        projects.insert(
            "third".to_string(),
            WasomeSourceDirectory::new(mock_location("third"), Vec::new(), Vec::new()).unwrap(),
        );

        let program = mock_program("ordered_program", projects);

        let keys: Vec<&String> = program.projects().keys().collect();
        assert_eq!(keys, vec!["first", "second", "third"]);
    }

    #[test]
    fn test_program_equality() {
        let mut projects1 = OrderedHashMap::new();
        projects1.insert(
            "proj".to_string(),
            WasomeSourceDirectory::new(
                mock_location("proj"),
                Vec::new(),
                vec![mock_file("file", 1)],
            )
            .unwrap(),
        );

        let projects2 = projects1.clone();

        let program1 = WasomeProgram::new(mock_location("test"), projects1);
        let program2 = WasomeProgram::new(mock_location("test"), projects2);

        assert_eq!(program1, program2);
    }

    // --- HasWasomeSourceElementLocation Trait Tests ---

    #[test]
    fn test_trait_implementation_program() {
        let program = mock_program("test_prog", OrderedHashMap::new());
        let location = HasWasomeSourceElementLocation::location(&program);

        assert_eq!(location.name(), "test_prog");
    }

    #[test]
    fn test_trait_implementation_directory() {
        let dir =
            WasomeSourceDirectory::new(mock_location("test_dir"), Vec::new(), Vec::new()).unwrap();
        let location = HasWasomeSourceElementLocation::location(&dir);

        assert_eq!(location.name(), "test_dir");
    }

    #[test]
    fn test_trait_implementation_file() {
        let file = mock_file("test_file", 1);
        let location = HasWasomeSourceElementLocation::location(&file);

        assert_eq!(location.name(), "test_file");
    }

    // --- duplicate_wasome_source_elements Tests ---

    #[test]
    fn test_duplicate_finder_empty() {
        let elements: Vec<WasomeSourceFile> = Vec::new();

        let result = duplicate_wasome_source_elements(elements.iter());

        assert!(result.is_none());
    }

    #[test]
    fn test_duplicate_finder_no_duplicates() {
        let files = vec![
            mock_file("file1", 1),
            mock_file("file2", 2),
            mock_file("file3", 3),
        ];

        let result = duplicate_wasome_source_elements(files.iter());

        assert!(result.is_none());
    }

    #[test]
    fn test_duplicate_finder_file_duplicate() {
        let files = vec![
            mock_file("unique1", 1),
            mock_file("duplicate", 2),
            mock_file("duplicate", 3),
            mock_file("unique2", 4),
        ];

        let result = duplicate_wasome_source_elements(files.iter());

        assert_eq!(result, Some("duplicate".to_string()));
    }

    #[test]
    fn test_duplicate_finder_directory_duplicate() {
        let dirs = vec![
            WasomeSourceDirectory::new(mock_location("a"), Vec::new(), Vec::new()).unwrap(),
            WasomeSourceDirectory::new(mock_location("b"), Vec::new(), Vec::new()).unwrap(),
            WasomeSourceDirectory::new(mock_location("b"), Vec::new(), Vec::new()).unwrap(),
        ];

        let result = duplicate_wasome_source_elements(dirs.iter());

        assert_eq!(result, Some("b".to_string()));
    }

    #[test]
    fn test_duplicate_finder_first_duplicate_wins() {
        let files = vec![
            mock_file("first_dup", 1),
            mock_file("second_dup", 2),
            mock_file("first_dup", 3),
            mock_file("second_dup", 4),
        ];

        let result = duplicate_wasome_source_elements(files.iter());

        assert_eq!(result, Some("first_dup".to_string()));
    }
}
