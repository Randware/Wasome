use ordered_hash_map::OrderedHashMap;
use source::types::FileID;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

/// Represents the entire Wasome program being compiled.
///
/// This struct contains the complete source file structure of a program,
/// including all projects and their directory hierarchies. It is created
/// during the source collection phase and later converted to an AST.
///
/// # Fields
///
/// - **`location`**: Metadata about the program root (name and filesystem path)
/// - **`projects`**: Ordered mapping of project names to their source directories
///
/// # Design Rationale
///
/// Uses `OrderedHashMap` instead of `HashMap` to preserve insertion order,
/// ensuring deterministic behavior for reproducible builds and consistent
/// error messages.
///
/// # Example
///
/// ```ignore
/// let program = WasomeProgram::new(location, projects);
/// for (name, dir) in program.projects() {
///     println!("Project: {} at {:?}", name, dir.location().path());
/// }
/// ```
#[derive(PartialEq, Eq, Debug)]
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
    pub fn destructure(
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

/// Represents a directory containing Wasome source files and subdirectories.
///
/// This struct forms part of the hierarchical representation of a program's
/// source file structure. Directories can contain both source files and
/// nested subdirectories, creating a tree structure.
///
/// # Fields
///
/// - **`location`**: Directory metadata (name and filesystem path)
/// - **`subdirs`**: Child directories (recursive structure)
/// - **`files`**: Source files contained in this directory
///
/// # Validation
///
/// When constructed via [`WasomeSourceDirectory::new`], this struct validates
/// that there are no duplicate file or directory names within the same parent
/// directory. This prevents ambiguous references during compilation.
///
/// # Example
///
/// ```ignore
/// let dir = WasomeSourceDirectory::new(location, subdirs, files)?;
/// println!("Directory: {}", dir.location().name());
/// println!("Files: {}", dir.files().len());
/// ```
#[derive(PartialEq, Eq, Debug)]
pub struct WasomeSourceDirectory {
    location: WasomeSourceElementLocation,
    subdirs: Vec<WasomeSourceDirectory>,
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
    /// # Returns
    ///
    /// - **`Ok(Self)`**: If no duplicate names found
    /// - **`Err(WasomeSourceDirectoryCreationError)`**: If duplicate file or directory names exist
    ///
    /// # Validation
    ///
    /// Checks that:
    /// - No two subdirectories have the same name
    /// - No two files have the same name
    ///
    /// This prevents ambiguous references during compilation.
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
    pub fn destructure(
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
/// This enum represents errors that occur when creating a [`WasomeSourceDirectory`],
/// specifically when the directory structure contains naming conflicts.
///
/// # Variants
///
/// - **`DuplicateFileNames`**: Two or more files in the same directory share a name
/// - **`DuplicateDirectoryNames`**: Two or more subdirectories in the same directory share a name
///
/// # Example
///
/// ```ignore
/// match WasomeSourceDirectory::new(location, subdirs, files) {
///     Ok(dir) => println!("Directory created successfully"),
///     Err(WasomeSourceDirectoryCreationError::DuplicateFileNames(name)) => {
///         eprintln!("Duplicate file: {}", name);
///     }
///     Err(WasomeSourceDirectoryCreationError::DuplicateDirectoryNames(name)) => {
///         eprintln!("Duplicate directory: {}", name);
///     }
/// }
/// ```
#[derive(Debug)]
pub enum WasomeSourceDirectoryCreationError {
    DuplicateFileNames(String),
    DuplicateDirectoryNames(String),
}

/// Represents a single Wasome source file.
///
/// This struct holds a reference to a loaded source file via its FileID
/// and contains metadata about the file's location in the filesystem.
///
/// # Fields
///
/// - **`location`**: File metadata (filename and filesystem path)
/// - **`file`**: Handle to the loaded file content in the SourceMap
///
/// # Usage
///
/// Source files are collected during the source collection phase and later
/// parsed into AST nodes. The FileID is used to access the actual file content
/// from the SourceMap when parsing is needed.
///
/// # Example
///
/// ```ignore
/// let file = WasomeSourceFile::new(location, file_id);
/// let content = source_map.get(file.file());
/// ```
#[derive(PartialEq, Eq, Debug)]
pub struct WasomeSourceFile {
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

/// Location metadata for a source element (program, directory, or file).
///
/// This struct encapsulates the identifying information for any element
/// in the source file hierarchy, providing both a human-readable name
/// and the full filesystem path.
///
/// # Fields
///
/// - **`name`**: Human-readable name (e.g., directory name, filename without extension)
/// - **`path`**: Full filesystem path to the element
///
/// # Usage
///
/// This type is used by [`WasomeProgram`], [`WasomeSourceDirectory`], and
/// [`WasomeSourceFile`] to track their location in the filesystem. It is
/// also used by the [`HasWasomeSourceElementLocation`] trait for generic
/// operations on source elements.
///
/// # Example
///
/// ```ignore
/// let location = WasomeSourceElementLocation::new("my_project".to_string(), path);
/// println!("Project: {} at {:?}", location.name(), location.path());
/// ```
#[derive(PartialEq, Eq, Debug)]
pub struct WasomeSourceElementLocation {
    name: String,
    // Relative to the program root
    path: PathBuf,
}

impl WasomeSourceElementLocation {
    /// Creates a new location metadata instance.
    ///
    /// # Parameters
    ///
    /// - **`name`**: Human-readable name for the element
    /// - **`path`**: Full filesystem path to the element
    pub fn new(name: String, path: PathBuf) -> Self {
        Self { name, path }
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
/// This trait provides a common interface for all source elements
/// ([`WasomeProgram`], [`WasomeSourceDirectory`], [`WasomeSourceFile`]),
/// enabling generic operations on them.
///
/// # Implementors
///
/// - [`WasomeProgram`]
/// - [`WasomeSourceDirectory`]
/// - [`WasomeSourceFile`]
///
/// # Usage
///
/// This trait is primarily used by the [`duplicate_wasome_source_elements`]
/// helper function to check for naming conflicts across different element types.
///
/// # Example
///
/// ```ignore
/// fn print_name(element: &impl HasWasomeSourceElementLocation) {
///     println!("Element: {}", element.location().name());
/// }
/// ```
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
///
/// # Implementation
///
/// Uses a HashSet for O(n) duplicate detection.
///
/// # Example
///
/// ```ignore
/// let dirs = vec![dir1, dir2, dir3];
/// if let Some(dup) = duplicate_wasome_source_elements(dirs.iter()) {
///     eprintln!("Duplicate directory name: {}", dup);
/// }
/// ```
///
/// # Note
///
/// This function is private and should only be called from
/// [`WasomeSourceDirectory::new`].
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
