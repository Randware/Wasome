use crate::program_information::ProgramInformation;
use crate::source_collector::source_element::{
    WasomeProgram, WasomeSourceDirectory, WasomeSourceDirectoryCreationError,
    WasomeSourceElementLocation, WasomeSourceFile,
};
use io::FullIO;
use source::SourceMap;
use std::path::Path;

pub mod source_element;

/// All valid wasome file extensions
const WASOME_FILE_ENDINGS: &[&str] = &[".waso", ".✨"];

/// Collects the complete source file structure of a Wasome program from disk.
///
/// This function serves as the entry point for the source collection phase.
/// It recursively traverses all project directories, loads source files into
/// the provided SourceMap, and builds a complete representation of the program
/// structure before any parsing occurs.
///
/// # Parameters
///
/// - **`to_collect`**: Program metadata containing the program name and list of projects
/// - **`load_from`**: SourceMap for loading files into memory and providing filesystem access
///
/// # Returns
///
/// - **`Ok(WasomeProgram)`**: Complete source hierarchy with all files loaded
/// - **`Err(CollectionError)`**: If filesystem access or directory validation fails
///
/// # Example
///
/// ```ignore
/// let program_info = ProgramInformation::new("my_program", projects);
/// let mut source_map = SourceMap::new();
/// let program = collect_program(&program_info, &mut source_map)?;
/// ```
pub(crate) fn collect_program(
    to_collect: &ProgramInformation,
    load_from: &mut SourceMap<impl FullIO>,
) -> Result<WasomeProgram, CollectionError> {
    Ok(WasomeProgram::new(
        WasomeSourceElementLocation::new(
            to_collect.name().to_string(),
            to_collect.path().to_path_buf(),
        ),
        to_collect
            .projects()
            .iter()
            .map(|project| {
                Result::<_, CollectionError>::Ok((
                    project.name().to_string(),
                    collect_dir(
                        project.name().to_string(),
                        to_collect.path().join(project.name()),
                        load_from,
                    )?,
                ))
            })
            .collect::<Result<_, _>>()?,
    ))
}
/// Recursively collects all files and subdirectories from a directory.
///
/// This function traverses the directory tree starting at the provided path,
/// loading all Wasome source files (`.waso` or `.✨`) into the SourceMap and
/// building a hierarchical representation of the directory structure.
///
/// # Parameters
///
/// - **`name`**: Human-readable name for this directory (used in location metadata)
/// - **`to_collect`**: Filesystem path to the directory to collect
/// - **`load_from`**: SourceMap for loading files and filesystem access
///
/// # Returns
///
/// - **`Ok(WasomeSourceDirectory)`**: Complete directory tree with all files and subdirectories
/// - **`Err(CollectionError)`**: If filesystem access fails or directory structure is invalid
///
/// # Behavior
///
/// 1. Lists all non-symlink subdirectories
/// 2. Recursively calls itself for each subdirectory
/// 3. Lists all Wasome source files using [`list_wasome_files_in_dir`]
/// 4. Loads each file into SourceMap to obtain a FileID
/// 5. Creates WasomeSourceFile instances for each file
/// 6. Constructs and returns WasomeSourceDirectory with validation
///
/// # Note
///
/// This function is private and should only be called from [`collect_program`].
// TODO: Split this up
fn collect_dir(
    name: String,
    to_collect: impl AsRef<Path>,
    load_from: &mut SourceMap<impl FullIO>,
) -> Result<WasomeSourceDirectory, CollectionError> {
    // Future improvement:
    // Remove the .collect()
    // This requires non-trivial lifetime changes in the io crate
    // However, it won't make a big performance difference for the time being to warrant higher
    // priority

    let subdirs = load_from
        .loader()
        .list_non_symlink_subdirs(&to_collect)?
        .collect::<Vec<_>>();
    let subdirs = subdirs
        .into_iter()
        .map(|dir| {
            let subdir_name = dir.to_string_lossy().to_string();
            let subdir_path = to_collect.as_ref().join(&dir);
            collect_dir(subdir_name, subdir_path, load_from)
        })
        .collect::<Result<Vec<WasomeSourceDirectory>, CollectionError>>()?;
    let files = list_wasome_files_in_dir(load_from, to_collect.as_ref())?.collect::<Vec<_>>();
    let files = files
        .into_iter()
        .map(|file| {
            let path = to_collect.as_ref().join(&file);
            let file_id = load_from.load_file(&path)?;
            let location = WasomeSourceElementLocation::new(file, path);

            Ok(WasomeSourceFile::new(location, file_id))
        })
        .collect::<Result<Vec<WasomeSourceFile>, io::Error>>()?;
    let location = WasomeSourceElementLocation::new(name, to_collect.as_ref().to_path_buf());

    Ok(WasomeSourceDirectory::new(location, subdirs, files)?)
}

/// Lists all Wasome source files in the provided directory.
///
/// This function scans a directory and filters its contents to return only
/// files with valid Wasome extensions (`.waso` or `.✨`). Files with non-UTF8
/// filenames are silently skipped as they may not be Wasome source files.
///
/// # Parameters
///
/// - **`load_from`**: SourceMap providing filesystem access via its loader
/// - **`dir`**: Directory path to scan (relative to SourceMap root)
///
/// # Returns
///
/// An iterator over filenames (with extensions) of all Wasome source files
/// in the provided directory. Returns an error if the directory cannot be read.
///
/// # Errors
///
/// Returns an `io::Error` if:
/// - Missing permissions to read the directory
/// - Directory not found
/// - Other filesystem access errors
///
/// # Note
///
/// This function is private and should only be called from [`collect_dir`].
fn list_wasome_files_in_dir<'b>(
    load_from: &'b mut SourceMap<impl FullIO>,
    dir: &'b Path,
) -> Result<impl Iterator<Item = String> + 'b, io::Error> {
    Ok(load_from
        .loader()
        .list_files(load_from.root_path().join(dir))?
        // Skip files with non-UTF8 filenames
        // They might be non-wasome files so we don't want to hard-fail
        .filter_map(|file_name| file_name.into_string().ok())
        .filter(|file| {
            WASOME_FILE_ENDINGS
                .iter()
                .any(|ending| file.ends_with(ending))
        }))
}

/// Error type for source collection failures.
///
/// This enum represents errors that can occur during the source collection phase,
/// when traversing the filesystem and loading source files into the SourceMap.
///
/// # Variants
///
/// - **`Io`**: Underlying filesystem I/O error
/// - **`WasomeSourceDirectoryCreationError`**: Directory structure validation error
#[derive(Debug)]
pub enum CollectionError {
    Io(io::Error),
    WasomeSourceDirectoryCreationError(WasomeSourceDirectoryCreationError),
}

impl From<io::Error> for CollectionError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<WasomeSourceDirectoryCreationError> for CollectionError {
    fn from(value: WasomeSourceDirectoryCreationError) -> Self {
        Self::WasomeSourceDirectoryCreationError(value)
    }
}
