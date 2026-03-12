use crate::program_information::ProgramInformation;
use crate::source_element::{
    WasomeProgram, WasomeSourceDirectory,
    WasomeSourceElementLocation, WasomeSourceFile,
};
use io::FullIO;
use source::SourceMap;
use std::path::Path;

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
/// Complete source hierarchy with all files loaded
///
/// # Errors
///
/// IO Errors may occur during the collection process
pub(crate) fn collect_program(
    to_collect: &ProgramInformation,
    load_from: &mut SourceMap<impl FullIO>,
) -> Result<WasomeProgram, CollectionError> {
    Ok(WasomeProgram::new(
        WasomeSourceElementLocation::new(to_collect.path().to_path_buf()),
        to_collect
            .projects()
            .iter()
            .map(|project| {
                Result::<_, CollectionError>::Ok((
                    project.name().to_string(),
                    collect_dir(to_collect.path().join(project.name()), load_from)?,
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
/// - **`to_collect`**: Filesystem path to the directory to collect
/// - **`load_from`**: SourceMap for loading files and filesystem access
///
/// # Returns
///
/// Complete directory tree with all files and subdirectories
///
/// # Errors
///
/// IO Errors may occur during the collection process
fn collect_dir(
    to_collect: impl AsRef<Path>,
    load_from: &mut SourceMap<impl FullIO>,
) -> Result<WasomeSourceDirectory, CollectionError> {
    let subdirs = collect_subdirs(&to_collect, load_from)?;
    let files = collect_files(&to_collect, load_from)?;
    let location = WasomeSourceElementLocation::new(to_collect.as_ref().to_path_buf());

    Ok(WasomeSourceDirectory::new(location, subdirs, files)
        .expect("Duplicate entries are not allowed per IO doc"))
}

/// Collects all Wasome source files from a directory.
///
/// This function scans the provided directory for Wasome source files
/// (`.waso` or `.✨`), loads each file into the SourceMap, and returns
/// a vector of `WasomeSourceFile` handles.
///
/// # Parameters
///
/// - **`to_collect`**: Directory path to scan for source files
///     - Relative to the root of `load_from`
/// - **`load_from`**: SourceMap for loading files into memory
///
/// # Returns
///
/// A vec of `WasomeSourceFile` for all source files found
///
/// # Errors
///
/// IO Errors if a file cannot be loaded into the SourceMap
fn collect_files(
    to_collect: &impl AsRef<Path>,
    load_from: &mut SourceMap<impl FullIO>,
) -> Result<Vec<WasomeSourceFile>, CollectionError> {
    // Future improvement:
    // Remove the .collect()
    // This requires non-trivial lifetime changes in the io crate
    // However, it won't make a big performance difference for the time being to warrant higher
    // priority
    let files = list_wasome_files_in_dir(load_from, to_collect.as_ref())?.collect::<Vec<_>>();
    let files = files
        .into_iter()
        .map(|file| {
            let path = to_collect.as_ref().join(&file);
            let file_id = load_from.load_file(&path)?;
            let location = WasomeSourceElementLocation::new(path);

            Ok(WasomeSourceFile::new(location, file_id))
        })
        .collect::<Result<Vec<WasomeSourceFile>, io::Error>>()?;
    Ok(files)
}

/// Recursively collects all subdirectories from a directory.
///
/// This function lists all non-symlink subdirectories in the provided path,
/// recursively collects each one, and returns a vector of `WasomeSourceDirectory`
/// handles representing the directory hierarchy.
///
/// # Parameters
///
/// - **`to_collect`**: Directory path to scan for subdirectories
///     - Relative to the root of `load_from`
/// - **`load_from`**: SourceMap for loading files and filesystem access
///
/// # Returns
///
/// A vector of `WasomeSourceDirectory` handles for all subdirectories found
///
/// # Errors
///
/// IO Errors if a subdirectory cannot be accessed or collected
fn collect_subdirs(
    to_collect: &impl AsRef<Path>,
    load_from: &mut SourceMap<impl FullIO>,
) -> Result<Vec<WasomeSourceDirectory>, CollectionError> {
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
            let subdir_path = to_collect.as_ref().join(&dir);
            collect_dir(subdir_path, load_from)
        })
        .collect::<Result<Vec<WasomeSourceDirectory>, CollectionError>>()?;
    Ok(subdirs)
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
/// - **`dir`**: Directory path to scan (relative to [`SourceMap`] root)
///
/// # Returns
///
/// An iterator over filenames (with extensions) of all Wasome source files
/// in the provided directory.
///
/// # Errors
///
/// Errors if:
/// - Missing permissions to read the directory
/// - Directory not found
/// - Other filesystem access errors
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
///
/// # Note
///
/// This currently only has a single variant. This is there to make adding future ones easier
#[derive(Debug)]
pub enum CollectionError {
    Io(io::Error),
}

impl From<io::Error> for CollectionError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}
