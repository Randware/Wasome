use std::path::Path;
use io::FullIO;
use source::SourceMap;
use crate::error::DriverError;
use crate::program_information::ProgramInformation;
use crate::source_collector::source_element::{WasomeProgram, WasomeSourceDirectory, WasomeSourceDirectoryCreationError, WasomeSourceElementLocation, WasomeSourceFile};

pub mod source_element;

/// All valid wasome file extensions
const WASOME_FILE_ENDINGS: &[&str] = &[".waso", ".✨"];

pub(crate) fn collect_program(to_collect: &ProgramInformation, load_from: &mut SourceMap<impl FullIO>) -> Result<WasomeProgram, CollectionError> {
    Ok(WasomeProgram::new(
        WasomeSourceElementLocation::new(to_collect.name().to_string(), to_collect.path().to_path_buf()),
        to_collect.projects().iter()
            .map(|project|
                Result::<_, CollectionError>::Ok((project.name().to_string(),
                 collect_dir(project.name().to_string(), to_collect.path().join(project.name()), load_from)?)))
            .collect::<Result<_, _>>()?
    ))
}
// TODO: Split this up
fn collect_dir(name: String, to_collect: impl AsRef<Path>, load_from: &mut SourceMap<impl FullIO>) -> Result<WasomeSourceDirectory, CollectionError> {
    // Future improvement:
    // Remove the .collect()
    // This requires non-trivial lifetime changes in the io crate
    // However, it won't make a big performance difference for the time being to warrant higher
    // priority
    let subdirs = load_from.loader().list_non_symlink_subdirs(&to_collect)?.collect::<Vec<_>>();
    let subdirs = subdirs
        .into_iter()
        .map(|dir| {
            let subdir_name = dir.to_string_lossy().to_string();
            let subdir_path = to_collect.as_ref().join(&dir);
            collect_dir(subdir_name, subdir_path, load_from)
        })
        .collect::<Result<Vec<WasomeSourceDirectory>, CollectionError>>()?;
    let files = list_wasome_files_in_dir(load_from, to_collect.as_ref())?.collect::<Vec<_>>();
    let files = files.into_iter()
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

/// Lists all Wasome source files in the provided directory
///
/// # Parameters
///
/// - **`module_path`** - The path of the module the file belongs to
///     - Relative to the root of the [`SourceMap`]
///
/// # Return
///
/// An iterator over the filenames of all wasome files in the provided directory
///     - Including file extensions
///
/// # Errors
///
/// There was an IO error, for example
/// - Missing permissions
/// - Directory not found
fn list_wasome_files_in_dir<'b>(
    load_from: &'b mut SourceMap<impl FullIO>,
    dir: &'b Path,
) -> Result<impl Iterator<Item = String> + 'b, io::Error> {
    Ok(load_from.loader()
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

#[derive(Debug)]
pub enum CollectionError {
    Io(io::Error),
    WasomeSourceDirectoryCreationError(WasomeSourceDirectoryCreationError)
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