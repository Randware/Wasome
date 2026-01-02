//! Contains traits that abstract io:
//!
//! * **PathResolver** - Resolves paths.
//!
//!     These resolved paths are used by the following traits.
//! * **FileLoader** - Loads individual files.
//! * **DirectoryLoader** - Loads files from directories.
//!
//!     More precisely, it allows for files inside a directory to be listed, similar to the
//!     ls command on UNIX.
//!
//! These traits allow the compiler to be decoupled from the physical disk enabling:
//! * **Testing:** Using a mock in-memory filesystem.
//! * **Virtualization:** Loading files from archives (ZIP, TAR) or URLs (which does not sound
//!   useful, but is a nice-to-have)
//! * **Sandboxing:** Restricting file access to specific directories.

use std::ffi::OsString;
use std::path::PathBuf;
pub use std::{io::Error, path::Path};

/// Resolve paths.
///
/// See [`resolve`](PathResolver::resolve) and the module level documentation for more details.
pub trait PathResolver {
    /// Resolves a relative path against a root path to produce a canonical identifier.
    ///
    /// This method is responsible for combining paths and handling platform-specific
    /// details (like symlinks or `..` components). The resulting [`PathBuf`] will be
    /// used as the unique key in the [`source::source::SourceMap`]'s cache.
    ///
    /// # Arguments
    ///
    /// * `root_path` - The root directory of the compilation context.
    /// * `relative_path` - The import path or file path relative to the root.
    fn resolve<T: AsRef<Path>, F: AsRef<Path>>(
        root_path: T,
        relative_path: F,
    ) -> Result<PathBuf, Error>;
}

/// Loads files.
///
/// See [`resolve`](FileLoader::load) and the module level documentation for more details.
pub trait FileLoader {
    /// Loads the content of a source file into memory.
    ///
    /// Implementations should read the bytes from the given `path` and return
    /// a [`String`] containing the raw content
    ///
    /// # Arguments
    ///
    /// * `path` - The absolute or canonical path to the file.
    ///
    /// # Returns
    ///
    /// * `Ok(String)` - The loaded file.
    /// * `Err(Error)` - If the file cannot be read or is too large.
    fn load<F: AsRef<Path>>(path: F) -> Result<String, Error>;
}

/// Loads contents of directories.
///
/// This allows for both files and subdirectories to be listed.
///
/// See the method and  module level documentation for more details.
pub trait DirectoryLoader {
    /// Lists the files of a given directory
    ///
    /// There is no filtering (e.g.: hidden files) and symlinks are resolved.
    ///
    /// # Arguments
    ///
    /// * `path` - The absolute or canonical path to the directory.
    ///
    /// # Returns
    ///
    /// * `Ok(impl Iterator<Item=OsString>)` - An iterator over the filenames (not paths).
    /// * `Err(Error)` - If an IO error occurred.
    fn list_files<'a, F: AsRef<Path> + 'a>(
        path: F,
    ) -> Result<impl Iterator<Item = OsString> + 'static, Error>;

    /// Lists the subdirectories of a given directory
    ///
    /// There is no filtering (e.g.: hidden directories) and symlinks are resolved.
    ///
    /// # Arguments
    ///
    /// * `path` - The absolute or canonical path to the directory.
    ///
    /// # Returns
    ///
    /// * `Ok(impl Iterator<Item=OsString>)` - An iterator over the names of the subdirectories (not paths).
    /// * `Err(Error)` - If an IO error occurred.
    fn list_subdirs<'a, F: AsRef<Path> + 'a>(
        path: F,
    ) -> Result<impl Iterator<Item = OsString> + 'static, Error>;
}

/// Helper trait that implements both [`PathResolver`] and [`FileLoader`]
///
/// It has now functionality on its own and is automatically implemented for all applicable types.
///
/// It only exists to make trait bounds simpler
pub trait FileIO: PathResolver + FileLoader {}

impl<T: PathResolver + FileLoader> FileIO for T {}

/// Helper trait that implements both [`PathResolver`] and [`FileLoader`]
///
/// It has now functionality on its own and is automatically implemented for all applicable types.
///
/// It only exists to make trait bounds simpler
pub trait DirectoryIO: PathResolver + DirectoryLoader {}

impl<T: PathResolver + DirectoryLoader> DirectoryIO for T {}

/// Helper trait that implements [`PathResolver`], [`DirectoryLoader`] and [`FileLoader`]
///
/// It has now functionality on its own and is automatically implemented for all applicable types.
///
/// It only exists to make trait bounds simpler
pub trait FullIO: PathResolver + DirectoryLoader + FileLoader {}

impl<T: PathResolver + DirectoryLoader + FileLoader> FullIO for T {}

#[cfg(test)]
mod tests {
    use crate::{FileIO, FullIO};

    // This is only here to check if it compiles
    #[allow(dead_code)]
    fn cast(input: impl FullIO) -> impl FileIO {
        input
    }
}