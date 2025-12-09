use std::path::PathBuf;
pub(crate) use std::{fs, io::Error, path::Path};

use crate::source::SourceFile;

/// A trait that abstracts file system operations for the [`crate::SourceMap`]
///
/// This trait allows the compiler to be decoupled from the physical disk enabling:
/// * **Testing:** Using a mock in-memory filesystem.
/// * **Virtualization:** Loading files from archives (ZIP, TAR) or URLs (which does not sound
///   useful, but is a nice-to-have)
/// * **Sandboxing:** Restricting file access to specific directories.
pub trait FileLoader {
    /// Loads the content of a source file into memory.
    ///
    /// Implementations should read the bytes from the given `path` and return
    /// a [`SourceFile`] struct containing the raw content and line indices.
    ///
    /// # Arguments
    ///
    /// * `path` - The absolute or canonical path to the file.
    ///
    /// # Returns
    ///
    /// * `Ok(SourceFile)` - The loaded and parsed source file.
    /// * `Err(Error)` - If the file cannot be read or is too large.
    fn load<F: AsRef<Path>>(path: F) -> Result<SourceFile, Error>;

    /// Resolves a relative path against a root path to produce a canonical identifier.
    ///
    /// This method is responsible for combining paths and handling platform-specific
    /// details (like symlinks or `..` components). The resulting [`PathBuf`] will be
    /// used as the unique key in the [`crate::SourceMap`]'s cache.
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

/// Default loader for `.waso` files
pub struct WasomeLoader;

impl FileLoader for WasomeLoader {
    /// Loads the source file content from the provided path
    ///
    /// The default implementation performs real filesystem I/O using [`std::fs::read_to_string`].
    /// It also performs a safety check to ensure the file size does not exceed the addressing
    /// limit.
    ///
    /// # Arguments
    ///
    /// * `path` - The path to the file to load.
    ///
    /// # Returns
    ///
    /// * `Ok(SourceFile)` - A new struct containing the path normalized content and line indices.
    /// * `Err(Error)` - If an I/O error occurs or the file size gets exceeded.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// * The underlying filesystem operation fails (e.g. [`std::io::ErrorKind::NotFound`]).
    /// * The file size exceeds [`u32::MAX`] bytes (~4GB), which is the maximum size supported
    ///   by the internal [`BytePos`] type.
    ///
    fn load<F: AsRef<Path>>(path: F) -> Result<SourceFile, Error> {
        let content = fs::read_to_string(&path)?;

        if content.len() > (u32::MAX as usize) {
            return Err(Error::new(
                std::io::ErrorKind::FileTooLarge,
                format!(
                    "Source file '{}' exceeds 4GB limit",
                    path.as_ref().display()
                ),
            ));
        }
        Ok(SourceFile::new(path.as_ref().to_path_buf(), content))
    }

    /// Resolves a relative path against a root to create a canonical, absolute path.
    ///
    /// This implementation uses [`std::fs::canonicalize`] which performs real filesystem
    /// operations to resolve symbolic links and ensure the path exists.
    ///
    /// # Arguments
    ///
    /// * `root_path` - The base path (e.g. project root).
    /// * `relative_path` - The path relative to the root.
    ///
    /// # Returns
    ///
    /// * `Ok(PathBuf)` - The absolute and canonicalized path.
    /// * `Err(Error)` - If the path does not exist or cannot be accessed.
    fn resolve<T: AsRef<Path>, F: AsRef<Path>>(
        root_path: T,
        relative_path: F,
    ) -> Result<PathBuf, Error> {
        let path = root_path.as_ref().join(relative_path);

        fs::canonicalize(path.as_path())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs::File, io::Write};
    use tempfile::tempdir;

    #[test]
    fn test_resolve_happy_path() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        // Create a real file so canonicalize works
        // NOTE: A MockFS would not work because the real
        // Wasome loader canonicalizes the paths, which means all files
        // have to exist
        let file_path = root.join("main.wasom");
        File::create(&file_path).unwrap();

        // Test resolving
        let resolved = WasomeLoader::resolve(root, Path::new("main.wasom"));
        assert!(resolved.is_ok());

        // Canonicalize usually resolves symlinks and absolute paths.
        // It should match the file_path we created.
        assert_eq!(resolved.unwrap(), fs::canonicalize(&file_path).unwrap());
    }

    #[test]
    fn test_resolve_not_found() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        // We don't create the file
        let resolved = WasomeLoader::resolve(root, Path::new("ghost.waso"));

        // Canonicalize fails on non-existent files
        assert!(resolved.is_err());
        assert_eq!(resolved.unwrap_err().kind(), std::io::ErrorKind::NotFound);
    }

    #[test]
    fn test_load_happy_path() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        let content = "Hello World";

        let mut file = File::create(&file_path).unwrap();
        write!(file, "{}", content).unwrap();

        let source_file = WasomeLoader::load(&file_path).expect("Should load");

        // USE THE GETTER! It is public, so use it.
        assert_eq!(source_file.content(), "Hello World");
    }
}
