use crate::{DirectoryLoader, FileLoader, PathResolver};
use std::ffi::OsString;
use std::fs;
use std::fs::{DirEntry, FileType, metadata, read_dir};
use std::io::Error;
use std::path::{Path, PathBuf};

/// Default loader for `.waso` files.
///
/// This implementation provides standard filesystem I/O for resolving paths,
/// loading file content, and listing directory entries.
pub struct WasomeLoader;

impl PathResolver for WasomeLoader {
    /// Resolves a relative path against a root to create a canonical, absolute path.
    ///
    /// This implementation uses [`fs::canonicalize`] which performs real filesystem
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
impl FileLoader for WasomeLoader {
    /// Loads the source file content from the provided path
    ///
    /// The default implementation performs real filesystem I/O using [`fs::read_to_string`].
    /// It also performs a safety check to ensure the file size does not exceed the addressing
    /// limit.
    ///
    /// # Arguments
    ///
    /// * `path` - The path to the file to load.
    ///
    /// # Returns
    ///
    /// * `Ok(String)` - The content of the loaded file.
    /// * `Err(Error)` - If an I/O error occurs or the file size gets exceeded.
    ///
    /// # Errors
    ///
    /// This function returns an error if:
    /// * The underlying filesystem operation fails (e.g. [`std::io::ErrorKind::NotFound`]).
    /// * The file size exceeds [`u32::MAX`] bytes (~4GB), which is the maximum size supported
    ///   by the internal [`BytePos`] type.
    ///
    fn load<F: AsRef<Path>>(path: F) -> Result<String, Error> {
        let metadata = fs::metadata(&path)?;
        if metadata.len() > (u32::MAX as u64) {
            return Err(Error::new(
                std::io::ErrorKind::FileTooLarge,
                format!(
                    "Source file '{}' exceeds 4GB limit",
                    path.as_ref().display()
                ),
            ));
        }
        let content = fs::read_to_string(&path)?;

        Ok(content)
    }
}

impl DirectoryLoader for WasomeLoader {
    fn list_files<'a, F: AsRef<Path> + 'a>(
        path: F,
    ) -> Result<impl Iterator<Item = OsString> + 'static, Error> {
        list_all_with_specific_property(path, entry_is_file)
    }

    fn list_subdirs<'a, F: AsRef<Path> + 'a>(
        path: F,
    ) -> Result<impl Iterator<Item = OsString> + 'static, Error> {
        list_all_with_specific_property(path, entry_is_directory)
    }
}

/// Reads the provided directory and returns the names of all elements whose path satisfies a condition.
///
/// There is no filtering of the contents of the directory (e.g. hidden files).
///
/// # Parameters
///
/// * `directory` - The directory to read from.
/// * `condition` - The property that needs to be satisfied.
///
///     **Parameter:**
///     * The element to check
///
///     **Return:**
///     * `true` - The element satisfies the condition.
///     * `false` - The element does not satisfy the condition.
///
/// # Returns
///
/// * `Ok(Elements)` - All elements that satisfied the property are returned.
///   Note that the iterator may be empty.
/// * `Err(Error)` - An IO error occurred.
fn list_all_with_specific_property<
    'a,
    F: AsRef<Path> + 'a,
    Condition: FnMut(&DirEntry) -> Result<bool, Error>,
>(
    directory: F,
    mut condition: Condition,
) -> Result<impl Iterator<Item = OsString> + 'static, Error> {
    Ok(read_dir(directory)?
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|elem| (condition(&elem), elem))
        .map(|elem| elem.0.map(|inner| (inner, elem.1)))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .filter(|elem| elem.0)
        .map(|elem| elem.1)
        .map(|elem| elem.file_name()))
}

/// Checks if the provided entry is a file after all symlinks are resolved
///
/// # Parameter
///
/// **to_check** - The entry
///
/// # Return
///
/// **Ok(bool)** - Is the entry a file?
/// **Err(Error)** - There was an IO error
fn entry_is_file(to_check: &DirEntry) -> Result<bool, Error> {
    dir_entry_has_file_type_condition(to_check, |ft| ft.is_file())
}

/// Checks if the provided entry is a directory after all symlinks are resolved
///
/// # Parameter
///
/// **to_check** - The entry
///
/// # Return
///
/// **Ok(bool)** - Is the entry a directory?
/// **Err(Error)** - There was an IO error
fn entry_is_directory(to_check: &DirEntry) -> Result<bool, Error> {
    dir_entry_has_file_type_condition(to_check, |ft| ft.is_dir())
}

fn dir_entry_has_file_type_condition<Condition: FnMut(FileType) -> bool>(
    to_check: &DirEntry,
    mut condition: Condition,
) -> Result<bool, Error> {
    let file_type = to_check.file_type()?;
    if file_type.is_symlink() {
        return metadata(to_check.path()).map(|inner| condition(inner.file_type()));
    }
    Ok(condition(file_type))
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
        let file_path = root.join("main.waso");
        File::create(&file_path).unwrap();

        // Test resolving
        let resolved = WasomeLoader::resolve(root, Path::new("main.waso"));
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
        assert_eq!(&source_file, "Hello World");
    }

    #[test]
    fn test_list_files_mixed_content() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        File::create(root.join("file1.txt")).unwrap();
        File::create(root.join("file2.rs")).unwrap();

        fs::create_dir(root.join("subdir")).unwrap();

        let files: Vec<OsString> = WasomeLoader::list_files(root)
            .expect("Should list files")
            .collect();

        assert!(files.contains(&OsString::from("file1.txt")));
        assert!(files.contains(&OsString::from("file2.rs")));
        assert!(!files.contains(&OsString::from("subdir")));

        assert_eq!(files.len(), 2);
    }

    #[test]
    fn test_list_subdirs_mixed_content() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        File::create(root.join("file.txt")).unwrap();

        fs::create_dir(root.join("sub1")).unwrap();
        fs::create_dir(root.join("sub2")).unwrap();

        let subdirs: Vec<OsString> = WasomeLoader::list_subdirs(root)
            .expect("Should list subdirs")
            .collect();

        assert!(subdirs.contains(&OsString::from("sub1")));
        assert!(subdirs.contains(&OsString::from("sub2")));
        assert!(!subdirs.contains(&OsString::from("file.txt")));

        assert_eq!(subdirs.len(), 2);
    }

    #[test]
    fn test_directory_loader_empty() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        let files = WasomeLoader::list_files(root).expect("Should succeed");
        assert_eq!(files.count(), 0);

        let subdirs = WasomeLoader::list_subdirs(root).expect("Should succeed");
        assert_eq!(subdirs.count(), 0);
    }

    #[test]
    fn test_directory_loader_not_found() {
        let dir = tempdir().unwrap();
        let root = dir.path().join("non_existent");

        let files_res = WasomeLoader::list_files(&root);
        assert!(files_res.is_err());
        if let Err(e) = files_res {
            assert_eq!(e.kind(), std::io::ErrorKind::NotFound);
        } else {
            panic!("Should be error");
        }

        let subdirs_res = WasomeLoader::list_subdirs(&root);
        assert!(subdirs_res.is_err());
        if let Err(e) = subdirs_res {
            assert_eq!(e.kind(), std::io::ErrorKind::NotFound);
        } else {
            panic!("Should be error");
        }
    }

    #[test]
    // We need platform specific features for this test.
    // So it won't work on Windows
    // How sad /s
    #[cfg(unix)]
    fn test_directory_loader_symlinks() {
        use std::os::unix::fs::symlink;
        let dir = tempdir().unwrap();
        let root = dir.path();

        let real_path = root.join("real");
        fs::create_dir(real_path.clone()).unwrap();
        fs::create_dir(real_path.clone().join("real_dir")).unwrap();

        File::create(real_path.clone().join("real_file")).unwrap();

        symlink(
            real_path.clone().join("real_file"),
            root.join("link_to_file"),
        )
        .unwrap();
        symlink(real_path.clone().join("real_dir"), root.join("link_to_dir")).unwrap();

        let files: Vec<OsString> = WasomeLoader::list_files(root)
            .expect("Should list files")
            .collect();
        assert!(!files.contains(&OsString::from("real_file")));
        assert!(files.contains(&OsString::from("link_to_file")));
        assert!(!files.contains(&OsString::from("real_dir")));
        assert!(!files.contains(&OsString::from("link_to_dir")));
        assert!(!files.contains(&OsString::from("real")));

        assert_eq!(1, files.len());

        let subdirs: Vec<OsString> = WasomeLoader::list_subdirs(root)
            .expect("Should list subdirs")
            .collect();
        assert!(!subdirs.contains(&OsString::from("real_dir")));
        assert!(subdirs.contains(&OsString::from("link_to_dir")));
        assert!(!subdirs.contains(&OsString::from("real_file")));
        assert!(!subdirs.contains(&OsString::from("link_to_file")));
        assert!(subdirs.contains(&OsString::from("real")));

        assert_eq!(2, subdirs.len());
    }
}
