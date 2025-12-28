use std::collections::HashMap;
use std::fs;
use std::io::{Error, ErrorKind};
use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex};

use source::SourceFile;
use source::loader::FileLoader;

pub static MOCK_FS: LazyLock<Mutex<HashMap<PathBuf, String>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

pub struct MockLoader;

impl MockLoader {
    pub fn add_file(path: impl Into<PathBuf>, content: &str) {
        MOCK_FS
            .lock()
            .unwrap()
            .insert(path.into(), content.to_string());
    }

    pub fn load_from_disk(virtual_path: &str, disk_filename: &str) {
        let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();

        let real_path = Path::new(&manifest).join("tests/src").join(disk_filename);

        let content = fs::read_to_string(&real_path)
            .expect(&format!("Could not read test file at: {:?}", real_path));

        Self::add_file(virtual_path, &content);
    }

    pub fn reset() {
        MOCK_FS.lock().unwrap().clear();
    }
}

impl FileLoader for MockLoader {
    fn load<F: AsRef<Path>>(path: F) -> Result<SourceFile, Error> {
        let path = path.as_ref().to_path_buf();
        let fs = MOCK_FS.lock().unwrap();

        match fs.get(&path) {
            Some(content) => Ok(SourceFile::new(path, content.clone())),
            None => Err(Error::new(
                ErrorKind::NotFound,
                format!("Mock file not found: {:?}", path),
            )),
        }
    }

    fn resolve<T: AsRef<Path>, F: AsRef<Path>>(
        root_path: T,
        relative_path: F,
    ) -> Result<PathBuf, Error> {
        Ok(root_path.as_ref().join(relative_path))
    }
}
