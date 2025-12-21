use std::path::PathBuf;

use source::{SourceMap, types::FileID};

pub trait SourceLookup {
    fn get_content(&self, id: FileID) -> Option<&str>;

    fn get_path(&self, id: FileID) -> Option<&PathBuf>;
}

impl SourceLookup for SourceMap {
    fn get_content(&self, id: FileID) -> Option<&str> {
        match self.get_file(&id) {
            Some(f) => Some(f.content()),
            None => None,
        }
    }

    fn get_path(&self, id: FileID) -> Option<&PathBuf> {
        match self.get_file(&id) {
            Some(f) => Some(f.path()),
            None => None,
        }
    }
}

pub struct NoSource;

impl SourceLookup for NoSource {
    fn get_content(&self, id: FileID) -> Option<&str> {
        None
    }

    fn get_path(&self, id: FileID) -> Option<&PathBuf> {
        None
    }
}
