use std::path::PathBuf;

use source::{SourceMap, loader::FileLoader, types::FileID};

pub trait SourceLookup {
    fn get_content(&self, id: FileID) -> Option<&str>;
    fn get_path(&self, id: FileID) -> Option<&PathBuf>;
}

impl<T: FileLoader> SourceLookup for SourceMap<T> {
    fn get_content(&self, id: FileID) -> Option<&str> {
        self.get_file(&id).map(|f| f.content())
    }

    fn get_path(&self, id: FileID) -> Option<&PathBuf> {
        self.get_file(&id).map(|f| f.path())
    }
}

pub struct NoSource;

impl SourceLookup for NoSource {
    fn get_content(&self, _id: FileID) -> Option<&str> {
        None
    }

    fn get_path(&self, _id: FileID) -> Option<&PathBuf> {
        None
    }
}
