use std::path::PathBuf;

use source::{SourceMap, loader::FileLoader, types::FileID};

/// Abstraction for retrieving source file content and metadata.
///
/// This trait decouples the diagnostic renderer from specific file loading implementations.
pub trait SourceLookup {
    fn get_content(&self, id: FileID) -> Option<&str>;
    fn get_path(&self, id: FileID) -> Option<&PathBuf>;
}

/// Implementation of [`SourceLookup`] for the standard [`SourceMap`].
impl<T: FileLoader> SourceLookup for SourceMap<T> {
    fn get_content(&self, id: FileID) -> Option<&str> {
        self.get_file(&id).map(|f| f.content())
    }

    fn get_path(&self, id: FileID) -> Option<&PathBuf> {
        self.get_file(&id).map(|f| f.path())
    }
}

/// A dummy implementation of [`SourceLookup`] that provides no data.
///
/// Useful for testing or when source code is not available.
pub struct NoSource;

impl SourceLookup for NoSource {
    fn get_content(&self, _id: FileID) -> Option<&str> {
        None
    }

    fn get_path(&self, _id: FileID) -> Option<&PathBuf> {
        None
    }
}
