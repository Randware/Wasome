use std::path::PathBuf;

use source::{SourceMap, types::FileID};

/// Interface for looking up source content and paths.
/// Decouples the Renderer from the specific storage mechanism.
pub trait SourceLookup {
    fn get_content(&self, id: FileID) -> Option<&str>;
    fn get_path(&self, id: FileID) -> Option<&PathBuf>;
}

impl SourceLookup for SourceMap {
    fn get_content(&self, id: FileID) -> Option<&str> {
        self.get_file(&id).map(|f| f.content())
    }

    fn get_path(&self, id: FileID) -> Option<&PathBuf> {
        self.get_file(&id).map(|f| f.path())
    }
}

/// A "Null Object" implementation for when no source is available
/// (e.g. CLI errors, file read errors, or testing without a map).
pub struct NoSource;

impl SourceLookup for NoSource {
    fn get_content(&self, _id: FileID) -> Option<&str> {
        None
    }

    fn get_path(&self, _id: FileID) -> Option<&PathBuf> {
        None
    }
}
