use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::error::{ManifestError, ManifestResult};

pub const MANIFEST_FILE: &str = "waso.toml";
pub const BINARY_ENTRY_FILE: &str = "src/main.waso";
pub const LIBRARY_ENTRY_FILE: &str = "src/lib.waso";
pub const LIB_PATH: &str = "lib/";

/// The top-level configuration structure.
#[derive(Debug, Deserialize)]
pub struct Manifest {
    pub project: ProjectMetadata,
    pub dependencies: Option<HashMap<String, String>>,
}

#[derive(Debug, Deserialize)]
pub struct ProjectMetadata {
    pub name: String,
    pub version: String,
}

impl Manifest {
    /// Walk up the file tree until a manifest file is found.
    pub fn find(start: impl AsRef<Path>) -> ManifestResult<PathBuf> {
        let mut current = fs::canonicalize(start)?;

        if current.is_file() {
            current.pop();
        }

        loop {
            let candidate = current.join(MANIFEST_FILE);

            if candidate.exists() {
                return Ok(candidate);
            }

            if !current.pop() {
                return Err(ManifestError::NotFound);
            }
        }
    }

    /// Parse a manifest file
    pub fn parse(content: &str) -> Result<Self, toml::de::Error> {
        let config: Manifest = toml::from_str(content)?;
        Ok(config)
    }

}
