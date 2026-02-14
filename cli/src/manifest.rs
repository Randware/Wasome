use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::error::ManifestResult;

pub const MANIFEST_NAME: &'static str = "waso.toml";

/// The top-level configuration structure.
#[derive(Debug, Deserialize)]
pub struct Manifest {
    pub project: ProjectMetadata,
    pub bin: Option<BinConfig>,
    pub dependencies: Option<HashMap<String, String>>,
}

#[derive(Debug, Deserialize)]
pub struct ProjectMetadata {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Deserialize)]
pub struct BinConfig {
    // The output binary name
    pub bin: String,
    // The source entry point
    pub entry: String,
}

impl Manifest {
    /// Walk up the file tree until a manifest file is found.
    pub fn find(start: impl AsRef<Path>) -> ManifestResult<PathBuf> {
        let mut current = fs::canonicalize(start)?;

        if current.is_file() {
            current.pop();
        }

        loop {
            let candidate = current.join(MANIFEST_NAME);

            if candidate.exists() {
                return Ok(candidate);
            }

            if !current.pop() {
                return Err(crate::error::ManifestError::NotFound);
            }
        }
    }

    /// Parse a manifest file
    pub fn parse(content: &str) -> ManifestResult<Self> {
        let config: Manifest = toml::from_str(content)?;
        Ok(config)
    }

    /// Attempt to load and parse the config file from the given path.
    pub fn load(path: impl AsRef<Path>) -> ManifestResult<Self> {
        let content = fs::read_to_string(path)?;
        Self::parse(&content)
    }

    /// Locates the manifest, loads it, and returns the pair.
    pub fn discover(start: impl AsRef<Path>) -> ManifestResult<(Self, PathBuf)> {
        let path = Self::find(start)?;

        let manifest = Self::load(&path)?;

        Ok((manifest, path))
    }

    /// Helper to determine if the current project is a library or binary
    pub fn is_library(&self) -> bool {
        self.bin.is_none()
    }
}
