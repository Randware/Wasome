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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_parse_valid() {
        let toml = r#"
            [project]
            name = "test"
            version = "1.0.0"
        "#;
        let manifest = Manifest::parse(toml).unwrap();

        assert_eq!(manifest.project.name, "test");
        assert_eq!(manifest.project.version, "1.0.0");
        assert!(manifest.dependencies.is_none());
    }

    #[test]
    fn test_parse_invalid() {
        let toml = r#"
            [project]
            name = "test"
            # missing version
        "#;

        assert!(Manifest::parse(toml).is_err());
    }

    #[test]
    fn test_find_manifest() {
        let dir = tempdir().unwrap();
        let canonical_dir = fs::canonicalize(dir.path()).unwrap();
        let manifest_path = canonical_dir.join(MANIFEST_FILE);
        fs::write(&manifest_path, "").unwrap();

        let found = Manifest::find(&canonical_dir).unwrap();

        assert_eq!(found, manifest_path);

        let deep = dir.path().join("src").join("nested");
        fs::create_dir_all(&deep).unwrap();
        let found2 = Manifest::find(&deep).unwrap();

        assert_eq!(found2, manifest_path);
    }

    #[test]
    fn test_find_manifest_not_found() {
        let dir = tempdir().unwrap();
        let err = Manifest::find(dir.path()).unwrap_err();

        assert!(matches!(err, ManifestError::NotFound));
    }
}
