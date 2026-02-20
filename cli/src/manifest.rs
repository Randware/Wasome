use driver::program_information::Project;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::error::{ManifestError, ManifestResult};
use crate::manifest;

pub const MANIFEST_NAME: &'static str = "waso.toml";
pub const LIB_PATH: &'static str = "lib/";

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
                return Err(ManifestError::NotFound);
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

    /// Resolves ALL dependencies recursively.
    pub fn resolve_dependencies(&self, project_root: &Path) -> ManifestResult<Vec<Project>> {
        let mut resolved_projects = Vec::new();

        let initial_chain = vec![format!("{}@{}", self.project.name, self.project.version)];

        self.resolve_recursive(project_root, &mut resolved_projects, initial_chain)?;

        Ok(resolved_projects)
    }

    /// Internal recursive helper with versioned stack trace
    fn resolve_recursive(
        &self,
        project_root: &Path,
        acc: &mut Vec<Project>,
        chain: Vec<String>,
    ) -> ManifestResult<()> {
        let deps = match &self.dependencies {
            Some(d) => d,
            None => return Ok(()),
        };

        let lib_root = project_root.join(LIB_PATH);

        for (name, version) in deps {
            let folder_name = format!("{}@{}", name, version);
            let dep_path = lib_root.join(&folder_name);
            let dep_manifest_path = dep_path.join(manifest::MANIFEST_NAME);

            if !dep_manifest_path.exists() {
                let chain_display = chain.join("/");

                return Err(ManifestError::MissingDependency(
                    folder_name.clone(),
                    chain_display,
                    format!("{}{}", LIB_PATH, folder_name),
                ));
            }

            let dep_manifest = Self::load(&dep_manifest_path)?;

            let dep_root_path = dep_path
                .strip_prefix(project_root)
                .unwrap_or(&dep_path)
                .to_path_buf();

            acc.push(Project::new(
                dep_manifest.project.name.clone(),
                dep_root_path.clone(),
            ));

            let mut next_chain = chain.clone();
            next_chain.push(format!(
                "{}@{}",
                dep_manifest.project.name, dep_manifest.project.version
            ));

            dep_manifest.resolve_recursive(&dep_root_path.to_path_buf(), acc, next_chain)?;
        }

        Ok(())
    }

    /// Helper to determine if the current project is a library or binary
    pub fn is_library(&self) -> bool {
        self.bin.is_none()
    }
}
