use std::env;
use std::path::{Path, PathBuf};

use linker::LinkableFile;

use crate::error::{CliError, CliResult};

pub const STDLIB_PROJECT_NAME: &str = "std";

const DEFAULT_STDLIB_REL_PATH: &str = "std";

/// Represents a specific compilation target and its configuration.
pub(crate) struct Target {
    pub name: String,
    pub root: PathBuf,
}

impl Target {
    pub fn new(name: String, root: PathBuf) -> Self {
        Self { name, root }
    }

    pub fn wasome_dir(&self) -> PathBuf {
        self.root.join("wasome")
    }

    pub fn bin_dir(&self) -> PathBuf {
        self.root.join("bin")
    }

    pub fn is_valid(&self) -> bool {
        self.wasome_dir().exists() && self.bin_dir().exists()
    }

    pub fn validate(&self) -> CliResult<()> {
        if !self.root.exists() {
            return Err(CliError::TargetNotFound(self.name.clone()));
        }

        let wasome = self.wasome_dir();
        let bin = self.bin_dir();

        let manifest_path = wasome.join(crate::manifest::MANIFEST_FILE);
        let content = std::fs::read_to_string(&manifest_path)
            .map_err(|_| CliError::StdlibNotFound(wasome.clone()))?;
        crate::manifest::Manifest::parse(&content)
            .map_err(|_| CliError::StdlibNotFound(wasome.clone()))?;

        if !bin.exists() || !bin.is_dir() {
            return Err(CliError::StdlibNotFound(bin));
        }

        Ok(())
    }

    pub fn load_archives(&self) -> CliResult<Vec<LinkableFile>> {
        let mut archives = Vec::new();

        if let Ok(entries) = std::fs::read_dir(self.bin_dir()) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() {
                    if let Some(ext) = path.extension() {
                        if ext == "a" || ext == "o" {
                            let linkable = LinkableFile::from_path(&path)
                                .map_err(|e| CliError::LinkFileError(path.clone(), e))?;
                            archives.push(linkable);
                        }
                    }
                }
            }
        }

        Ok(archives)
    }
}

/// Resolves the stdlib location and loads its components.
///
/// Default location: `<binary_dir>/std/`
/// Override: `--std <path>` (must contain `wasome/` and `bin/`)
pub(crate) struct StdlibResolver;

impl StdlibResolver {
    /// Lists all available targets in the stdlib root directory.
    pub fn list_targets() -> CliResult<Vec<String>> {
        let stdlib_base = Self::default_stdlib_path()?;

        let mut targets = Vec::new();
        if let Ok(entries) = std::fs::read_dir(&stdlib_base) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                        let target = Target::new(name.to_string(), path);
                        if target.is_valid() {
                            targets.push(target.name);
                        }
                    }
                }
            }
        }

        targets.sort();
        Ok(targets)
    }

    /// Determines the stdlib root directory for a specific target.
    pub fn resolve(target_name: &str, override_path: Option<&Path>) -> CliResult<Target> {
        let target_root = match override_path {
            Some(path) => path.to_path_buf(),
            None => Self::default_stdlib_path()?.join(target_name),
        };

        let target = Target::new(target_name.to_string(), target_root);
        target.validate()?;

        Ok(target)
    }

    /// Computes the default stdlib path relative to the current binary.
    fn default_stdlib_path() -> CliResult<PathBuf> {
        let exe = env::current_exe()?;
        let bin_dir = exe
            .parent()
            .expect("Binary should always have a parent directory");
        Ok(bin_dir.join(DEFAULT_STDLIB_REL_PATH))
    }
}
