use std::env;
use std::path::{Path, PathBuf};

use linker::LinkableFile;

use crate::error::{CliError, CliResult};

const DEFAULT_STDLIB_REL_PATH: &str = "std";
const STDLIB_ARCHIVE_NAME: &str = "libstd.a";

pub const STDLIB_PROJECT_NAME: &str = "std";

/// Resolved paths to the stdlib components.
///
/// Separates the wasome source project and the pre-compiled Rust archive,
/// making it straightforward to extend with target-based selection
/// (e.g. `runtime/` vs `browser/` sub-directories) in the future.
pub(crate) struct StdlibPaths {
    /// Path to the wasome stdlib project root (contains `waso.toml` and `src/`).
    pub wasome: PathBuf,
    /// Path to the pre-compiled Rust archive (`.a` file).
    pub archive: PathBuf,
}

/// Resolves the stdlib location and loads its components.
///
/// Default location: `<binary_dir>/std/`
/// Override: `--stdlib <path>` (must contain `wasome/` and `rust/<STDLIB_ARCHIVE_NAME>`)
pub(crate) struct StdlibResolver;

impl StdlibResolver {
    /// Determines the stdlib root directory.
    ///
    /// If an override path is provided, it is used directly.
    /// Otherwise, the stdlib is resolved relative to the CLI binary location.
    pub fn resolve(override_path: Option<&Path>) -> CliResult<StdlibPaths> {
        let stdlib_root = match override_path {
            Some(path) => path.to_path_buf(),
            None => Self::default_stdlib_path()?,
        };

        let wasome = stdlib_root.join("wasome");
        let archive = stdlib_root.join("rust").join(STDLIB_ARCHIVE_NAME);

        // Validate that the expected structure exists by parsing the manifest
        let manifest_path = wasome.join(crate::manifest::MANIFEST_FILE);
        let content = std::fs::read_to_string(&manifest_path)
            .map_err(|_| CliError::StdlibNotFound(wasome.clone()))?;
        crate::manifest::Manifest::parse(&content)
            .map_err(|_| CliError::StdlibNotFound(wasome.clone()))?;
        if !archive.exists() {
            return Err(CliError::StdlibNotFound(archive));
        }

        Ok(StdlibPaths { wasome, archive })
    }

    /// Reads the pre-compiled Rust stdlib archive into a `LinkableFile`.
    pub fn load_archive(paths: &StdlibPaths) -> CliResult<LinkableFile> {
        LinkableFile::from_path(&paths.archive)
            .map_err(|e| CliError::LinkFileError(paths.archive.clone(), e))
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
