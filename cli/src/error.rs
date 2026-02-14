use source::{SourceMap, types::FileID};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CliError {
    #[error(transparent)]
    Manifest(#[from] ManifestError),

    /// This error is used to provide the caller with an instance of a SourceMap and the project
    /// manifests FileID, so we can display snippets
    #[error("Failed to parse manifest")]
    ManifestParse(toml::de::Error, SourceMap, FileID),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

pub type CliResult<T> = Result<T, CliError>;

#[derive(Debug, Error)]
pub enum ManifestError {
    #[error("Could not find a project manifest")]
    NotFound,

    #[error("Provided path already inside other project")]
    AlreadyFound,

    #[error("Failed to read manifest file: {0}")]
    Io(#[from] std::io::Error),

    #[error("Failed to parse manifest: {0}")]
    Parse(#[from] toml::de::Error),
}

pub type ManifestResult<T> = Result<T, ManifestError>;
