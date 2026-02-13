use thiserror::Error;

#[derive(Debug, Error)]
pub enum CliError {
    #[error(transparent)]
    Manifest(#[from] ManifestError),

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
