use std::io;

use error::diagnostic::{Diagnostic, Level, Snippet};
use source::{SourceMap, types::FileID};
use thiserror::Error;

use crate::manifest;

#[derive(Debug, Error)]
pub enum CliError {
    #[error(transparent)]
    Manifest(ManifestError),

    /// This error is used to provide the caller with an instance of a SourceMap and the project
    /// manifests FileID, so we can display snippets
    #[error("Failed to parse manifest")]
    ManifestParse(toml::de::Error, SourceMap, FileID),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Could not compile project")]
    CompilationFailed,
}

impl CliError {
    /// Renders this error as a diagnostic to stderr.
    pub fn print(self) -> io::Result<()> {
        match self {
            CliError::ManifestParse(toml_err, source, file_id) => {
                let snippet = if let Some(range) = toml_err.span() {
                    Snippet::builder()
                        .file(file_id)
                        .primary(range, toml_err.message())
                        .build()
                } else {
                    Snippet::builder().file(file_id).build()
                };

                Diagnostic::builder()
                    .level(Level::Error)
                    .message("Malformed project manifest")
                    .snippet(snippet)
                    .build()
                    .print_snippets(&source)?;
            }

            CliError::Manifest(ref err) => {
                let builder = Diagnostic::builder()
                    .level(Level::Error)
                    .message(err.to_string());

                match err {
                    ManifestError::NotFound => {
                        builder
                            .help("Try initializing a project first")
                            .build()
                            .print()?;
                    }
                    ManifestError::AlreadyFound => {
                        builder
                            .help("Try initializing your project somewhere else")
                            .build()
                            .print()?;
                    }
                    ManifestError::NoEntry(_) => {
                        builder
                            .help(format!(
                                "Make sure your project has a valid '{}' or '{}' file",
                                manifest::BINARY_ENTRY_FILE,
                                manifest::LIBRARY_ENTRY_FILE
                            ))
                            .build()
                            .print()?;
                    }
                    ManifestError::MultipleEntries(_) => {
                        builder
                            .help(format!(
                                "Make sure your project only has either a '{}' or '{}' file",
                                manifest::BINARY_ENTRY_FILE,
                                manifest::LIBRARY_ENTRY_FILE
                            ))
                            .build()
                            .print()?;
                    }
                    ManifestError::MissingDependency(_, _) => {
                        builder
                            .help("Make sure you have a local or global version of the dependency available and its manifest is valid")
                            .build()
                            .print()?;
                    }
                    ManifestError::Parse(_, _) => {
                        panic!("ManifestError::Parse should never be printed standalone")
                    }
                    _ => {
                        builder.build().print()?;
                    }
                }
            }

            CliError::Io(err) => {
                Diagnostic::builder()
                    .level(Level::Error)
                    .message(err.to_string())
                    .build()
                    .print()?;
            }

            CliError::CompilationFailed => {}
        }

        Ok(())
    }
}

pub type CliResult<T> = Result<T, CliError>;

#[derive(Debug, Error)]
pub enum ManifestError {
    #[error("Could not find a project manifest")]
    NotFound,

    #[error("Provided path is already root of another project")]
    AlreadyFound,

    #[error("Failed to read manifest file: {0}")]
    Io(#[from] std::io::Error),

    #[error("Failed to parse manifest: {0}")]
    Parse(toml::de::Error, FileID),

    #[error("Invalid or no entry file for project '{0}'")]
    NoEntry(String),

    #[error("Multiple entries for project '{0}'")]
    MultipleEntries(String),

    #[error("Cannot check library project")]
    LibraryCheckUnsupported,

    #[error("Dependency '{0}' (required by '{1}') could not be resolved")]
    MissingDependency(String, String),
}

pub type ManifestResult<T> = Result<T, ManifestError>;
