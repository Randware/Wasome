use error::diagnostic::{Diagnostic, Snippet};
use source::types::Span;
use std::error::Error as StdError;
use std::io;
use std::path::PathBuf;

/// Error code constants
pub(super) const INVALID_CHARS_IN_MAIN_FILE: &str = "E4001";
pub(super) const MAIN_FILE_PROJECT_NOT_FOUND: &str = "E4002";
pub(super) const MAIN_FILE_PATH_EMPTY: &str = "E4003";
pub(super) const UNABLE_TO_LOAD_FILE: &str = "E4004";
pub(super) const UNABLE_TO_LOAD_DIRECTORY: &str = "E4005";
pub(super) const UNRESOLVED_IMPORT_ERROR: &str = "E4006";

/// Custom error enum for driver errors
#[derive(Debug)]
pub(super) enum DriverError {
    MainFileNonUtf8Chars,
    MainFileProjectNotFound,
    MainFilePathEmpty,
    UnableToLoadFile {
        path: PathBuf,
        source: Box<dyn StdError + Send + Sync>,
    },
    UnableToLoadDirectory {
        path: PathBuf,
        source: Box<dyn StdError + Send + Sync>,
    },
    UnresolvedImport {
        span: Span,
    },
    SyntaxError {
        diagnostic: Diagnostic,
    },
}

impl DriverError {
    pub(super) fn main_file_non_utf8_chars_error() -> Self {
        Self::MainFileNonUtf8Chars
    }

    pub(super) fn main_file_project_not_found_error() -> Self {
        Self::MainFileProjectNotFound
    }

    pub(super) fn main_file_path_empty_error() -> Self {
        Self::MainFilePathEmpty
    }

    pub(super) fn unable_to_load_file_error(path: &PathBuf, error: &io::Error) -> Self {
        Self::UnableToLoadFile {
            path: path.clone(),
            source: error.to_string().into(),
        }
    }

    pub(super) fn unable_to_load_directory_error(path: &PathBuf, error: &io::Error) -> Self {
        Self::UnableToLoadDirectory {
            path: path.clone(),
            source: error.to_string().into(),
        }
    }

    pub(super) fn unresolved_import_error(span: Span) -> Self {
        Self::UnresolvedImport { span }
    }
}

impl Into<Diagnostic> for DriverError {
    fn into(self) -> Diagnostic {
        match self {
            Self::MainFileNonUtf8Chars => Diagnostic::builder()
                .message("The main file path may not contain non-UTF8 chars")
                .code(INVALID_CHARS_IN_MAIN_FILE)
                .help("Only use valid UTF-8 characters")
                .build(),
            Self::MainFileProjectNotFound => Diagnostic::builder()
                .message("The project of the main file could not be found")
                .code(MAIN_FILE_PROJECT_NOT_FOUND)
                .help("Provide a valid main file path")
                .build(),
            Self::MainFilePathEmpty => Diagnostic::builder()
                .message("The path of the main file is empty")
                .code(MAIN_FILE_PATH_EMPTY)
                .help("Provide a valid main file path")
                .build(),
            Self::UnableToLoadFile { path, source } => Diagnostic::builder()
                .message(format!(
                    "Unable to load file {}: {}",
                    path.display(),
                    source
                ))
                .code(UNABLE_TO_LOAD_FILE)
                .build(),
            Self::UnableToLoadDirectory { path, source } => Diagnostic::builder()
                .message(format!(
                    "Unable to load directory {}: {}",
                    path.display(),
                    source
                ))
                .code(UNABLE_TO_LOAD_DIRECTORY)
                .build(),
            Self::UnresolvedImport { span } => Diagnostic::builder()
                .message("Unable to resolve import")
                .code(UNRESOLVED_IMPORT_ERROR)
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Unable to find the referenced file")
                        .build(),
                )
                .build(),
            Self::SyntaxError { diagnostic } => diagnostic,
        }
    }
}
