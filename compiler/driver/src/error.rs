use error::diagnostic::{Diagnostic, Snippet};
use source::types::Span;
use std::io;
use std::path::PathBuf;

/// Error code constants for driver diagnostics.
///
/// These codes follow the format `E####` where the first digit indicates the category:
/// - `E4xxx`: Driver-level errors (program loading, file system operations)
const INVALID_CHARS_IN_MAIN_FILE: &str = "E4001";
const MAIN_FILE_PROJECT_NOT_FOUND: &str = "E4002";
const MAIN_FILE_PATH_EMPTY: &str = "E4003";
const IO_ERROR: &str = "E4004";
const UNRESOLVED_IMPORT_ERROR: &str = "E4005";

/// Custom error enum for driver-level errors.
///
/// This enum represents all error conditions that can occur during the driver's execution,
/// including program loading, file system operations, and parsing. It provides a unified
/// error type that can be converted to [`Diagnostic`] for display to the user.
///
/// # Variants
///
/// ## Main File Errors
///
/// These errors occur during the initial program validation phase when determining the
/// entry point of the program.
///
/// - [`MainFileNonUtf8Chars`](Self::MainFileNonUtf8Chars) - The main file path contains
///   invalid UTF-8 characters.
/// - [`MainFileProjectNotFound`](Self::MainFileProjectNotFound) - The project containing
///   the main file does not exist in the program configuration.
/// - [`MainFilePathEmpty`](Self::MainFilePathEmpty) - The main file path is empty or
///   does not specify a file.
///
/// ## File System Errors
///
/// These errors occur when the driver attempts to read files or directories from the
/// file system.
///
/// - [`UnableToLoadFile`](Self::UnableToLoadFile) - Failed to read a source file.
///   Contains the file path and the underlying I/O error.
/// - [`UnableToLoadDirectory`](Self::UnableToLoadDirectory) - Failed to list files in
///   a directory. Contains the directory path and the underlying I/O error. Should the directory
///   not exist on disk, [`UnresolvedImport`](Self::UnresolvedImport) will be emitted instead.
///
/// ## Import Errors
///
/// These errors occur when the driver cannot resolve an import statement.
///
/// - [`UnresolvedImport`](Self::UnresolvedImport) - An import statement references a
///   module that cannot be found. Contains the span of the import statement for
///   error reporting.
///
/// ## Parsing Errors
///
/// These errors occur when the source code contains syntax errors.
///
/// - [`SyntaxError`](Self::SyntaxError) - The parser encountered invalid syntax.
///   Contains the full diagnostic from the parser.
///
/// # Conversion to Diagnostic
///
/// This enum implements `Into<Diagnostic>` (via `From<DriverError>`) to convert
/// driver errors into user-facing diagnostics. Each variant is converted to an
/// appropriate diagnostic with:
///
/// - A descriptive error message
/// - An error code for reference
/// - A help message (where applicable)
/// - Source snippets (for [`UnresolvedImport`])
///
#[derive(Debug)]
pub(super) enum DriverError {
    /// The main file path contains non-UTF-8 characters.
    ///
    /// This error occurs when the main file path cannot be converted to a valid
    /// UTF-8 string. This is required for consistent cross-platform path handling.
    MainFileNonUtf8Chars,

    /// The project containing the main file was not found.
    ///
    /// This error occurs when the main file is specified as belonging to a project
    /// that is not included in the program configuration.
    MainFileProjectNotFound,

    /// The main file path does not specify a file.
    ///
    /// This error occurs when the main file configuration is empty or contains
    /// only directory components without a filename.
    MainFilePathEmpty,

    /// An Io error
    ///
    /// This error occurs when an io error occurs during driver operation. Common causes
    /// include:
    ///
    /// - A file does not exist
    /// - Insufficient permissions to read a file
    /// - A file is locked by another process
    ///
    /// # Fields
    ///
    /// - `source`: The underlying I/O error that caused the failure.
    Io {
        /// The underlying I/O error.
        source: io::Error,
    },

    /// An import statement references a module that cannot be found.
    ///
    /// This error occurs when the driver cannot resolve an import statement to
    /// a valid module. This can happen when:
    ///
    /// - The imported module does not exist in the project structure
    /// - The import path is malformed
    /// - The module file is not a valid Wasome source file
    ///
    /// # Fields
    ///
    /// - `span`: The location of the import statement in the source file.
    ///   This is used to provide precise error reporting with source snippets.
    UnresolvedImport {
        /// The location of the import statement.
        span: Span,
    },

    /// The parser encountered invalid syntax.
    ///
    /// This error occurs when the source code contains syntax errors that prevent
    /// parsing. The full diagnostic from the parser is preserved, including all
    /// error locations, help messages, and source snippets.
    ///
    /// # Fields
    ///
    /// - `diagnostic`: The complete diagnostic from the parser.
    SyntaxError {
        /// The diagnostic from the parser.
        diagnostic: Diagnostic,
    },
}

impl From<Diagnostic> for DriverError {
    /// Converts a [`Diagnostic`] into a [`DriverError::SyntaxError`].
    ///
    /// This conversion is used when propagating parser errors through the driver.
    /// The diagnostic is wrapped without modification, preserving all error
    /// information including snippets, help messages, and multiple error locations.
    ///
    /// # Parameters
    ///
    /// - `diagnostic`: The diagnostic from the parser or another component.
    ///
    /// # Returns
    ///
    /// A `DriverError::SyntaxError` containing the original diagnostic.
    fn from(diagnostic: Diagnostic) -> Self {
        Self::SyntaxError { diagnostic }
    }
}

impl From<DriverError> for Diagnostic {
    /// Converts a [`DriverError`] into a user-facing [`Diagnostic`].
    ///
    /// This conversion creates a diagnostic with an appropriate error message,
    /// error code, and help text for each error variant. Some variants also
    /// include source snippets for precise error location.
    ///
    /// # Variants
    ///
    /// - `MainFileNonUtf8Chars`: Creates a diagnostic with error code `E4001`.
    /// - `MainFileProjectNotFound`: Creates a diagnostic with error code `E4002`.
    /// - `MainFilePathEmpty`: Creates a diagnostic with error code `E4003`.
    /// - `UnableToLoadFile`: Creates a diagnostic with error code `E4004` and
    ///   includes the file path and I/O error details in the message.
    /// - `UnableToLoadDirectory`: Creates a diagnostic with error code `E4005` and
    ///   includes the directory path and I/O error details in the message.
    /// - `UnresolvedImport`: Creates a diagnostic with error code `E4006` and
    ///   includes a primary annotation at the import statement location.
    /// - `SyntaxError`: Returns the wrapped diagnostic unchanged.
    fn from(val: DriverError) -> Self {
        match val {
            DriverError::MainFileNonUtf8Chars => Self::builder()
                .message("The main file path may not contain non-UTF8 chars")
                .code(INVALID_CHARS_IN_MAIN_FILE)
                .help("Only use valid UTF-8 characters")
                .build(),
            DriverError::MainFileProjectNotFound => Self::builder()
                .message("The project of the main file could not be found")
                .code(MAIN_FILE_PROJECT_NOT_FOUND)
                .help("Provide a valid main file path")
                .build(),
            DriverError::MainFilePathEmpty => Self::builder()
                .message("The path of the main file is empty")
                .code(MAIN_FILE_PATH_EMPTY)
                .help("Provide a valid main file path")
                .build(),
            DriverError::Io { source } => Self::builder()
                .message(format!(
                    "IO Error: {}",
                    source
                ))
                .code(IO_ERROR)
                .build(),
            DriverError::UnresolvedImport { span } => Self::builder()
                .message("Unable to resolve import")
                .code(UNRESOLVED_IMPORT_ERROR)
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Unable to find the referenced file")
                        .build(),
                )
                .build(),
            DriverError::SyntaxError { diagnostic } => diagnostic,
        }
    }
}
