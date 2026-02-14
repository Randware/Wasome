use error::diagnostic::{Diagnostic, Level};
use source::types::Span;

/// Represents all errors that can occur during semantic analysis.
#[derive(Debug, Clone)]
pub enum SemanticError {
    /// A type could not be resolved (e.g., used in a signature but not defined).
    UnknownType { name: String, span: Span },

    /// A variable or function was referenced but does not exist in the current scope.
    UnknownSymbol { name: String, span: Span },

    /// A type mismatch occurred (e.g., expected `s32`, found `bool`).
    TypeMismatch { expected: String, found: String, span: Span },

    /// A generic fallback error during the refactoring process.
    /// This will be replaced by specific error variants as we progress.
    Custom { message: String, span: Span },
}

impl SemanticError {
    /// Converts the internal semantic error into a user-facing `Diagnostic`.
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            SemanticError::UnknownType { name, span: _ } => {
                Diagnostic::builder()
                    .level(Level::Error)
                    .message(format!("Unknown type '{}'", name))
                    // TODO: Add snippet pointing to the span once we integrate SnippetBuilder
                    .build()
            }
            SemanticError::UnknownSymbol { name, span: _ } => {
                Diagnostic::builder()
                    .level(Level::Error)
                    .message(format!("Cannot find symbol '{}' in this scope", name))
                    .build()
            }
            SemanticError::TypeMismatch { expected, found, span: _ } => {
                Diagnostic::builder()
                    .level(Level::Error)
                    .message(format!("Type mismatch: expected '{}', found '{}'", expected, found))
                    .build()
            }
            SemanticError::Custom { message, span: _ } => {
                Diagnostic::builder()
                    .level(Level::Error)
                    .message(message.clone())
                    .build()
            }
        }
    }
}