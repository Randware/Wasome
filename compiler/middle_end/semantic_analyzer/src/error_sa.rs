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
    Custom { message: String, span: Span },
}

impl SemanticError {
    /// Gibt die Position des Fehlers zurück.
    pub fn span(&self) -> Span {
        match self {
            SemanticError::UnknownType { span, .. } => *span,
            SemanticError::UnknownSymbol { span, .. } => *span,
            SemanticError::TypeMismatch { span, .. } => *span,
            SemanticError::Custom { span, .. } => *span,
        }
    }

    /// Gibt die Fehlermeldung zurück.
    pub fn message(&self) -> String {
        match self {
            SemanticError::UnknownType { name, .. } => format!("Unknown type '{}'", name),
            SemanticError::UnknownSymbol { name, .. } => format!("Cannot find symbol '{}' in this scope", name),
            SemanticError::TypeMismatch { expected, found, .. } => {
                format!("Type mismatch: expected '{}', found '{}'", expected, found)
            }
            SemanticError::Custom { message, .. } => message.clone(),
        }
    }

    /// Wandelt den internen semantischen Fehler in ein benutzerfreundliches `Diagnostic` um.
    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::builder()
            .level(Level::Error)
            .message(self.message())
            .build()
    }
}