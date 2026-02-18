use error::diagnostic::{Diagnostic, Level, Snippet};
use source::types::Span;

/// Represents all errors that can occur during semantic analysis.
#[derive(Debug, Clone)]
pub enum SemanticError {
    /// E3001: A type could not be resolved.
    UnknownType { name: String, span: Span },

    /// E3002: A variable or function does not exist in the current scope.
    UnknownSymbol { name: String, span: Span },

    /// E3003: A type mismatch occurred.
    TypeMismatch { expected: String, found: String, span: Span },

    /// E3004: A symbol (variable, function, enum, struct) is already declared.
    AlreadyDeclared { name: String, kind: String, span: Span },

    /// E3005: A function is missing a return value on some paths.
    MissingReturn { func_name: String, span: Span },

    /// E3006: Arguments do not match the function/method signature.
    ArgumentMismatch { expected: usize, found: usize, span: Span },

    /// E3007: A condition (if/while) must evaluate to a boolean.
    ConditionNotBoolean { span: Span },

    /// E3008: A break statement was used outside of a loop.
    BreakOutsideLoop { span: Span },

    /// E3009: Invalid usage of a symbol (e.g., calling a struct like a function).
    InvalidUsage { message: String, span: Span },

    /// E3999: An internal compiler error during semantic analysis.
    Internal { message: String, span: Span },

    /// E3000: Generic fallback for errors that haven't been categorized yet.
    Custom { message: String, span: Span },
}

impl SemanticError {
    /// Converts the internal semantic error into a user-facing `Diagnostic`.
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            SemanticError::UnknownType { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3001".to_string())
                .message(format!("Unknown type '{}'", name))
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "Type not found in this scope").build())
                .build(),

            SemanticError::UnknownSymbol { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3002".to_string())
                .message(format!("Cannot find symbol '{}' in this scope", name))
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "Symbol not found").build())
                .build(),

            SemanticError::TypeMismatch { expected, found, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3003".to_string())
                .message(format!("Type mismatch: expected '{}', found '{}'", expected, found))
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, format!("Expected {}, found {}", expected, found)).build())
                .build(),

            SemanticError::AlreadyDeclared { name, kind, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3004".to_string())
                .message(format!("{} '{}' is already defined", kind, name))
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, format!("Name '{}' is already in use", name)).build())
                .build(),

            SemanticError::MissingReturn { func_name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3005".to_string())
                .message(format!("Function '{}' must return a value", func_name))
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "Not all control paths return a value").build())
                .build(),

            SemanticError::ArgumentMismatch { expected, found, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3006".to_string())
                .message(format!("Argument mismatch: expected {} arguments, found {}", expected, found))
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "Incorrect number of arguments").build())
                .build(),

            SemanticError::ConditionNotBoolean { span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3007".to_string())
                .message("Condition must evaluate to a boolean".to_string())
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "Expected a boolean expression").build())
                .build(),

            SemanticError::BreakOutsideLoop { span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3008".to_string())
                .message("Break statement used outside of a loop".to_string())
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "Cannot break here").build())
                .build(),

            SemanticError::InvalidUsage { message, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3009".to_string())
                .message(message.clone())
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "Invalid usage").build())
                .build(),

            SemanticError::Internal { message, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3999".to_string())
                .message(format!("Internal Compiler Error: {}", message))
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, "The compiler encountered an unexpected state").build())
                .build(),

            SemanticError::Custom { message, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3000".to_string())
                .message(message.clone())
                .snippet(Snippet::builder().file(span.file_id).primary(span.start..span.end, message.clone()).build())
                .build(),
        }
    }
}