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
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    /// E3004: A symbol (variable, function, enum, struct) is already declared.
    AlreadyDeclared {
        name: String,
        kind: String,
        span: Span,
    },

    /// E3005: A function is missing a return value on some paths.
    MissingReturn { func_name: String, span: Span },

    /// E3006: Arguments do not match the function/method signature.
    ArgumentMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },

    /// E3007: A condition (if/while) must evaluate to a boolean.
    ConditionNotBoolean { span: Span },

    /// E3008: A break statement was used outside of a loop.
    BreakOutsideLoop { span: Span },

    /// E3009: Invalid usage of a symbol (e.g., calling a struct like a function).
    InvalidUsage { message: String, span: Span },

    /// E3010: Incorrect number of type parameters for generics.
    GenericArgumentCountMismatch {
        name: String,
        expected: usize,
        found: usize,
        span: Span,
    },

    /// E3010: Accessing a private field from outside its struct
    PrivateFieldAccess {
        field: String,
        struct_name: String,
        span: Span,
    },

    /// E3011: Missing or incorrectly named field during struct initialization.
    MissingOrInvalidStructField {
        struct_name: String,
        field_name: String,
        span: Span,
    },

    /// E3012: An enum variant was initialized with the wrong payload types.
    VariantPayloadMismatch {
        variant_name: String,
        expected_types: String,
        found_types: String,
        span: Span,
    },

    /// E3013: Access to a private symbol from outside its module.
    PrivateSymbolAccess { name: String, span: Span },

    /// E3014: Methods can only be called on structs.
    MethodOnNonStruct { type_name: String, span: Span },

    /// E3015: The symbol exists but is of the wrong kind (e.g., function instead of struct).
    SymbolKindMismatch {
        name: String,
        expected: String,
        span: Span,
    },

    /// E3016: Access to a field that does not exist in the struct.
    UnknownField {
        struct_name: String,
        field_name: String,
        span: Span,
    },

    /// E3017: Attempt to access a field on a data type that is not a struct.
    FieldAccessOnNonStruct { type_name: String, span: Span },

    /// E3018: A void function cannot return a value.
    VoidReturnsValue { span: Span },

    /// E3019: An empty return statement in a function that expects a value.
    MissingReturnValue { expected_type: String, span: Span },

    /// E3020: Primitive data types cannot have type parameters (generics).
    PrimitiveWithTypeParameters { type_name: String, span: Span },

    /// E3021: An operator is applied to types that do not support it (e.g., bool + s32).
    UnsupportedBinaryOperation {
        op: String,
        left_type: String,
        right_type: String,
        span: Span,
    },

    /// E3022: A unary operator is applied to an invalid type (e.g., -bool).
    UnsupportedUnaryOperation {
        op: String,
        target_type: String,
        span: Span,
    },

    /// E3023: A literal is malformed or out of bounds for its data type.
    InvalidLiteralFormat { value: String, span: Span },

    /// E3024: The enum exists, but the requested variant (e.g., ::Some) is missing.
    UnknownEnumVariant {
        enum_name: String,
        variant_name: String,
        span: Span,
    },

    /// E3025: A variable is defined twice in the exact same local scope (shadowing violation).
    LocalVariableShadowing { name: String, span: Span },

    /// E3026: A void function or method is used as a value in an expression.
    VoidUsedAsValue { name: String, span: Span },

    /// E3027: The provided parameters in the struct initialization do not match the defined fields.
    StructInitializationMismatch { struct_name: String, span: Span },

    /// E3999: An internal compiler error during semantic analysis.
    Internal { message: String, span: Span },

    /// E3000: Generic fallback for errors that haven't been categorized yet.
    Custom { message: String, span: Span },

    /// E3028: A type parameter is declared more than once in the same generic declaration.
    DuplicateTypeParameter { name: String, span: Span },

    /// E3029: A drop method has an invalid signature (e.g., has parameters, generics, or a return type).
    InvalidDropSignature { message: String, span: Span },
}

impl SemanticError {
    /// Converts the internal semantic error into a user-facing `Diagnostic`.
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            SemanticError::UnknownType { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3001".to_string())
                .message(format!("Unknown type '{}'", name))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Type not found in this scope")
                        .build(),
                )
                .build(),

            SemanticError::UnknownSymbol { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3002".to_string())
                .message(format!("Cannot find symbol '{}' in this scope", name))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Symbol not found")
                        .build(),
                )
                .build(),

            SemanticError::TypeMismatch {
                expected,
                found,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3003".to_string())
                .message(format!(
                    "Type mismatch: expected '{}', found '{}'",
                    expected, found
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            format!("Expected {}, found {}", expected, found),
                        )
                        .build(),
                )
                .build(),

            SemanticError::AlreadyDeclared { name, kind, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3004".to_string())
                .message(format!("{} '{}' is already defined", kind, name))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            format!("Name '{}' is already in use", name),
                        )
                        .build(),
                )
                .build(),

            SemanticError::MissingReturn { func_name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3005".to_string())
                .message(format!("Function '{}' must return a value", func_name))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Not all control paths return a value")
                        .build(),
                )
                .build(),

            SemanticError::ArgumentMismatch {
                expected,
                found,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3006".to_string())
                .message(format!(
                    "Argument mismatch: expected {} arguments, found {}",
                    expected, found
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Incorrect number of arguments")
                        .build(),
                )
                .build(),

            SemanticError::ConditionNotBoolean { span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3007".to_string())
                .message("Condition must evaluate to a boolean".to_string())
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Expected a boolean expression")
                        .build(),
                )
                .build(),

            SemanticError::BreakOutsideLoop { span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3008".to_string())
                .message("Break statement used outside of a loop".to_string())
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Cannot break here")
                        .build(),
                )
                .build(),

            SemanticError::InvalidUsage { message, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3009".to_string())
                .message(message.clone())
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Invalid usage")
                        .build(),
                )
                .build(),

            SemanticError::GenericArgumentCountMismatch {
                name,
                expected,
                found,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3010".to_string())
                .message(format!("Generic argument count mismatch for '{}'", name))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            format!("Expected {}, but found {}", expected, found),
                        )
                        .build(),
                )
                .build(),

            SemanticError::PrivateFieldAccess {
                field,
                struct_name,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3010".to_string())
                .message(format!(
                    "Cannot access private field '{}' of struct '{}'",
                    field, struct_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            format!("Field '{}' is private", field),
                        )
                        .build(),
                )
                .build(),

            SemanticError::MissingOrInvalidStructField {
                struct_name,
                field_name,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3011".to_string())
                .message(format!(
                    "Invalid or missing field '{}' in initialization of struct '{}'",
                    field_name, struct_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            "Check struct definition for correct fields",
                        )
                        .build(),
                )
                .build(),

            SemanticError::VariantPayloadMismatch {
                variant_name,
                expected_types,
                found_types,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3012".to_string())
                .message(format!(
                    "Invalid payload for enum variant '{}'",
                    variant_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            format!("Expected '{}', found '{}'", expected_types, found_types),
                        )
                        .build(),
                )
                .build(),

            SemanticError::PrivateSymbolAccess { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3013".to_string())
                .message(format!("Symbol '{}' is private", name))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            "Cannot access private symbol from this scope",
                        )
                        .build(),
                )
                .build(),

            SemanticError::MethodOnNonStruct { type_name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3014".to_string())
                .message(format!(
                    "Cannot call methods on non-struct type '{}'",
                    type_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Method call is invalid here")
                        .build(),
                )
                .build(),

            SemanticError::SymbolKindMismatch {
                name,
                expected,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3015".to_string())
                .message(format!("'{}' is not a {}", name, expected))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            format!("Expected a {}, but found something else", expected),
                        )
                        .build(),
                )
                .build(),

            SemanticError::UnknownField {
                struct_name,
                field_name,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3016".to_string())
                .message(format!(
                    "Struct '{}' has no field named '{}'",
                    struct_name, field_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Unknown field")
                        .build(),
                )
                .build(),

            SemanticError::FieldAccessOnNonStruct { type_name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3017".to_string())
                .message(format!(
                    "Cannot access fields on non-struct type '{}'",
                    type_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Field access is invalid here")
                        .build(),
                )
                .build(),

            SemanticError::VoidReturnsValue { span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3018".to_string())
                .message("Void functions cannot return a value".to_string())
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Unexpected return value")
                        .build(),
                )
                .build(),

            SemanticError::MissingReturnValue {
                expected_type,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3019".to_string())
                .message(format!("Expected return value of type '{}'", expected_type))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Provide a value to return")
                        .build(),
                )
                .build(),

            SemanticError::PrimitiveWithTypeParameters { type_name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3020".to_string())
                .message(format!(
                    "Primitive type '{}' cannot have type parameters",
                    type_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Remove the type parameters")
                        .build(),
                )
                .build(),

            SemanticError::UnsupportedBinaryOperation {
                op,
                left_type,
                right_type,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3021".to_string())
                .message(format!(
                    "Cannot apply operator '{}' to types '{}' and '{}'",
                    op, left_type, right_type
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Unsupported types for this operator")
                        .build(),
                )
                .build(),

            SemanticError::UnsupportedUnaryOperation {
                op,
                target_type,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3022".to_string())
                .message(format!(
                    "Cannot apply unary operator '{}' to type '{}'",
                    op, target_type
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Unsupported type for this operator")
                        .build(),
                )
                .build(),

            SemanticError::InvalidLiteralFormat { value, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3023".to_string())
                .message(format!("Literal '{}' is invalid or out of bounds", value))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Check the format and size limits")
                        .build(),
                )
                .build(),

            SemanticError::UnknownEnumVariant {
                enum_name,
                variant_name,
                span,
            } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3024".to_string())
                .message(format!(
                    "Enum '{}' has no variant named '{}'",
                    enum_name, variant_name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Variant not found")
                        .build(),
                )
                .build(),

            SemanticError::LocalVariableShadowing { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3025".to_string())
                .message(format!("Variable '{}' shadows an existing variable", name))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            "Variable is already defined in this local scope",
                        )
                        .build(),
                )
                .build(),

            SemanticError::VoidUsedAsValue { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3026".to_string())
                .message(format!(
                    "Function or method '{}' does not return a value",
                    name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Cannot use void result as a value")
                        .build(),
                )
                .build(),

            SemanticError::StructInitializationMismatch { struct_name, span } => {
                Diagnostic::builder()
                    .level(Level::Error)
                    .code("E3027".to_string())
                    .message(format!(
                        "Initialization parameters do not match fields of struct '{}'",
                        struct_name
                    ))
                    .snippet(
                        Snippet::builder()
                            .file(span.file_id)
                            .primary(span.start..span.end, "Invalid struct initialization")
                            .build(),
                    )
                    .build()
            }

            SemanticError::DuplicateTypeParameter { name, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3028".to_string())
                .message(format!(
                    "Type parameter '{}' is declared more than once",
                    name
                ))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Duplicate type parameter declaration")
                        .build(),
                )
                .build(),

            SemanticError::InvalidDropSignature { message, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3029".to_string())
                .message(format!("Invalid drop method signature: {}", message))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, "Invalid drop definition")
                        .build(),
                )
                .build(),

            SemanticError::Internal { message, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3999".to_string())
                .message(format!("Internal Compiler Error: {}", message))
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(
                            span.start..span.end,
                            "The compiler encountered an unexpected state",
                        )
                        .build(),
                )
                .build(),

            SemanticError::Custom { message, span } => Diagnostic::builder()
                .level(Level::Error)
                .code("E3000".to_string())
                .message(message.clone())
                .snippet(
                    Snippet::builder()
                        .file(span.file_id)
                        .primary(span.start..span.end, message.clone())
                        .build(),
                )
                .build(),
        }
    }
}