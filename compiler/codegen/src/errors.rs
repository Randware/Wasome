use source::types::Span;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum CodegenError<'src> {
    #[error("Unknown variable '{0}'")]
    UnknownVariable(&'src str, Span),

    #[error("Unknown function '{0}'")]
    UnknownFunction(&'src str, Span),

    #[error("Main function not found")]
    MissingMainFunction,

    #[error("Entry block definition failed")]
    BlockCreationFailure,

    #[error("Duplicate project name detected: '{0}'")]
    DuplicateProjectName(String),

    #[error("Feature '{0}' is not yet implemented in backend")]
    Unimplemented(&'static str, Span),

    #[error("Internal Compiler Error: {0}")]
    /// *Internal Compiler Error*
    Ice(String),
}
