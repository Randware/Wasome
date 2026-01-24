use bon::Builder;
use source::types::FileID;
use std::{io, ops::Range};

use crate::{
    renderer::Renderer,
    source::{NoSource, SourceLookup},
};

/// Defines the severity level of a [`Diagnostic`], affecting styling and output behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    /// Indicates issues that prevent code generation.
    Error,
    /// Indicates anti-patterns, potential errors, or deprecated code.
    Warning,
    /// Indicates informational messages that can be safely ignored, such as compilation status.
    Info,
}

/// Represents a compiler diagnostic.
///
/// Constructed using the builder pattern via [`Diagnostic::builder()`].
#[derive(Builder, Debug, Clone)]
pub struct Diagnostic {
    /// The list of code snippets attached to this diagnostic.
    #[builder(field)]
    pub(crate) snippets: Vec<Snippet>,

    /// The severity level of the diagnostic. Defaults to [`Level::Error`].
    #[builder(default = Level::Error)]
    pub(crate) level: Level,

    /// The primary error message displayed in the header.
    #[builder(into)]
    pub(crate) message: String,

    /// An optional unique identifier code for this error case.
    #[builder(into)]
    pub(crate) code: Option<String>,

    /// An optional help message to guide the user towards a solution.
    #[builder(into)]
    pub(crate) help: Option<String>,
}

impl<S: diagnostic_builder::State> DiagnosticBuilder<S> {
    /// Adds a [`Snippet`] to this diagnostic.
    ///
    /// Snippets are rendered in the order they are added.
    pub fn snippet(mut self, snippet: Snippet) -> Self {
        self.snippets.push(snippet);
        self
    }
}

impl Diagnostic {
    /// Renders the diagnostic to `stdout` or `stderr` without source code snippets.
    ///
    /// Any attached snippets will be ignored.
    pub fn print(&self) -> io::Result<()> {
        self.print_snippets(&NoSource)
    }

    /// Renders the diagnostic to `stdout` or `stderr`, including source code snippets.
    ///
    /// Requires a [`SourceLookup`] implementation to retrieve file content. If source lookup fails
    /// for a specific file, that snippet will be omitted.
    pub fn print_snippets(&self, source: &impl SourceLookup) -> io::Result<()> {
        Renderer::render(self, source)
    }
}

/// Represents a source code snippet attached to a [`Diagnostic`].
///
/// Constructed using the builder pattern via [`Snippet::builder()`].
#[derive(Builder, Debug, Clone)]
pub struct Snippet {
    /// The list of annotations within this snippet.
    #[builder(field)]
    pub(crate) annotations: Vec<Annotation>,

    /// The [`FileID`] of the source file containing this snippet.
    pub(crate) file: FileID,
}

/// Internal representation of a snippet annotation.
///
/// This struct cannot be constructed directly; use the helper methods on [`SnippetBuilder`] instead.
#[derive(Debug, Clone)]
pub(crate) struct Annotation {
    pub(crate) range: Range<usize>,
    pub(crate) message: String,
    pub(crate) primary: bool,
}

impl<S: snippet_builder::State> SnippetBuilder<S> {
    /// Adds a primary annotation to the snippet.
    ///
    /// Primary annotations highlight the code directly responsible for the error using a unique color.
    /// Most diagnostics should have exactly one primary annotation, though multiple are supported.
    ///
    /// **Note:** At least one annotation is required to render the snippet.
    pub fn primary(mut self, range: Range<usize>, message: impl Into<String>) -> Self {
        self.annotations.push(Annotation {
            range,
            message: message.into(),
            primary: true,
        });
        self
    }

    /// Adds a context annotation to the snippet.
    ///
    /// Context annotations provide additional background information or trace the circumstances
    /// leading to the error. They should be used in conjunction with a primary annotation.
    ///
    /// **Note:** At least one annotation is required to render the snippet.
    pub fn context(mut self, range: Range<usize>, message: impl Into<String>) -> Self {
        self.annotations.push(Annotation {
            range,
            message: message.into(),
            primary: false,
        });
        self
    }
}
