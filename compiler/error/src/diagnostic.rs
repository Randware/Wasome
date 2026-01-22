use bon::Builder;
use source::types::FileID;
use std::{io, ops::Range};

use crate::{
    renderer::Renderer,
    source::{NoSource, SourceLookup},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Info,
}

#[derive(Builder, Debug, Clone)]
pub struct Diagnostic {
    #[builder(field)]
    pub(crate) snippets: Vec<Snippet>,

    #[builder(default = Level::Error)]
    pub(crate) level: Level,

    #[builder(into)]
    pub(crate) message: String,

    #[builder(into)]
    pub(crate) code: Option<String>,

    #[builder(into)]
    pub(crate) help: Option<String>,
}

impl<S: diagnostic_builder::State> DiagnosticBuilder<S> {
    pub fn snippet(mut self, snippet: Snippet) -> Self {
        self.snippets.push(snippet);
        self
    }
}

impl Diagnostic {
    /// Renders the diagnostic without any code snippets to stdout/stderr
    pub fn print(&self) -> io::Result<()> {
        self.print_snippets(&NoSource)
    }

    /// Renders the diagnostic, including source code snippets to stdout/stderr
    pub fn print_snippets(&self, source: &impl SourceLookup) -> io::Result<()> {
        Renderer::render(self, source)
    }
}

#[derive(Builder, Debug, Clone)]
pub struct Snippet {
    #[builder(field)]
    pub(crate) primary: Annotation,

    #[builder(field)]
    pub(crate) context: Vec<Annotation>,

    pub(crate) file: FileID,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Annotation {
    pub range: Range<usize>,
    pub message: String,
}

impl Annotation {
    fn new(range: Range<usize>, message: impl Into<String>) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }
}

impl<S: snippet_builder::State> SnippetBuilder<S> {
    /// Adds a primary annotation to the snippet.
    pub fn primary(mut self, range: Range<usize>, message: impl Into<String>) -> Self {
        self.primary = Annotation::new(range, message);
        self
    }

    /// Adds a context annotation to the snippet.
    pub fn context(mut self, range: Range<usize>, message: impl Into<String>) -> Self {
        self.context.push(Annotation::new(range, message));
        self
    }
}
