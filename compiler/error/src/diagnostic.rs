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
    pub snippets: Vec<Snippet>,

    #[builder(default = Level::Error)]
    pub level: Level,

    #[builder(into)]
    pub message: String,

    #[builder(into)]
    pub code: Option<String>,

    #[builder(into)]
    pub help: Option<String>,
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
    pub annotations: Vec<Annotation>,

    pub file: FileID,
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub ranges: Vec<Range<usize>>,
    pub message: String,
}

impl<S: snippet_builder::State> SnippetBuilder<S> {
    /// Adds a single annotation to the snippet.
    pub fn annotate(self, range: Range<usize>, message: impl Into<String>) -> Self {
        self.annotate_many(std::iter::once(range), message)
    }

    /// Adds multiple locations for the same message.
    pub fn annotate_many<I>(mut self, ranges: I, message: impl Into<String>) -> Self
    where
        I: IntoIterator<Item = Range<usize>>,
    {
        let ranges: Vec<_> = ranges.into_iter().collect();

        if !ranges.is_empty() {
            self.annotations.push(Annotation {
                ranges,
                message: message.into(),
            });
        }
        self
    }
}
