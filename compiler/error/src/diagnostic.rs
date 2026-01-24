use bon::Builder;
use source::types::FileID;
use std::{io, ops::Range};

use crate::{
    renderer::Renderer,
    source::{NoSource, SourceLookup},
};

/// Level is used to assign certain styling and behaviour to a Diagnostic.
/// Generally:
/// - 'Error' should be used, when the compiler runs into any issues, that make it impossible to
/// generate executable code.
/// - 'Warning' should be used, when the compiler is able to detect anti-patterns,
/// error-prone or deprecated code for example. The code compiles, but warnings should still be
/// taken into consideration in most cases.
/// - 'Info' should be used, when a message can be completely ignored by the user without any
/// consequences. Things like updates regarding the compilation status for example.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Info,
}

/// Diagnostic is used for constructing diagnostics and gradually adding information to it using
/// a builder pattern, which can be accessed using 'Diagnostic::builder()'.
#[derive(Builder, Debug, Clone)]
pub struct Diagnostic {
    /// All snippets are stored here.
    #[builder(field)]
    pub(crate) snippets: Vec<Snippet>,

    /// Set the 'Level' of the Diagnostic. If none is specified, it will default to 'Level::Error'
    #[builder(default = Level::Error)]
    pub(crate) level: Level,

    /// Set the primary error message displayed inside the header. This is required.
    #[builder(into)]
    pub(crate) message: String,

    /// Set a specific code that can be used to uniquely identify this error case. This is
    /// optional.
    #[builder(into)]
    pub(crate) code: Option<String>,

    /// Set a general help message, that can push the user towards fixing the issue. This is
    /// optional.
    #[builder(into)]
    pub(crate) help: Option<String>,
}

impl<S: diagnostic_builder::State> DiagnosticBuilder<S> {
    /// Add a 'Snippet' to this Diagnostic. Each snippet will be rendered as its own snippet block.
    ///
    /// Snippets will be rendered in the order of declaration.
    pub fn snippet(mut self, snippet: Snippet) -> Self {
        self.snippets.push(snippet);
        self
    }
}

impl Diagnostic {
    /// Renders the diagnostic without any code snippets to stdout/stderr. If there are any
    /// snippets attached, they will not be rendered.
    pub fn print(&self) -> io::Result<()> {
        self.print_snippets(&NoSource)
    }

    /// Renders the diagnostic, including source code snippets to stdout/stderr. This requires a
    /// reference to a 'SourceLookup' implementer. In case something goes wrong during source
    /// lookup, the corresponding snippet will not be rendered.
    pub fn print_snippets(&self, source: &impl SourceLookup) -> io::Result<()> {
        Renderer::render(self, source)
    }
}

/// Snippet is used for constructing and annotating code snippets. This can be done, by accessing
/// the 'Snippet::builder()' and gradually adding information
#[derive(Builder, Debug, Clone)]
pub struct Snippet {
    /// All snippet annoations are stored here. At least one annotation is required, for rendering
    /// the snippet.
    #[builder(field)]
    pub(crate) annotations: Vec<Annotation>,

    /// The 'FileID' this code snippet resides in. This can be obtained from the source map. This
    /// is required.
    pub(crate) file: FileID,
}

/// Annotation contains all information related to a code snippet annotation. It cannot be
/// constructed directly and is only used internally for easier data handling. Annotation happens
/// with simple wrapper functions instead.
#[derive(Debug, Clone)]
pub(crate) struct Annotation {
    pub(crate) range: Range<usize>,
    pub(crate) message: String,
    pub(crate) primary: bool,
}

impl<S: snippet_builder::State> SnippetBuilder<S> {
    /// Adds a primary annotation to the snippet. Primary annotations should be used sparingly,
    /// since they are meant to quickly direct the users attention to code that is directly
    /// responsible. This is done by highlighting it in a unique color. All diagnostics should
    /// usually have exactly one primary annotation in most cases, though it is possible to add
    /// more primary annotations if needed.
    ///
    /// Keep in mind, at least one annotation is required, for rendering the snippet.
    ///
    /// Annotations will be rendered in the order of declaration.
    pub fn primary(mut self, range: Range<usize>, message: impl Into<String>) -> Self {
        self.annotations.push(Annotation {
            range,
            message: message.into(),
            primary: true,
        });
        self
    }

    /// Adds a context annotation to the snippet. Context annotations should be used if further
    /// context can be added. It is meant to help the user understand the circumstances that lead
    /// to something. Keep in mind, that these context erros will probably require advanced code
    /// understanding, but users might really appreciate that extra context.
    ///
    /// These context annoations should pretty much always be used in combination with primary
    /// annotations.
    ///
    /// Keep in mind, at least one annotation is required, for rendering the snippet.
    ///
    /// Annotations will be rendered in the order of declaration.
    pub fn context(mut self, range: Range<usize>, message: impl Into<String>) -> Self {
        self.annotations.push(Annotation {
            range,
            message: message.into(),
            primary: false,
        });
        self
    }
}
