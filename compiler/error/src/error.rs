use std::{
    io::{self, Write},
    ops::Range,
};

use ariadne::{Color, Label, Report, ReportKind, Source, Span};
use bon::Builder;

use crate::snippet::Snippet;

#[derive(Builder)]
pub struct Error {
    #[builder(field)]
    snippets: Vec<Snippet>,

    kind: ErrorKind,
    message: &'static str,
}

impl<S: error_builder::State> ErrorBuilder<S> {
    pub fn snippet(mut self, snippet: Snippet) -> Self {
        self.snippets.push(snippet);
        self
    }
}

impl Error {
    pub fn print(&self) -> io::Result<()> {
        self.print_to(self.kind.output())
    }

    pub fn print_to(&self, output: Box<dyn Write>) -> io::Result<()> {
        self.render().write(("", Source::from("")), output)
    }

    fn render(&self) -> Report<'_, (&str, Range<usize>)> {
        let builder = Report::build(
            ReportKind::Custom(self.kind.heading(), self.kind.styling().kind_heading),
            ("", 0..0),
        )
        .with_message(self.message)
        .with_labels(self.snippets.iter().map(|snippet| {
            Label::new((
                snippet.source.file().filepath().to_str().unwrap(),
                snippet.source.start().line()..snippet.source.start().char(),
            ))
        }));

        builder.finish()
    }
}

pub struct ErrorStyling {
    kind_heading: Color,
    message: Color,
    code_snippet: Color,
    code_error: Color,
}

pub enum ErrorKind {
    Error,
    Warning,
    Info,
}

trait PrintableError {
    fn heading(&self) -> &'static str;
    fn styling(&self) -> ErrorStyling;
    fn output(&self) -> Box<dyn Write>;
}

impl PrintableError for ErrorKind {
    fn heading(&self) -> &'static str {
        match self {
            ErrorKind::Error => "Error",
            ErrorKind::Warning => "Warning",
            ErrorKind::Info => "Info",
        }
    }

    fn styling(&self) -> ErrorStyling {
        match self {
            ErrorKind::Error => ErrorStyling {
                kind_heading: Color::Red,
                message: Color::BrightWhite,
                code_snippet: Color::Primary,
                code_error: Color::Red,
            },
            ErrorKind::Warning => ErrorStyling {
                kind_heading: Color::Yellow,
                message: Color::BrightWhite,
                code_snippet: Color::Primary,
                code_error: Color::Yellow,
            },
            ErrorKind::Info => ErrorStyling {
                kind_heading: Color::Blue,
                message: Color::BrightWhite,
                code_snippet: Color::Primary,
                code_error: Color::Blue,
            },
        }
    }

    fn output(&self) -> Box<dyn Write> {
        match self {
            ErrorKind::Error => Box::new(io::stderr()),
            ErrorKind::Warning => Box::new(io::stdout()),
            ErrorKind::Info => Box::new(io::stdout()),
        }
    }
}
