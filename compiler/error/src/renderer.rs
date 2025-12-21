use std::io;

use crate::diagnostic::Diagnostic;
use crate::source::{NoSource, SourceLookup};

pub struct Renderer;

impl Renderer {
    /// Renders the diagnostic, including source code snippets to stdout/stderr
    pub fn print_snippets(diagnostic: &Diagnostic, source: &impl SourceLookup) -> io::Result<()> {
        Ok(())
    }

    /// Renders the diagnostic without any code snippets to stdout/stderr
    pub fn print(diagnostic: &Diagnostic) -> io::Result<()> {
        Self::print_snippets(diagnostic, &NoSource)
    }
}
