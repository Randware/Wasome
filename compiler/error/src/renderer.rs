use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

use ariadne::{Config, Label, Report, ReportBuilder, ReportKind};
use regex::Regex;
use source::types::FileID;
use yansi::{Color, Paint};

use crate::diagnostic::{Diagnostic, Level, Snippet};
use crate::source::SourceLookup;

/// Defines the color scheme for diagnostic elements.
struct DiagnosticStyling {
    type_heading: Color,
    help_heading: Color,
    diagnostic_message: Color,
    annotation_message: Color,
    help_message: Color,
    primary_highlight: Color,
    context_highlight: Color,
}

/// Stores cached content and path information for a source file.
#[derive(Clone, Debug)]
struct CachedSource {
    path: String,
    content: String,
}

/// Handles the construction and rendering of diagnostics.
pub struct Renderer<'a> {
    diagnostic: &'a Diagnostic,
    writer: Box<dyn Write>,
    styling: DiagnosticStyling,
    cache: HashMap<FileID, CachedSource>,
}

impl<'a> Renderer<'a> {
    /// Constructs a [`Renderer`] and renders the provided [`Diagnostic`].
    pub(crate) fn render(
        diagnostic: &'a Diagnostic,
        source: &'a impl SourceLookup,
    ) -> io::Result<()> {
        let mut renderer = Self::new(diagnostic, source);

        renderer.print()
    }

    fn new(diagnostic: &'a Diagnostic, source: &'a dyn SourceLookup) -> Self {
        let writer = Self::resolve_output(diagnostic.level);
        let styling = Self::resolve_styling(diagnostic.level);

        let mut cache = HashMap::new();
        let unique: HashSet<_> = diagnostic.snippets.iter().map(|s| s.file).collect();

        // Populate cache with source content. Missing sources are skipped gracefully
        // to ensure the diagnostic can still be rendered (albeit partially).
        for id in unique {
            if let Some(path_buf) = source.get_path(id) {
                let path = path_buf.to_string_lossy().to_string();

                if let Some(content) = source.get_content(id) {
                    cache.insert(
                        id,
                        CachedSource {
                            path,
                            content: content.to_string(),
                        },
                    );
                }
            }
        }

        Self {
            diagnostic,
            writer,
            styling,
            cache,
        }
    }

    /// Determines the appropriate output stream (`stdout` or `stderr`) based on the [`Level`].
    fn resolve_output(level: Level) -> Box<dyn Write> {
        match level {
            Level::Error => Box::new(io::stderr()),
            Level::Warning | Level::Info => Box::new(io::stdout()),
        }
    }

    /// Determines the appropriate color styling based on the [`Level`].
    fn resolve_styling(level: Level) -> DiagnosticStyling {
        match level {
            Level::Error => DiagnosticStyling {
                type_heading: Color::Red,
                help_heading: Color::Green,
                diagnostic_message: Color::Primary,
                annotation_message: Color::Primary,
                help_message: Color::Primary,
                primary_highlight: Color::Red,
                context_highlight: Color::Blue,
            },
            Level::Warning => DiagnosticStyling {
                type_heading: Color::Yellow,
                help_heading: Color::Green,
                diagnostic_message: Color::Primary,
                annotation_message: Color::Primary,
                help_message: Color::Primary,
                primary_highlight: Color::Yellow,
                context_highlight: Color::Blue,
            },
            Level::Info => DiagnosticStyling {
                type_heading: Color::Cyan,
                help_heading: Color::Green,
                diagnostic_message: Color::Primary,
                annotation_message: Color::Primary,
                help_message: Color::Primary,
                primary_highlight: Color::Cyan,
                context_highlight: Color::Blue,
            },
        }
    }

    /// Returns the rendering configuration based on the [`Level`].
    fn resolve_config(level: Level) -> Config {
        match level {
            _ => Config::default(),
        }
    }

    /// Orchestrates the printing of all diagnostic components.
    fn print(&mut self) -> io::Result<()> {
        // Add padding before the diagnostic
        writeln!(self.writer)?;

        self.print_header()?;

        for snippet in &self.diagnostic.snippets {
            self.render_snippet(snippet)?;
        }

        self.render_help()?;

        // Add padding after the diagnostic
        writeln!(self.writer)?;

        Ok(())
    }

    /// Formats and prints the diagnostic header.
    fn print_header(&mut self) -> io::Result<()> {
        let title = match self.diagnostic.level {
            Level::Error => "Error",
            Level::Warning => "Warning",
            Level::Info => "Info",
        };

        let heading = format!(" {} ", title);

        write!(
            self.writer,
            "{}",
            heading.fg(self.styling.type_heading).invert().bold()
        )?;

        write!(
            self.writer,
            " {}",
            self.diagnostic
                .message
                .clone()
                .fg(self.styling.diagnostic_message)
        )?;

        if let Some(code) = &self.diagnostic.code {
            write!(
                self.writer,
                " {}",
                format!("{}", code).fg(self.styling.type_heading).bold()
            )?;
        }

        writeln!(self.writer)?;

        Ok(())
    }

    /// Formats and prints a single code [`Snippet`].
    fn render_snippet(&mut self, snippet: &Snippet) -> io::Result<()> {
        if let Some(path) = self
            .cache
            .get(&snippet.file)
            .and_then(|c| Some(c.path.clone()))
        {
            let kind = ReportKind::Custom("", self.styling.type_heading);

            // Use a dummy range 0..0 for the primary location, as we strip it manually later.
            let report = Report::build(kind, (path.clone(), 0..0))
                .with_config(Self::resolve_config(self.diagnostic.level));

            let report = self.label_report(report, snippet);

            let buffer = self.strip_report(report)?;

            self.writer.write_all(&buffer)?;
        }

        Ok(())
    }

    /// Formats and prints the help message footer.
    fn render_help(&mut self) -> io::Result<()> {
        if let Some(help) = &self.diagnostic.help {
            writeln!(
                self.writer,
                "{} {}",
                " Help ".fg(self.styling.help_heading).invert().bold(),
                help.fg(self.styling.help_message)
            )?;
        }

        Ok(())
    }

    /// Post-processes the `ariadne` [`Report`] to strip unwanted artifacts.
    ///
    /// This removes the default header lines and cleans up primary location indicators
    /// that are not needed for this renderer's style.
    fn strip_report(
        &mut self,
        report: ReportBuilder<(String, std::ops::Range<usize>)>,
    ) -> io::Result<Vec<u8>> {
        let mut buffer: Vec<u8> = Vec::new();
        let sources = ariadne::sources(
            self.cache
                .values()
                .map(|c| (c.path.clone(), c.content.clone())),
        );

        // Render the ariadne report into the buffer
        report.finish().write(sources, &mut buffer)?;

        let s =
            String::from_utf8(buffer).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let mut output_lines = Vec::new();

        for (i, line) in s.lines().enumerate() {
            // Skip the first line as it contains an unwanted header
            if i == 0 {
                continue;
            }

            // Strip ANSI sequences to safely check the start of the line
            let clean = Self::strip_ansi(line);
            let trimmed = clean.trim_start();

            // Detect if this is a frame line and strip the location info if necessary
            if trimmed.starts_with('╭') || trimmed.starts_with('├') {
                output_lines.push(Self::remove_primary_location(line));
            } else {
                output_lines.push(line.to_string());
            }
        }

        // Convert lines back to a byte stream
        let mut output = output_lines.join("\n").into_bytes();

        // Restore the trailing newline
        output.push(b'\n');

        Ok(output)
    }

    /// Removes ANSI escape sequences from a string.
    fn strip_ansi(s: &str) -> String {
        let re = Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]").unwrap();
        re.replace_all(s, "").to_string()
    }

    /// Removes the primary error location text (e.g., line/column numbers) from a line.
    fn remove_primary_location(line: &str) -> String {
        if let Some(colon_idx) = line.find(':') {
            if let Some(space_offset) = line[colon_idx..].find(' ') {
                let space_idx = colon_idx + space_offset;

                return format!("{}{}", &line[..colon_idx], &line[space_idx..]);
            }
        }

        line.to_string()
    }

    /// Applies annotations from the [`Snippet`] to the [`ReportBuilder`].
    fn label_report<'b>(
        &self,
        mut builder: ReportBuilder<'b, (String, std::ops::Range<usize>)>,
        snippet: &'b Snippet,
    ) -> ReportBuilder<'b, (String, std::ops::Range<usize>)> {
        if let Some(cached) = self.cache.get(&snippet.file) {
            for ann in &snippet.annotations {
                let color = match ann.primary {
                    true => self.styling.primary_highlight,
                    false => self.styling.context_highlight,
                };

                let label = Label::new((cached.path.clone(), ann.range.clone()))
                    .with_message(&ann.message.fg(self.styling.annotation_message))
                    .with_color(color);

                builder.add_label(label);
            }
        }

        builder
    }
}
