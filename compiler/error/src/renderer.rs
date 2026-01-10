use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

use ariadne::{Config, Label, Report, ReportBuilder, ReportKind};
use source::types::FileID;
use yansi::{Color, Paint};

use crate::diagnostic::{Diagnostic, Level, Snippet};
use crate::source::SourceLookup;

struct DiagnosticStyling {
    heading: Color,
    message: Color,
    error: Color,
    help: Color,
}

#[derive(Clone, Debug)]
struct CachedSource {
    path: String,
    content: String,
}

pub struct Renderer<'a> {
    diagnostic: &'a Diagnostic,
    writer: Box<dyn Write>,
    styling: DiagnosticStyling,
    cache: HashMap<FileID, CachedSource>,
}

impl<'a> Renderer<'a> {
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

    fn resolve_output(level: Level) -> Box<dyn Write> {
        match level {
            Level::Error => Box::new(io::stderr()),
            Level::Warning | Level::Info => Box::new(io::stdout()),
        }
    }

    fn resolve_styling(level: Level) -> DiagnosticStyling {
        match level {
            Level::Error => DiagnosticStyling {
                heading: Color::Red,
                message: Color::BrightWhite,
                error: Color::Red,
                help: Color::Green,
            },
            Level::Warning => DiagnosticStyling {
                heading: Color::Yellow,
                message: Color::BrightWhite,
                error: Color::Yellow,
                help: Color::Green,
            },
            Level::Info => DiagnosticStyling {
                heading: Color::Cyan,
                message: Color::BrightWhite,
                error: Color::Cyan,
                help: Color::Green,
            },
        }
    }

    fn resolve_config(level: Level) -> Config {
        match level {
            _ => Config::default(),
        }
    }

    fn print(&mut self) -> io::Result<()> {
        self.render_header()?;

        for snippet in &self.diagnostic.snippets {
            self.render_snippet(snippet)?;
        }

        self.render_help()?;

        // Add an empty line between diagnostics for separation
        writeln!(self.writer)?;

        Ok(())
    }

    fn render_help(&mut self) -> io::Result<()> {
        if let Some(help) = &self.diagnostic.help {
            writeln!(
                self.writer,
                "{} {}",
                " Help ".bg(self.styling.help).bold(),
                help
            )?;
        }

        Ok(())
    }

    fn render_snippet(&mut self, snippet: &Snippet) -> io::Result<()> {
        let kind = ReportKind::Custom("", self.styling.heading);

        let report = Report::build(kind, (String::new(), 0..0))
            .with_config(Self::resolve_config(self.diagnostic.level));

        let report = self.label_report(report, snippet);

        let buffer = self.strip_report(report)?;

        self.writer.write_all(&buffer)?;

        Ok(())
    }

    fn render_header(&mut self) -> io::Result<()> {
        let title = match self.diagnostic.level {
            Level::Error => "Error",
            Level::Warning => "Warning",
            Level::Info => "Info",
        };

        let heading = format!(" {} ", title);

        write!(self.writer, "{}", heading.bg(self.styling.heading))?;

        write!(
            self.writer,
            " {}",
            self.diagnostic.message.clone().fg(self.styling.message)
        )?;

        if let Some(code) = &self.diagnostic.code {
            write!(
                self.writer,
                " {}",
                format!("[{}] ", code).fg(self.styling.heading)
            )?;
        }

        writeln!(self.writer)?;

        Ok(())
    }

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

        report.finish().write(sources, &mut buffer)?;

        // Remove the first line of the report
        if let Some(newline_idx) = buffer.iter().position(|&b| b == b'\n') {
            buffer.drain(0..=newline_idx);
        }

        // Remove the primary error position
        if let Some(colon_idx) = buffer.iter().position(|&b| b == b':') {
            if let Some(space_offset) = buffer[colon_idx..].iter().position(|&b| b == b' ') {
                let space_idx = colon_idx + space_offset - 1;

                buffer.drain(colon_idx..=space_idx);
            }
        }

        Ok(buffer)
    }

    fn label_report<'b>(
        &self,
        mut builder: ReportBuilder<'b, (String, std::ops::Range<usize>)>,
        snippet: &'b Snippet,
    ) -> ReportBuilder<'b, (String, std::ops::Range<usize>)> {
        let mut labels = Vec::new();

        if let Some(cached) = self.cache.get(&snippet.file) {
            for ann in &snippet.annotations {
                for range in &ann.ranges {
                    labels.push((cached.path.clone(), range.clone(), ann.message.as_str()));
                }
            }
        }

        labels.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.start.cmp(&b.1.start)));

        for (path, range, message) in labels {
            builder = builder.with_label(
                Label::new((path, range))
                    .with_color(self.styling.error)
                    .with_message(message),
            );
        }

        builder
    }
}
