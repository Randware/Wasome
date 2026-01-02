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
    source: &'a dyn SourceLookup,
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
            source,
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

    fn print(&mut self) -> io::Result<()> {
        self.render_header()?;

        for snippet in &self.diagnostic.snippets {
            let (path, range) = self.primary_span(snippet);
            let kind = ReportKind::Custom("", self.styling.heading);
            let builder = Report::build(kind, (path, range)).with_config(Config::default());

            let report = self.add_labels_to_report(builder, snippet);

            self.render_report_headless(report)?;
        }

        if let Some(help) = &self.diagnostic.help {
            writeln!(self.writer, "{} {}", " Help ".bg(self.styling.help).bold(), help)?;
        }

        // Add an empty line between diagnostics for separation
        writeln!(self.writer)?;

        Ok(())
    }

    fn render_header(&mut self) -> io::Result<()> {
        let heading_text = match self.diagnostic.level {
            Level::Error => "Error",
            Level::Warning => "Warning",
            Level::Info => "Info",
        };

        let heading_str = format!(" {} ", heading_text);
        let heading = heading_str.bg(self.styling.heading).bold();

        write!(self.writer, "{}", heading)?;

        write!(
            self.writer,
            " {}",
            self.diagnostic.message.clone().fg(self.styling.message)
        )?;

        if let Some(code) = &self.diagnostic.code {
            write!(self.writer, " {}", format!("[{}] ", code).fg(self.styling.heading))?;
        }

        writeln!(self.writer)?;

        Ok(())
    }

    /// Renders a report but strips the first line (the empty header)
    fn render_report_headless(
        &mut self,
        report: ReportBuilder<(String, std::ops::Range<usize>)>,
    ) -> io::Result<()> {
        let mut buffer = Vec::new();
        let sources = ariadne::sources(self.cache.values().map(|c| (c.path.clone(), c.content.clone())));

        report.finish().write(sources, &mut buffer)?;

        if let Some(newline_pos) = buffer.iter().position(|&b| b == b'\n') {
            self.writer.write_all(&buffer[newline_pos + 1..])?;
        } else {
            self.writer.write_all(&buffer)?;
        }

        Ok(())
    }

    fn add_labels_to_report<'b>(
        &self,
        mut builder: ReportBuilder<'b, (String, std::ops::Range<usize>)>,
        snippet: &'b Snippet,
    ) -> ReportBuilder<'b, (String, std::ops::Range<usize>)> {
        for (path, range, message) in self.collect_labels(snippet) {
            builder = builder.with_label(
                Label::new((path, range))
                    .with_color(self.styling.error)
                    .with_message(message),
            );
        }

        builder
    }

    fn primary_span(
        &self,
        snippet: &Snippet,
    ) -> (String, std::ops::Range<usize>) {
        if let Some(first_ann) = snippet.annotations.first() {
            if let Some(range) = first_ann.ranges.first() {
                if let Some(cached) = self.cache.get(&snippet.file) {
                    let span = snippet.file.span(range.start as u32, range.end as u32);

                    return (
                        cached.path.clone(),
                        span.start.0 as usize..span.end.0 as usize,
                    );
                }
            }
        }

        (String::new(), 0..0)
    }

    fn collect_labels<'b>(
        &self,
        snippet: &'b Snippet,
    ) -> Vec<(String, std::ops::Range<usize>, &'b str)> {
        let mut labels = Vec::new();

        if let Some(cached) = self.cache.get(&snippet.file) {
            for ann in &snippet.annotations {
                for range in &ann.ranges {
                    labels.push((cached.path.clone(), range.clone(), ann.message.as_str()));
                }
            }
        }

        labels.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.start.cmp(&b.1.start)));

        labels
    }
}
