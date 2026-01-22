use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

use ariadne::{Config, Label, Report, ReportBuilder, ReportKind};
use source::types::FileID;
use yansi::{Color, Paint};

use crate::diagnostic::{Diagnostic, Level, Snippet};
use crate::source::SourceLookup;

struct DiagnosticStyling {
    type_heading: Color,
    help_heading: Color,
    diagnostic_message: Color,
    annotation_message: Color,
    help_message: Color,
    snippet_highlight: Color,
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
                type_heading: Color::Red,
                help_heading: Color::Green,
                diagnostic_message: Color::Primary,
                annotation_message: Color::Primary,
                help_message: Color::Primary,
                snippet_highlight: Color::Red,
            },
            Level::Warning => DiagnosticStyling {
                type_heading: Color::Yellow,
                help_heading: Color::Green,
                diagnostic_message: Color::Primary,
                annotation_message: Color::Primary,
                help_message: Color::Primary,
                snippet_highlight: Color::Yellow,
            },
            Level::Info => DiagnosticStyling {
                type_heading: Color::Cyan,
                help_heading: Color::Green,
                diagnostic_message: Color::Primary,
                annotation_message: Color::Primary,
                help_message: Color::Primary,
                snippet_highlight: Color::Cyan,
            },
        }
    }

    fn resolve_config(level: Level) -> Config {
        match level {
            _ => Config::default(),
        }
    }

    fn print(&mut self) -> io::Result<()> {
        // Add an empty line before this diagnostic
        writeln!(self.writer)?;

        self.render_header()?;

        for snippet in &self.diagnostic.snippets {
            self.render_snippet(snippet)?;
        }

        self.render_help()?;

        // Add an empty line after this diagnostic
        writeln!(self.writer)?;

        Ok(())
    }

    fn render_header(&mut self) -> io::Result<()> {
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

    fn render_snippet(&mut self, snippet: &Snippet) -> io::Result<()> {
        let (path, primary_range) = if let Some(cached) = self.cache.get(&snippet.file) {
            (cached.path.clone(), snippet.primary.range.clone())
        } else {
            (String::new(), 0..0)
        };

        let kind = ReportKind::Custom("", self.styling.type_heading);

        let report = Report::build(kind, (path, primary_range))
            .with_config(Self::resolve_config(self.diagnostic.level));

        let report = self.label_report(report, snippet);

        let buffer = self.render_report(report)?;

        self.writer.write_all(&buffer)?;

        Ok(())
    }

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

    fn render_report(
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

        // Remove the first line of the report (ariadne header)
        if let Some(newline_idx) = buffer.iter().position(|&b| b == b'\n') {
            buffer.drain(0..=newline_idx);
        }

        Ok(buffer)
    }

    fn label_report<'b>(
        &self,
        mut builder: ReportBuilder<'b, (String, std::ops::Range<usize>)>,
        snippet: &'b Snippet,
    ) -> ReportBuilder<'b, (String, std::ops::Range<usize>)> {
        if let Some(cached) = self.cache.get(&snippet.file) {
            let mut annotations = snippet.context.clone();
            annotations.push(snippet.primary.clone());

            annotations.sort_by_key(|ann| ann.range.start);

            for ann in annotations {
                let label = Label::new((cached.path.clone(), ann.range.clone()))
                    .with_message(&ann.message.fg(self.styling.annotation_message))
                    .with_color(self.styling.snippet_highlight);
                builder.add_label(label);
            }
        }

        builder
    }
}
