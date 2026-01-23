use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

use ariadne::{Config, Label, Report, ReportBuilder, ReportKind};
use regex::Regex;
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
    primary_highlight: Color,
    context_highlight: Color,
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

        // We add our files with None checks, since we always want the user to see at least something,
        // even if we are missing sources
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
        if let Some(path) = self
            .cache
            .get(&snippet.file)
            .and_then(|c| Some(c.path.clone()))
        {
            let kind = ReportKind::Custom("", self.styling.type_heading);

            // Primary range is 0..0, since we strip it anyway
            let report = Report::build(kind, (path.clone(), 0..0))
                .with_config(Self::resolve_config(self.diagnostic.level));

            let report = self.label_report(report, snippet);

            let buffer = self.strip_report(report)?;

            self.writer.write_all(&buffer)?;
        }

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

        let s =
            String::from_utf8(buffer).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let mut output_lines = Vec::new();

        for (i, line) in s.lines().enumerate() {
            if i == 0 {
                continue;
            }

            // Strip ANSI color sequences from line, to detect the first actual character
            let clean = Self::strip_ansi(line);
            // Strip whitespace at the start of line
            let trimmed = clean.trim_start();

            // If our line stars with one of these charactes, it is a header line
            if trimmed.starts_with('╭') || trimmed.starts_with('├') {
                output_lines.push(Self::remove_primary_location(line));
            } else {
                output_lines.push(line.to_string());
            }
        }

        let mut output = output_lines.join("\n").into_bytes();
        output.push(b'\n');

        Ok(output)
    }

    fn strip_ansi(s: &str) -> String {
        let re = Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]").unwrap();
        re.replace_all(s, "").to_string()
    }

    fn remove_primary_location(line: &str) -> String {
        if let Some(colon_idx) = line.find(':') {
            if let Some(space_offset) = line[colon_idx..].find(' ') {
                let space_idx = colon_idx + space_offset;

                return format!("{}{}", &line[..colon_idx], &line[space_idx..]);
            }
        }

        line.to_string()
    }

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
