use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

use ariadne::{Color, ColorGenerator, Config, Label, Report, ReportKind};
use source::types::FileID;

use crate::diagnostic::{Diagnostic, Level, Snippet};
use crate::source::SourceLookup;

struct DiagnosticStyling {
    heading: Color,
    message: Color,
    code: Color,
}

trait Renderable {
    fn heading(&self) -> &'static str;
    fn styling(&self) -> DiagnosticStyling;
    fn output(&self) -> Box<dyn Write>;
    fn config(&self) -> Config;
}

impl Renderable for Diagnostic {
    fn heading(&self) -> &'static str {
        match self.level {
            Level::Error => "Error",
            Level::Warning => "Warning",
            Level::Info => "Info",
        }
    }

    fn styling(&self) -> DiagnosticStyling {
        match self.level {
            Level::Error => DiagnosticStyling {
                heading: Color::Red,
                message: Color::White,
                code: Color::Red,
            },
            Level::Warning => DiagnosticStyling {
                heading: Color::Yellow,
                message: Color::White,
                code: Color::Yellow,
            },
            Level::Info => DiagnosticStyling {
                heading: Color::Cyan,
                message: Color::White,
                code: Color::Cyan,
            },
        }
    }

    fn output(&self) -> Box<dyn Write> {
        match self.level {
            Level::Error => Box::new(io::stderr()),
            Level::Warning | Level::Info => Box::new(io::stdout()),
        }
    }

    fn config(&self) -> Config {
        Config::default().with_underlines(true)
    }
}

#[derive(Clone, Debug)]
struct CachedSource {
    path: String,
    content: String,
}

pub struct Renderer<'a> {
    diagnostic: &'a Diagnostic,
    source: &'a dyn SourceLookup,
}

impl<'a> Renderer<'a> {
    pub(crate) fn render(
        diagnostic: &'a Diagnostic,
        source: &'a impl SourceLookup,
    ) -> io::Result<()> {
        let renderer = Self::new(diagnostic, source);

        renderer.print()
    }

    fn new(diagnostic: &'a Diagnostic, source: &'a dyn SourceLookup) -> Self {
        Self { diagnostic, source }
    }

    fn print(&self) -> io::Result<()> {
        let mut writer = self.diagnostic.output();

        let mut cache: HashMap<FileID, CachedSource> = HashMap::new();
        let unique: HashSet<_> = self.diagnostic.snippets.iter().map(|s| s.file).collect();

        for id in unique {
            let path = self
                .source
                .get_path(id)
                .expect(format!("Fatal: FileID {:?} missing from sourcemap", id).as_str())
                .to_string_lossy()
                .to_string();

            let content = self
                .source
                .get_content(id)
                .expect(format!("Fatal: FileID {:?} missing from sourcemap", id).as_str())
                .to_string();

            cache.insert(id, CachedSource { path, content });
        }

        let (primary_path, primary_range) = self.primary_span(&cache);

        let mut builder = Report::build(
            ReportKind::Custom(self.diagnostic.heading(), self.diagnostic.styling().heading),
            (primary_path, primary_range),
        )
        .with_message(&self.diagnostic.message)
        .with_config(self.diagnostic.config());

        if let Some(code) = &self.diagnostic.code {
            builder = builder.with_code(code);
        }

        if let Some(help) = &self.diagnostic.help {
            builder = builder.with_help(help);
        }

        let mut colors = ColorGenerator::new();

        for snippet in &self.diagnostic.snippets {
            let path = &cache.get(&snippet.file).unwrap().path;

            for (ai, annotation) in snippet.annotations.iter().enumerate() {
                let annotation_color = colors.next();
                let last_index = annotation.ranges.len().saturating_sub(1);

                for (ri, range) in annotation.ranges.iter().enumerate() {
                    let mut label =
                        Label::new((path.clone(), range.clone())).with_color(annotation_color);

                    if ri == last_index {
                        label = label.with_message(&annotation.message)
                    }

                    builder = builder.with_label(label);
                }
            }
        }
        let sources = cache.into_values().map(|c| (c.path, c.content));

        builder
            .finish()
            .write(ariadne::sources(sources), &mut writer)
    }

    fn primary_span(
        &self,
        cache: &HashMap<FileID, CachedSource>,
    ) -> (String, std::ops::Range<usize>) {
        let fallback = (String::new(), 0..0);

        self.diagnostic
            .snippets
            .first()
            .and_then(|snippet| {
                let mut ranges = snippet
                    .annotations
                    .iter()
                    .flat_map(|a| a.ranges.iter())
                    .map(|r| snippet.file.span(r.start as u32, r.end as u32));

                let first = ranges.next()?;
                let merged = ranges.fold(first, |acc, next| acc.merge(next).unwrap_or(acc));

                // Unwrapping is safe here, since the snippets for all diagnostics are guaranteed to be
                // loaded
                let path = &cache.get(&snippet.file).unwrap().path;

                Some((path.clone(), merged.start.0 as usize..merged.end.0 as usize))
            })
            .unwrap_or(fallback)
    }
}
