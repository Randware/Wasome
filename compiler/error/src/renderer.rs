use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

use ariadne::{Color, Config, Fmt, Label, Report, ReportKind};
use source::types::FileID;

use crate::diagnostic::{Diagnostic, Level};
use crate::source::SourceLookup;

struct DiagnosticStyling {
    heading: Color,
    message: Color,
    error: Color,
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
                message: Color::BrightWhite,
                error: Color::Red,
            },
            Level::Warning => DiagnosticStyling {
                heading: Color::Yellow,
                message: Color::BrightWhite,
                error: Color::Yellow,
            },
            Level::Info => DiagnosticStyling {
                heading: Color::Cyan,
                message: Color::BrightWhite,
                error: Color::Cyan,
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
        Config::default()
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

        let cache = self.build_cache();

        let (primary_path, primary_range) = self.primary_span(&cache);

        let styling = self.diagnostic.styling();

        let mut builder = Report::build(
            ReportKind::Custom(self.diagnostic.heading(), styling.heading),
            (primary_path, primary_range),
        )
        .with_config(self.diagnostic.config())
        .with_message(
            &self
                .diagnostic
                .message
                .clone()
                .fg(self.diagnostic.styling().message),
        );

        if let Some(code) = &self.diagnostic.code {
            builder = builder.with_code(code);
        }

        if let Some(help) = &self.diagnostic.help {
            builder = builder.with_help(help);
        }

        for (path, range, message) in self.collect_labels(&cache) {
            builder = builder.with_label(
                Label::new((path, range))
                    .with_color(styling.error)
                    .with_message(message),
            );
        }

        let sources = cache
            .iter()
            .map(|(_, c)| (c.path.clone(), c.content.clone()));

        builder
            .finish()
            .write(ariadne::sources(sources), &mut writer)
    }

    fn build_cache(&self) -> HashMap<FileID, CachedSource> {
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

        cache
    }

    fn primary_span(
        &self,
        cache: &HashMap<FileID, CachedSource>,
    ) -> (String, std::ops::Range<usize>) {
        if let Some(first_snippet) = self.diagnostic.snippets.first() {
            if let Some(first_annotation) = first_snippet.annotations.first() {
                if let Some(first_range) = first_annotation.ranges.first() {
                    let path = &cache.get(&first_snippet.file).unwrap().path;

                    let span = first_snippet
                        .file
                        .span(first_range.start as u32, first_range.end as u32);

                    return (path.clone(), span.start.0 as usize..span.end.0 as usize);
                }
            }
        }

        (String::new(), 0..0)
    }

    fn collect_labels(
        &self,
        cache: &HashMap<FileID, CachedSource>,
    ) -> Vec<(String, std::ops::Range<usize>, &str)> {
        let mut labels = Vec::new();

        for snippet in &self.diagnostic.snippets {
            let path = &cache.get(&snippet.file).unwrap().path;

            for annotation in &snippet.annotations {
                for range in &annotation.ranges {
                    labels.push((path.clone(), range.clone(), annotation.message.as_str()));
                }
            }
        }

        labels.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.start.cmp(&b.1.start)));

        labels
    }
}
