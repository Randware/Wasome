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
        Config::default()
    }
}

#[derive(Clone, Debug)]
struct CachedSource {
    path: String,
    content: String,
}

struct ResolutionCache {
    data: HashMap<FileID, CachedSource>,
}

impl ResolutionCache {
    fn new(snippets: &[Snippet], source_lookup: &dyn SourceLookup, writer: &mut dyn Write) -> Self {
        let unique_ids: HashSet<FileID> = snippets.iter().map(|s| s.file).collect();
        let mut data = HashMap::with_capacity(unique_ids.len());

        for id in unique_ids {
            let path = match source_lookup.get_path(id) {
                Some(p) => p.to_string_lossy().to_string(),
                None => {
                    // We ignore the Result here, since we can't really do anything, if we are not
                    // even able to print
                    let _ = writeln!(
                        writer,
                        "Warning: Could not resolve path for <FileID {:?}>, filepaths can't be displayed",
                        id
                    );

                    format!("<FileID {:?}>", id)
                }
            };

            let content = match source_lookup.get_content(id) {
                Some(c) => c.to_string(),
                None => {
                    // We ignore the Result here, since we can't really do anything, if we are not
                    // even able to print
                    let _ = writeln!(
                        writer,
                        "Warning: Could not read content for <FileID {:?}>, snippets can't be displayed",
                        id
                    );

                    String::new()
                }
            };

            data.insert(id, CachedSource { path, content });
        }

        Self { data }
    }
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

        let cache = ResolutionCache::new(&self.diagnostic.snippets, self.source, &mut writer);

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

        for snippet in &self.diagnostic.snippets {
            // Unwrapping is safe here, since the snippets for all diagnostics are guaranteed to be
            // loaded
            let path = &cache.data.get(&snippet.file).unwrap().path;

            for annotation in &snippet.annotations {
                let mut colors = ColorGenerator::new();

                for range in &annotation.ranges {
                    let label = Label::new((path.clone(), range.clone()))
                        .with_message(&annotation.message)
                        .with_color(colors.next());

                    builder = builder.with_label(label);
                }
            }
        }

        let sources = cache.data.into_values().map(|c| (c.path, c.content));

        builder
            .finish()
            .write(ariadne::sources(sources), &mut writer)
    }

    fn primary_span(&self, cache: &ResolutionCache) -> (String, std::ops::Range<usize>) {
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
                let path = &cache.data.get(&snippet.file).unwrap().path;

                Some((path.clone(), merged.start.0 as usize..merged.end.0 as usize))
            })
            .unwrap_or(fallback)
    }
}
