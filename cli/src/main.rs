mod command;
mod dependencies;
mod error;
mod execute;
mod manifest;
mod template;

use ::error::diagnostic::{Diagnostic, Level, Snippet};
use clap::Parser;
use std::io;

use crate::{command::Cli, execute::Executable};

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    if let Err(err) = cli.execute() {
        match err {
            error::CliError::ManifestParse(toml_err, source, file_id) => {
                let snippet = if let Some(range) = toml_err.span() {
                    Snippet::builder()
                        .file(file_id)
                        .primary(range, toml_err.message())
                        .build()
                } else {
                    Snippet::builder().file(file_id).build()
                };

                Diagnostic::builder()
                    .level(Level::Error)
                    .message("Malformed project manifest")
                    .snippet(snippet)
                    .build()
                    .print_snippets(&source)?;
            }

            error::CliError::Manifest(err) => {
                let builder = Diagnostic::builder()
                    .level(Level::Error)
                    .message(err.to_string());

                match err {
                    error::ManifestError::NotFound => {
                        builder
                            .help("Try initalizing a project first")
                            .build()
                            .print()?;
                    }
                    error::ManifestError::AlreadyFound => {
                        builder
                            .help("Try initializing your project somewhere else")
                            .build()
                            .print()?;
                    }
                    error::ManifestError::NoEntry(_) => {
                        builder
                            .help(format!(
                                "Make sure your project has a valid '{}' or '{}' file",
                                manifest::BINARY_ENTRY_FILE,
                                manifest::LIBRARY_ENTRY_FILE
                            ))
                            .build()
                            .print()?;
                    }
                    error::ManifestError::MultipleEntries(_) => {
                        builder
                            .help(format!(
                                "Make sure your project only has either a '{}' or '{}' file",
                                manifest::BINARY_ENTRY_FILE,
                                manifest::LIBRARY_ENTRY_FILE
                            ))
                            .build()
                            .print()?;
                    }
                    error::ManifestError::MissingDependency(_, _) => {
                        builder
                            .help("Make sure you have a local or global version of the dependency available and its manifest is valid")
                            .build()
                            .print()?;
                    }
                    error::ManifestError::Parse(_) => {
                        panic!("Manifest parse errors should never be returned standalone")
                    }
                    _ => {
                        builder.build().print()?;
                    }
                }
            }

            error::CliError::Io(err) => {
                Diagnostic::builder()
                    .level(Level::Error)
                    .message(err.to_string())
                    .build()
                    .print()?;
            }
        }
    }

    Ok(())
}
