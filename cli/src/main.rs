mod command;
mod dependencies;
mod error;
mod execute;
mod manifest;
mod template;
pub mod workspace;

use clap::Parser;
use std::{io, process::ExitCode};

use crate::{command::Cli, error::CliError, execute::Executable};

fn main() -> io::Result<ExitCode> {
    let cli = Cli::parse();

    if let Err(err) = cli.execute() {
        if !matches!(&err, CliError::CompilationFailed) {
            err.print()?;
        }

        return Ok(ExitCode::FAILURE);
    }

    Ok(ExitCode::SUCCESS)
}
