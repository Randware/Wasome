mod command;
mod execute;

use anyhow::Result;
use clap::Parser;

use crate::{command::Cli, execute::Executable};

fn main() -> Result<()> {
    let cli = Cli::parse();

    cli.execute()
}
