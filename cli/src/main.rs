mod command;
mod error;
mod execute;
mod manifest;
mod template;

use clap::Parser;

use crate::{command::Cli, execute::Executable};

fn main() {
    let cli = Cli::parse();

    if let Err(err) = cli.execute() {
        println!("{}", err);
    }
}
