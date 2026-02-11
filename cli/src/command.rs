use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueHint};

#[derive(Parser)]
#[command(version, about = "The Wasome programming language toolchain")]
#[command(propagate_version = true)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) command: Command,
}

#[derive(Args, Debug)]
pub(crate) struct CheckArgs {
    #[arg(help = "Path of project to check [default: Current directory]", value_hint = ValueHint::DirPath, default_value = ".", hide_default_value = true)]
    pub(crate) path: PathBuf,
}

#[derive(Args, Debug)]
pub(crate) struct BuildArgs {
    #[arg(help = "Path of project to compile [default: Current directory]", value_hint = ValueHint::DirPath, default_value = ".", hide_default_value = true)]
    pub(crate) path: PathBuf,
}

#[derive(Args, Debug)]
pub(crate) struct NewArgs {
    #[arg(help = "Where to initialize new project [default: Current directory]", value_hint = ValueHint::DirPath, default_value = ".", hide_default_value = true)]
    pub(crate) path: PathBuf,
}

#[derive(Args, Debug)]
pub(crate) struct FmtArgs {
    #[arg(help = "Path of project to format [default: Current directory]", value_hint = ValueHint::DirPath, default_value = ".", hide_default_value = true)]
    pub(crate) path: PathBuf,
}

#[derive(Subcommand)]
pub(crate) enum Command {
    #[command(about = "Check the project source for issues")]
    Check(CheckArgs),
    #[command(about = "Compile the project")]
    Build(BuildArgs),
    #[command(about = "Initialize new project")]
    New(NewArgs),
    #[command(about = "Format the project source")]
    Fmt(FmtArgs),
}
