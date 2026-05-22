use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum, ValueHint};

#[derive(Parser)]
#[command(
    name = "waso",
    version,
    about = "The Wasome programming language toolchain"
)]
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
    #[arg(
        long,
        help = "Optimization profile for compilation",
        default_value = "default",
        value_enum,
        ignore_case = true
    )]
    pub(crate) profile: Profile,
}

#[derive(Args, Debug)]
pub(crate) struct NewArgs {
    #[arg(help = "Where to initialize new project [default: Current directory]", value_hint = ValueHint::DirPath, default_value = ".", hide_default_value = true)]
    pub(crate) path: PathBuf,
    #[arg(long, help = "Initialize a library project")]
    pub(crate) lib: bool,
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

#[derive(ValueEnum, Clone, Copy, Debug, PartialEq, Eq, Default)]
pub(crate) enum Profile {
    #[value(
        name = "debug",
        alias = "O0",
        help = "(O0) No optimization, lowest compile time, best for debugging"
    )]
    Debug,
    #[value(
        name = "basic",
        alias = "O1",
        help = "(O1) Basic optimizations, no significant compile time cost"
    )]
    Basic,
    #[default]
    #[value(
        name = "default",
        alias = "O2",
        help = "(O2) Fast execution, good compile time, recommended in most cases"
    )]
    Default,
    #[value(
        name = "max",
        alias = "O3",
        help = "(O3) Maximum speed, significantly increased binary size"
    )]
    Max,
    #[value(
        name = "size",
        alias = "Os",
        help = "(Os) Optimize for binary size, no significant performance cost"
    )]
    Size,
    #[value(
        name = "size-min",
        alias = "Oz",
        help = "(Oz) Minimum binary size at all costs, significant performance cost"
    )]
    SizeMin,
}
