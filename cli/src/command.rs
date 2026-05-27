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
    #[arg(
        long = "std",
        help = "Provide a custom standard library location",
        value_hint = ValueHint::DirPath
    )]
    pub(crate) stdlib_path: Option<PathBuf>,
    #[arg(
        long = "link",
        help = "Provide additional linking objects (.o or .a files)",
        value_hint = ValueHint::FilePath
    )]
    pub(crate) link_files: Vec<PathBuf>,
    #[arg(
        long,
        help = "Compilation target (maps to a standard library implementation)"
    )]
    pub(crate) target: Option<String>,
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

#[cfg(feature = "runtime")]
#[derive(Args, Debug)]
pub(crate) struct RunArgs {
    #[arg(help = "Path of project to build and run, or path to a compiled WASM file [default: Current directory]", value_hint = ValueHint::AnyPath, default_value = ".", hide_default_value = true)]
    pub(crate) path: PathBuf,
    #[arg(long, help = "Allow access to read/write directories (e.g. '.', '/tmp')", value_name = "DIR")]
    pub(crate) dir: Vec<String>,
    #[arg(long, help = "Pass environment variables to the program (e.g. KEY=VALUE)", value_name = "KEY=VALUE")]
    pub(crate) env: Vec<String>,
    #[arg(last = true, help = "Arguments to pass to the WebAssembly program")]
    pub(crate) args: Vec<String>,
}

#[derive(Args, Debug)]
pub(crate) struct TargetArgs {
    #[command(subcommand)]
    pub(crate) command: TargetCommand,
}

#[derive(Subcommand, Debug)]
pub(crate) enum TargetCommand {
    #[command(about = "List available targets")]
    List(TargetListArgs),
}

#[derive(Args, Debug)]
pub(crate) struct TargetListArgs {}

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
    #[cfg(feature = "runtime")]
    #[command(about = "Build and run the project, or run a compiled WASM file")]
    Run(RunArgs),
    #[command(about = "Manage compilation targets")]
    Target(TargetArgs),
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

impl Profile {
    pub(crate) fn to_opt_level(self) -> codegen::OptLevel {
        match self {
            Self::Debug => codegen::OptLevel::O0,
            Self::Basic => codegen::OptLevel::O1,
            Self::Default => codegen::OptLevel::O2,
            Self::Max => codegen::OptLevel::O3,
            Self::Size => codegen::OptLevel::Os,
            Self::SizeMin => codegen::OptLevel::Oz,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[test]
    #[cfg(not(feature = "runtime"))]
    fn test_run_command_disabled_without_runtime_feature() {
        // When the runtime feature is disabled, parsing "run" should fail
        // because the Run variant is compiled out.
        let result = Cli::try_parse_from(vec!["waso", "run"]);
        match result {
            Err(e) => assert_eq!(e.kind(), clap::error::ErrorKind::InvalidSubcommand),
            Ok(_) => panic!("Expected error due to invalid subcommand"),
        }
    }
    
    #[test]
    #[cfg(feature = "runtime")]
    fn test_run_command_enabled_with_runtime_feature() {
        // When the runtime feature is enabled, parsing "run" should succeed 
        // if valid arguments are passed.
        let result = Cli::try_parse_from(vec!["waso", "run"]);
        assert!(result.is_ok());
    }
}
