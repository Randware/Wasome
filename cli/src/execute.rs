use crate::{
    command::{BuildArgs, CheckArgs, Cli, Command, FmtArgs, NewArgs},
    error::{CliError, CliResult, ManifestError},
    manifest::Manifest,
};

pub(crate) trait Executable {
    fn execute(self) -> CliResult<()>;
}

impl Executable for Cli {
    fn execute(self) -> CliResult<()> {
        self.command.execute()
    }
}

impl Executable for Command {
    fn execute(self) -> CliResult<()> {
        match self {
            Command::Check(args) => args.execute(),
            Command::Build(args) => args.execute(),
            Command::New(args) => args.execute(),
            Command::Fmt(args) => args.execute(),
        }
    }
}

impl Executable for CheckArgs {
    fn execute(self) -> CliResult<()> {
        let path = self.path.canonicalize()?;

        let (manifest, manifest_path) = Manifest::discover(path)?;

        println!(
            "Checking project at {}",
            manifest_path.parent().unwrap().display()
        );

        Ok(())
    }
}

impl Executable for BuildArgs {
    fn execute(self) -> CliResult<()> {
        let path = self.path.canonicalize()?;

        let (manifest, manifest_path) = Manifest::discover(path)?;

        println!(
            "Compiling project at {}",
            manifest_path.parent().unwrap().display()
        );

        Ok(())
    }
}

impl Executable for NewArgs {
    fn execute(self) -> CliResult<()> {
        let path = self.path.canonicalize()?;

        //  NOTE: This does NOT allow creating projects inside other projects, maybe we want this
        //  to be allowed in the future
        if Manifest::find(&path).is_ok() {
            return Err(CliError::Manifest(ManifestError::AlreadyFound));
        }

        println!("Creating new project at {}", path.display());

        Ok(())
    }
}

impl Executable for FmtArgs {
    fn execute(self) -> CliResult<()> {
        let path = self.path.canonicalize()?;

        let (manifest, manifest_path) = Manifest::discover(path)?;

        println!(
            "Formatting project at {}",
            manifest_path.parent().unwrap().display()
        );

        Ok(())
    }
}
