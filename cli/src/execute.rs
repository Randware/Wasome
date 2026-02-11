use crate::command::{BuildArgs, CheckArgs, Cli, Command, FmtArgs, NewArgs};
use anyhow::Result;

pub(crate) trait Executable {
    fn execute(self) -> Result<()>;
}

impl Executable for Cli {
    fn execute(self) -> Result<()> {
        self.command.execute()
    }
}

impl Executable for Command {
    fn execute(self) -> Result<()> {
        match self {
            Command::Check(args) => args.execute(),
            Command::Build(args) => args.execute(),
            Command::New(args) => args.execute(),
            Command::Fmt(args) => args.execute(),
        }
    }
}

impl Executable for CheckArgs {
    fn execute(self) -> Result<()> {
        let path = self.path.canonicalize()?;

        println!("Checking project at {}", path.display());

        Ok(())
    }
}

impl Executable for BuildArgs {
    fn execute(self) -> Result<()> {
        let path = self.path.canonicalize()?;

        println!("Compiling project at {}", path.display());

        Ok(())
    }
}

impl Executable for NewArgs {
    fn execute(self) -> Result<()> {
        let path = self.path.canonicalize()?;

        println!("Creating new project at {}", path.display());

        Ok(())
    }
}

impl Executable for FmtArgs {
    fn execute(self) -> Result<()> {
        let path = self.path.canonicalize()?;

        println!("Formatting project at {}", path.display());

        Ok(())
    }
}
