use std::path::Path;

use error::diagnostic::{Diagnostic, Level};

use crate::{
    command::{BuildArgs, CheckArgs, Cli, Command, FmtArgs, NewArgs},
    error::{CliError, CliResult, ManifestError},
    manifest::Manifest,
    template::Template,
    workspace::Workspace,
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

        let mut workspace = Workspace::load(&path)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!(
                "Checking project at {}",
                workspace.info.path().display()
            ))
            .build()
            .print()?;

        match driver::syntax_check(&workspace.info, &mut workspace.source) {
            Ok(_) => Diagnostic::builder()
                .level(Level::Info)
                .message("Check was successful")
                .build()
                .print()?,
            Err(d) => {
                d.print_snippets(&workspace.source)?;

                Diagnostic::builder()
                    .level(Level::Error)
                    .message("Check was not successful")
                    .build()
                    .print()?;

                return Err(CliError::CompilationFailed);
            }
        }

        Ok(())
    }
}

impl Executable for BuildArgs {
    fn execute(self) -> CliResult<()> {
        let path = self.path.canonicalize()?;

        let workspace = Workspace::load(&path)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!(
                "Compiling project at {}",
                workspace.info.path().display()
            ))
            .build()
            .print()?;

        // TODO: Compiling is not yet possible
        todo!();

        Ok(())
    }
}

impl Executable for NewArgs {
    fn execute(self) -> CliResult<()> {
        let path = if self.path.exists() {
            self.path.canonicalize()?
        } else {
            std::env::current_dir()?.join(&self.path)
        };

        let start = if path.exists() {
            path.as_path()
        } else {
            Path::new(".")
        };

        if Manifest::find(start).is_ok() {
            return Err(CliError::Manifest(ManifestError::AlreadyFound));
        }

        let name = path
            .file_name()
            .ok_or_else(|| CliError::Io(std::io::Error::from(std::io::ErrorKind::InvalidInput)))?
            .to_string_lossy();

        let template = match self.lib {
            true => Template::lib(&name),
            false => Template::bin(&name),
        };

        template.write(&path)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!("Created new project at {}", path.display()))
            .build()
            .print()?;

        Ok(())
    }
}

impl Executable for FmtArgs {
    fn execute(self) -> CliResult<()> {
        let path = self.path.canonicalize()?;

        let workspace = Workspace::load(&path)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!(
                "Formatting project at {}",
                workspace.info.path().display()
            ))
            .build()
            .print()?;

        // TODO: Formatting is not yet possible
        todo!();

        Ok(())
    }
}
