use std::path::{Path, PathBuf};

use driver::program_information::{ProgramInformation, Project};
use error::diagnostic::{Diagnostic, Level};
use source::SourceMap;

use crate::{
    command::{BuildArgs, CheckArgs, Cli, Command, FmtArgs, NewArgs},
    error::{CliError, CliResult, ManifestError},
    manifest::{self, Manifest},
    template::Template,
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

        let manifest_path = Manifest::find(&path)?;
        let root = manifest_path
            .parent()
            .expect("Manifest should always have a parent directory")
            .to_path_buf();

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!("Checking project at {}", root.display()))
            .build()
            .print()?;

        let mut source = SourceMap::new(root.clone());
        let file_id = source.load_file(manifest::MANIFEST_FILE)?;

        let content = source.get_file(&file_id).unwrap().content();

        let manifest = match Manifest::parse(content) {
            Ok(m) => m,
            Err(e) => {
                if let ManifestError::Parse(toml_err) = e {
                    return Err(CliError::ManifestParse(toml_err, source, file_id));
                }

                return Err(CliError::Manifest(e));
            }
        };

        let bin_file = root.join(manifest::BINARY_ENTRY_FILE);
        let lib_file = root.join(manifest::LIBRARY_ENTRY_FILE);

        let entry_file = match (bin_file.exists(), lib_file.exists()) {
            (true, true) => {
                return Err(CliError::Manifest(ManifestError::MultipleEntries(
                    manifest.project.name.clone(),
                )));
            }
            (true, false) => bin_file,
            (false, true) => {
                // NOTE: We cannot check libraries currently
                Diagnostic::builder()
                    .level(Level::Error)
                    .message("Cannot check library project")
                    .build()
                    .print()?;

                return Ok(());
            }
            (false, false) => {
                return Err(CliError::Manifest(ManifestError::NoEntry(
                    manifest.project.name.clone(),
                )));
            }
        };

        let entry_file = entry_file
            .strip_prefix(&root)
            .unwrap_or(&entry_file)
            .to_path_buf();

        let mut projects = match manifest.resolve_dependencies(&root) {
            Ok(p) => p,
            Err(e) => {
                if let ManifestError::Parse(toml_err) = e {
                    return Err(CliError::ManifestParse(toml_err, source, file_id));
                }

                return Err(CliError::Manifest(e));
            }
        };

        projects.push(Project::new(
            manifest.project.name.clone(),
            PathBuf::from("."),
        ));

        let info = ProgramInformation::new(
            manifest.project.name.clone(),
            root.clone(),
            projects,
            manifest.project.name.clone(),
            entry_file,
        );

        // If we cannot create a valid ProgramInformation, the entry file is empty
        let info = info.ok_or_else(|| {
            CliError::Manifest(ManifestError::NoEntry(manifest.project.name.clone()))
        })?;

        match driver::syntax_check(&info, &mut source) {
            Ok(_) => Diagnostic::builder()
                .level(Level::Info)
                .message("Check was successful")
                .build()
                .print()?,
            Err(d) => {
                d.print_snippets(&source)?;

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

        let (manifest, manifest_path) = Manifest::discover(path)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!(
                "Compiling project at {}",
                manifest_path.parent().unwrap().display()
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

        let (manifest, manifest_path) = Manifest::discover(path)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!(
                "Formatting project at {}",
                manifest_path.parent().unwrap().display()
            ))
            .build()
            .print()?;

        // TODO: Formatting is not yet possible
        todo!();

        Ok(())
    }
}
