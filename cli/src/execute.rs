use std::path::{Path, PathBuf};

use driver::program_information::{ProgramInformation, Project};
use source::SourceMap;

use crate::{
    command::{BuildArgs, CheckArgs, Cli, Command, FmtArgs, NewArgs},
    error::{CliError, CliResult, ManifestError},
    manifest::Manifest,
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
        let root = manifest_path.parent().unwrap().to_path_buf();

        let mut source = SourceMap::new(root.clone());
        let file_id = source.load_file(crate::manifest::MANIFEST_NAME)?;

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

        if manifest.is_library() {
            // NOTE: We cannot check libraries for now, since we don't have an entry point
            println!("Skipping check for library project (no entry point)");
            return Ok(());
        }

        let mut projects = manifest.resolve_dependencies(&root)?;

        projects.push(Project::new(
            manifest.project.name.clone(),
            PathBuf::from("."),
        ));

        let info = ProgramInformation::new(
            manifest.project.name.clone(),
            root.clone(),
            projects,
            manifest.project.name.clone(),
            PathBuf::from(manifest.bin.unwrap().entry),
        );

        let info = info.ok_or_else(|| {
            // TODO: Maybe define separate error for this
            CliError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Failed to create program information: Entry file invalid",
            ))
        })?;

        match driver::syntax_check(&info, &mut source) {
            Some(_) => println!(
                "Check for project '{}' was successful",
                manifest.project.name
            ),
            None => println!(
                "Check for project '{}' was NOT successful",
                manifest.project.name
            ),
        }

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

        println!("Created new project at {}", path.display());

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

        // TODO: Formatting is not yet possible
        todo!();

        Ok(())
    }
}
