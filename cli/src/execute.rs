use std::path::Path;

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
        let root = manifest_path.parent().unwrap();

        let mut source = SourceMap::new(root.to_path_buf());

        let file_id = source.load_file(crate::manifest::MANIFEST_NAME)?;

        let content = source.get_file(&file_id).unwrap().content();

        let manifest = match Manifest::parse(content) {
            Ok(m) => m,
            Err(e) => match e {
                ManifestError::Parse(toml_err) => {
                    return Err(CliError::ManifestParse(toml_err, source, file_id));
                }
                _ => return Err(CliError::Manifest(e)),
            },
        };

        //  TODO: Call driver with required information

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

        Ok(())
    }
}
