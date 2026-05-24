use driver::program_information::LoadInformation;
use error::diagnostic::{Diagnostic, Level};
use std::path;

use crate::{
    command::{
        BuildArgs, CheckArgs, Cli, Command, FmtArgs, NewArgs, TargetArgs, TargetCommand,
        TargetListArgs,
    },
    error::{CliError, CliResult, ManifestError},
    manifest::{self},
    pipeline,
    stdlib::StdlibResolver,
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
            Command::Target(args) => args.execute(),
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
                workspace.info().path().display()
            ))
            .build()
            .print()?;

        pipeline::check(&mut workspace)?;

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
                workspace.info().path().display()
            ))
            .build()
            .print()?;

        let project_name = workspace.info().name().to_string();

        let target = self.target.as_deref().unwrap_or("runtime");
        let stdlib_paths = StdlibResolver::resolve(target, self.stdlib_path.as_deref())?;

        let output = pipeline::build(workspace, self.profile, &stdlib_paths, &self.link_files)?;

        let output_dir = path.join(manifest::OUTPUT_DIR);

        std::fs::create_dir_all(&output_dir)?;
        let output_file =
            output_dir.join(format!("{}.{}", project_name, manifest::OUTPUT_EXTENSION));
        std::fs::write(&output_file, &output.data)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!("Built {}", output_file.display()))
            .build()
            .print()?;

        Ok(())
    }
}

impl Executable for NewArgs {
    fn execute(self) -> CliResult<()> {
        let path = path::absolute(self.path)?;

        if path.join(manifest::MANIFEST_FILE).exists() {
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

        let mut workspace = Workspace::load(&path)?;

        Diagnostic::builder()
            .level(Level::Info)
            .message(format!(
                "Formatting project at {}",
                workspace.info().path().display()
            ))
            .build()
            .print()?;

        let program = pipeline::load(&mut workspace)?;

        pipeline::fmt(&mut workspace, &program)?;

        Ok(())
    }
}

impl Executable for TargetArgs {
    fn execute(self) -> CliResult<()> {
        match self.command {
            TargetCommand::List(args) => args.execute(),
        }
    }
}

impl Executable for TargetListArgs {
    fn execute(self) -> CliResult<()> {
        let targets = StdlibResolver::list_targets()?;

        if targets.is_empty() {
            Diagnostic::builder()
                .level(Level::Warning)
                .message("No targets found")
                .help("Make sure you installed the Wasome toolchain properly")
                .build()
                .print()?;
        } else {
            let mut list = String::from("Available targets:");
            for target in targets {
                list.push_str(&format!("\n  - {}", target));
            }
            Diagnostic::builder()
                .level(Level::Info)
                .message(list)
                .build()
                .print()?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs, path::PathBuf};
    use tempfile::tempdir;

    fn valid_fixture_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/valid")
    }

    fn invalid_fixture_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/invalid")
    }

    #[test]
    fn test_new_binary_project() {
        let dir = tempdir().unwrap();
        let target = dir.path().join("bin_project");

        let args = NewArgs {
            path: target.clone(),
            lib: false,
        };
        args.execute().unwrap();

        let template = Template::bin("bin_project");
        for (rel_path, content) in template.files {
            let disk_content = fs::read(target.join(rel_path)).unwrap();

            assert_eq!(disk_content, *content);
        }
    }

    #[test]
    fn test_new_library_project() {
        let dir = tempdir().unwrap();
        let target = dir.path().join("lib_project");

        let args = NewArgs {
            path: target.clone(),
            lib: true,
        };
        args.execute().unwrap();

        let template = Template::lib("lib_project");
        for (rel_path, content) in template.files {
            let disk_content = fs::read(target.join(rel_path)).unwrap();

            assert_eq!(disk_content, *content);
        }
    }

    #[test]
    fn test_new_already_existing_manifest() {
        let dir = tempdir().unwrap();
        let target = dir.path().join("existing");

        NewArgs {
            path: target.clone(),
            lib: false,
        }
        .execute()
        .unwrap();

        let err = NewArgs {
            path: target.clone(),
            lib: false,
        }
        .execute()
        .unwrap_err();

        assert!(matches!(
            err,
            CliError::Manifest(ManifestError::AlreadyFound)
        ));
    }

    #[test]
    fn test_check_valid_project() {
        let path = valid_fixture_path();
        let args = CheckArgs { path };
        args.execute().unwrap();
    }

    #[test]
    fn test_check_syntax_error() {
        let path = invalid_fixture_path();
        let args = CheckArgs { path };

        let err = args.execute().unwrap_err();

        assert!(matches!(err, CliError::CompilationFailed));
    }

    #[test]
    fn test_build_without_manifest() {
        let dir = tempdir().unwrap();
        let args = BuildArgs {
            path: dir.path().to_path_buf(),
            profile: Default::default(),
            stdlib_path: None,
            link_files: vec![],
            target: None,
        };

        let err = args.execute().unwrap_err();

        assert!(matches!(err, CliError::Manifest(ManifestError::NotFound)));
    }

    #[test]
    fn test_build_missing_stdlib() {
        let path = valid_fixture_path();
        let args = BuildArgs {
            path,
            profile: Default::default(),
            stdlib_path: None,
            link_files: vec![],
            target: Some("nonexistent_target".to_string()),
        };
        let err = args.execute().unwrap_err();
        assert!(matches!(err, CliError::TargetNotFound(_)));
    }

    #[test]
    fn test_fmt_valid_project() {
        let path = valid_fixture_path();
        let args = FmtArgs { path };
        args.execute().unwrap();
    }
}
