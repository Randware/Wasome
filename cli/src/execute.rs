use std::path;

use error::diagnostic::{Diagnostic, Level};

use crate::{
    command::{BuildArgs, CheckArgs, Cli, Command, FmtArgs, NewArgs},
    error::{CliError, CliResult, ManifestError},
    manifest::{self},
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
        };

        let err = args.execute().unwrap_err();

        assert!(matches!(err, CliError::Manifest(ManifestError::NotFound)));
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_build_panics() {
        let path = valid_fixture_path();
        let args = BuildArgs { path };
        let _ = args.execute();
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_fmt_panics() {
        let path = valid_fixture_path();
        let args = FmtArgs { path };
        let _ = args.execute();
    }
}
