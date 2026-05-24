use std::path::{Path, PathBuf};

use codegen::OptLevel;
use driver::program_information::{
    BinaryProgramInformation, ConcreteBinaryProgramInformation, ConcreteCompileInformation,
    ConcreteLoadBinaryProgramInformation, ConcreteLoadInformation, LoadInformation,
    ProgramInformation, Project,
};
use source::SourceMap;

use crate::{
    dependencies::DependencyResolver,
    error::{CliError, CliResult, ManifestError},
    manifest::{self, Manifest},
};

pub(crate) struct Workspace {
    pub source: SourceMap,
    load: ConcreteLoadInformation,
    binary: ConcreteBinaryProgramInformation,
}

impl Workspace {
    /// Provides a combined view for pipelines that only need load + binary info
    /// (e.g. syntax checking, formatting).
    pub fn info(&self) -> ConcreteLoadBinaryProgramInformation {
        ConcreteLoadBinaryProgramInformation::new(
            ConcreteLoadInformation::new(
                self.load.name().to_string(),
                self.load.path().to_path_buf(),
                clone_projects(self.load.projects()),
            ),
            ConcreteBinaryProgramInformation::new(
                self.binary.main_project().to_string(),
                self.binary.main_file().to_path_buf(),
            ),
        )
        .expect("Workspace was validated at load time")
    }

    /// Builds a `ProgramInformation` for compilation, incorporating the opt level
    /// and any additional projects (e.g. stdlib wasome sources).
    pub fn into_program_info(
        self,
        opt_level: OptLevel,
        extra_projects: Vec<Project>,
    ) -> (ProgramInformation, SourceMap) {
        let mut projects = clone_projects(self.load.projects());
        projects.extend(extra_projects);

        let load = ConcreteLoadInformation::new(
            self.load.name().to_string(),
            self.load.path().to_path_buf(),
            projects,
        );

        let binary = ConcreteBinaryProgramInformation::new(
            self.binary.main_project().to_string(),
            self.binary.main_file().to_path_buf(),
        );

        let load_binary = ConcreteLoadBinaryProgramInformation::new(load, binary)
            .expect("Workspace was validated at load time");

        let compile = ConcreteCompileInformation::new(opt_level);
        let info = ProgramInformation::new(load_binary, compile);

        (info, self.source)
    }

    pub fn load(path: impl AsRef<Path>) -> CliResult<Self> {
        let manifest_path = Manifest::find(path).map_err(CliError::Manifest)?;
        let root = manifest_path
            .parent()
            .expect("Manifest should always have a parent directory")
            .to_path_buf();

        let mut source = SourceMap::with_default(root.clone());

        // We wrap everything past here in a closure so we can catch ManifestError
        // and attach the SourceMap to it before returning CliError
        let mut build_workspace =
            || -> Result<(ConcreteLoadInformation, ConcreteBinaryProgramInformation), ManifestError> {
                let file_id = source.load_file(manifest::MANIFEST_FILE)?;
                let content = source.get_file(&file_id).unwrap().content();

                let manifest =
                    Manifest::parse(content).map_err(|e| ManifestError::Parse(e, file_id))?;

                let entry_file = find_entry_file(&root, &manifest)?;

                let mut projects =
                    DependencyResolver::new(root.clone()).resolve_all(&manifest, &mut source)?;

                projects.push(Project::new(
                    manifest.project.name.clone(),
                    PathBuf::from(manifest::SRC_DIR),
                ));

                let load = ConcreteLoadInformation::new(
                    manifest.project.name.clone(),
                    root.clone(),
                    projects,
                );

                let binary = ConcreteBinaryProgramInformation::new(
                    manifest.project.name.clone(),
                    entry_file,
                );

                // Validate the combination
                ConcreteLoadBinaryProgramInformation::new(
                    ConcreteLoadInformation::new(
                        load.name().to_string(),
                        load.path().to_path_buf(),
                        clone_projects(load.projects()),
                    ),
                    ConcreteBinaryProgramInformation::new(
                        binary.main_project().to_string(),
                        binary.main_file().to_path_buf(),
                    ),
                )
                .ok_or_else(|| ManifestError::NoEntry(load.name().to_string()))?;

                Ok((load, binary))
            };

        match build_workspace() {
            Ok((load, binary)) => Ok(Self { source, load, binary }),
            Err(ManifestError::Parse(err, file_id)) => {
                Err(CliError::ManifestParse(err, source, file_id))
            }
            Err(err) => Err(CliError::Manifest(err)),
        }
    }
}

/// Reconstructs a `Vec<Project>` from a slice, since `Project` does not implement `Clone`.
fn clone_projects(projects: &[Project]) -> Vec<Project> {
    projects
        .iter()
        .map(|p| Project::new(p.name().to_string(), p.path().to_path_buf()))
        .collect()
}

fn find_entry_file(root: &Path, manifest: &Manifest) -> Result<PathBuf, ManifestError> {
    let bin_file = root.join(manifest::BINARY_ENTRY_FILE);
    let lib_file = root.join(manifest::LIBRARY_ENTRY_FILE);

    let entry_file = match (bin_file.exists(), lib_file.exists()) {
        (true, true) => {
            return Err(ManifestError::MultipleEntries(
                manifest.project.name.clone(),
            ));
        }
        (true, false) => bin_file,
        (false, true) => {
            return Err(ManifestError::LibraryCheckUnsupported);
        }
        (false, false) => {
            return Err(ManifestError::NoEntry(manifest.project.name.clone()));
        }
    };

    Ok(entry_file
        .strip_prefix(root.join(manifest::SRC_DIR))
        .unwrap_or(&entry_file)
        .to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{manifest::MANIFEST_FILE, template::Template};
    use driver::program_information::{BinaryProgramInformation, LoadInformation};
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_load_binary_workspace() {
        let dir = tempdir().unwrap();
        Template::bin("bin_project").write(dir.path()).unwrap();

        let workspace = Workspace::load(dir.path()).unwrap();
        let info = workspace.info();

        assert_eq!(info.name(), "bin_project");
        assert_eq!(info.main_file(), Path::new("main.waso"));
    }

    #[test]
    fn test_load_no_entry_error() {
        let dir = tempdir().unwrap();
        let manifest_path = dir.path().join(MANIFEST_FILE);
        fs::write(
            &manifest_path,
            r#"
            [project]
            name = "bin_project"
            version = "0.1.0"
        "#,
        )
        .unwrap();

        let err = match Workspace::load(dir.path()) {
            Err(e) => e,
            Ok(_) => panic!("Expected error"),
        };
        match err {
            CliError::Manifest(ManifestError::NoEntry(name)) => assert_eq!(name, "bin_project"),
            _ => panic!("Expected NoEntry error"),
        }
    }

    #[test]
    fn test_load_multiple_entries_error() {
        let dir = tempdir().unwrap();
        Template::bin("dual").write(dir.path()).unwrap();

        let src_dir = dir.path().join(manifest::SRC_DIR);
        fs::write(src_dir.join("lib.waso"), "fn lib() {}").unwrap();

        let err = match Workspace::load(dir.path()) {
            Err(e) => e,
            Ok(_) => panic!("Expected error"),
        };
        match err {
            CliError::Manifest(ManifestError::MultipleEntries(name)) => assert_eq!(name, "dual"),
            _ => panic!("Expected MultipleEntries error"),
        }
    }
}
