use std::path::{Path, PathBuf};

use driver::program_information::ProgramInformation;
use source::SourceMap;

use crate::{
    dependencies::DependencyResolver,
    error::{CliError, CliResult, ManifestError},
    manifest::{self, Manifest},
};

pub struct Workspace {
    pub source: SourceMap,
    pub info: ProgramInformation,
}

impl Workspace {
    pub fn load(path: impl AsRef<Path>) -> CliResult<Self> {
        let manifest_path = Manifest::find(path).map_err(CliError::Manifest)?;
        let root = manifest_path
            .parent()
            .expect("Manifest should always have a parent directory")
            .to_path_buf();

        let mut source = SourceMap::with_default(root.clone());

        // We wrap everything past here in a closure so we can catch ManifestError
        // and attach the SourceMap to it before returning CliError
        let mut build_workspace = || -> Result<ProgramInformation, ManifestError> {
            let file_id = source.load_file(manifest::MANIFEST_FILE)?;
            let content = source.get_file(&file_id).unwrap().content();

            let manifest =
                Manifest::parse(content).map_err(|e| ManifestError::Parse(e, file_id))?;

            let entry_file = find_entry_file(&root, &manifest)?;

            let mut projects =
                DependencyResolver::new(root.clone()).resolve_all(&manifest, &mut source)?;

            projects.push(driver::program_information::Project::new(
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

            info.ok_or_else(|| ManifestError::NoEntry(manifest.project.name.clone()))
        };

        match build_workspace() {
            Ok(info) => Ok(Self { source, info }),
            Err(ManifestError::Parse(err, file_id)) => {
                Err(CliError::ManifestParse(err, source, file_id))
            }
            Err(err) => Err(CliError::Manifest(err)),
        }
    }
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
        .strip_prefix(root)
        .unwrap_or(&entry_file)
        .to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{manifest::MANIFEST_FILE, template::Template};
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_load_binary_workspace() {
        let dir = tempdir().unwrap();
        Template::bin("bin_project").write(dir.path()).unwrap();

        let workspace = Workspace::load(dir.path()).unwrap();

        assert_eq!(workspace.info.name(), "bin_project");
        assert_eq!(workspace.info.main_file(), Path::new("src/main.waso"));
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

        let src_dir = dir.path().join("src");
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
