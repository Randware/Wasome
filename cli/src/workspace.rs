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

        let mut source = SourceMap::new(root.clone());

        // We wrap everything past here in a closure so we can catch ManifestError
        // and attach the SourceMap to it before returning CliError.
        let mut build_workspace = || -> Result<ProgramInformation, ManifestError> {
            let file_id = source.load_file(manifest::MANIFEST_FILE)?;
            let content = source.get_file(&file_id).unwrap().content();

            let manifest = match Manifest::parse(content) {
                Ok(m) => m,
                Err(e) => return Err(ManifestError::Parse(e, file_id)),
            };

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

            let entry_file = entry_file
                .strip_prefix(&root)
                .unwrap_or(&entry_file)
                .to_path_buf();

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
