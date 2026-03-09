use std::path::PathBuf;

use driver::program_information::Project;
use source::SourceMap;

use crate::{
    error::ManifestError,
    manifest::{self, Manifest},
};

pub struct DependencyResolver {
    project_root: PathBuf,
    // NOTE: We could add a global cache location here later
}

impl DependencyResolver {
    pub fn new(project_root: PathBuf) -> Self {
        Self { project_root }
    }

    /// Recursively resolve all dependencies defined in the given manifest.
    pub fn resolve_all(
        &self,
        manifest: &Manifest,
        source: &mut SourceMap,
    ) -> Result<Vec<Project>, ManifestError> {
        let mut resolved_projects = Vec::new();

        let initial_chain = vec![version_str(
            &manifest.project.name,
            &manifest.project.version,
        )];

        self.resolve_recursive(manifest, source, &mut resolved_projects, initial_chain)?;

        Ok(resolved_projects)
    }

    /// Takes a dependency name and version, and figures out where it lives
    pub fn locate(&self, name: &str, version: &str) -> Option<PathBuf> {
        let folder_name = version_str(name, version);

        // Try local project "lib" folder first (local dependencies have higher importance)
        let local_path = self
            .project_root
            .join(manifest::LIB_PATH)
            .join(&folder_name);

        if local_path.join(crate::manifest::MANIFEST_FILE).exists() {
            return Some(local_path);
        }

        // NOTE: We could check a global cache here later

        None
    }

    /// Internal recursive helper with versioned stack trace
    fn resolve_recursive(
        &self,
        manifest: &Manifest,
        source: &mut SourceMap,
        acc: &mut Vec<Project>,
        chain: Vec<String>,
    ) -> Result<(), ManifestError> {
        let deps = match &manifest.dependencies {
            Some(d) => d,
            None => return Ok(()),
        };

        for (name, version) in deps {
            let dep_id = version_str(name, version);

            let dep_path = match self.locate(name, version) {
                Some(path) => path,
                None => {
                    let chain_display = chain.join("/");
                    return Err(ManifestError::MissingDependency(
                        dep_id.clone(),
                        chain_display,
                    ));
                }
            };

            let file_id = source.load_file(dep_path.join(manifest::MANIFEST_FILE))?;
            let content = source.get_file(&file_id).unwrap().content();

            let dep_manifest = match Manifest::parse(content) {
                Ok(m) => m,
                Err(e) => return Err(ManifestError::Parse(e, file_id)),
            };

            let dep_root_path = dep_path
                .strip_prefix(&self.project_root)
                .unwrap_or(&dep_path)
                .to_path_buf();

            acc.push(Project::new(
                dep_manifest.project.name.clone(),
                dep_root_path.clone(),
            ));

            let mut next_chain = chain.clone();
            next_chain.push(version_str(
                &dep_manifest.project.name,
                &dep_manifest.project.version,
            ));

            let resolver = DependencyResolver::new(dep_path.clone());
            resolver.resolve_recursive(&dep_manifest, source, acc, next_chain)?;
        }

        Ok(())
    }
}

/// Helper for constructing version strings for dependencies
fn version_str(name: impl ToString, version: impl ToString) -> String {
    format!("{}@{}", name.to_string(), version.to_string())
}
