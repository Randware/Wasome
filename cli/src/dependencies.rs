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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::manifest::MANIFEST_FILE;
    use std::{fs, path::Path};
    use tempfile::tempdir;

    fn write_manifest(dir: &Path, name: &str, version: &str, deps: &[(&str, &str)]) {
        let deps_section = if deps.is_empty() {
            String::new()
        } else {
            let entries: Vec<String> = deps.iter().map(|(n, v)| format!("{n} = \"{v}\"")).collect();
            format!("[dependencies]\n{}", entries.join("\n"))
        };

        let content = format!(
            r#"[project]
name = "{name}"
version = "{version}"
{deps_section}"#,
        );

        fs::write(dir.join(MANIFEST_FILE), content).unwrap();
    }

    fn create_dep(root: &Path, name: &str, version: &str, deps: &[(&str, &str)]) {
        let dep_dir = root.join("lib").join(version_str(name, version));
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, name, version, deps);
    }

    fn load_and_resolve(root: &Path) -> Result<Vec<Project>, ManifestError> {
        let mut source = SourceMap::with_default(root.to_path_buf());
        let manifest =
            Manifest::parse(&fs::read_to_string(root.join(MANIFEST_FILE)).unwrap()).unwrap();
        let resolver = DependencyResolver::new(root.to_path_buf());
        resolver.resolve_all(&manifest, &mut source)
    }

    #[test]
    fn test_locate_found_in_local_lib() {
        let root = tempdir().unwrap();
        let dep_dir = root.path().join("lib").join("math@1.0.0");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "math", "1.0.0", &[]);

        let resolver = DependencyResolver::new(root.path().to_path_buf());
        let result = resolver.locate("math", "1.0.0");

        assert!(result.is_some());
        assert_eq!(result.unwrap(), dep_dir);
    }

    #[test]
    fn test_locate_missing_dependency() {
        let root = tempdir().unwrap();
        let resolver = DependencyResolver::new(root.path().to_path_buf());

        assert!(resolver.locate("nonexistent", "1.0.0").is_none());
    }

    #[test]
    fn test_locate_folder_exists_but_no_manifest() {
        let root = tempdir().unwrap();
        let dep_dir = root.path().join("lib").join("math@1.0.0");
        fs::create_dir_all(&dep_dir).unwrap();

        let resolver = DependencyResolver::new(root.path().to_path_buf());

        assert!(resolver.locate("math", "1.0.0").is_none());
    }

    #[test]
    fn test_resolve_all_no_dependencies() {
        let root = tempdir().unwrap();
        write_manifest(root.path(), "my_app", "0.1.0", &[]);

        let projects = load_and_resolve(root.path()).unwrap();

        assert!(projects.is_empty());
    }

    #[test]
    fn test_resolve_all_single_dependency() {
        let root = tempdir().unwrap();
        create_dep(root.path(), "math", "1.0.0", &[]);
        write_manifest(root.path(), "my_app", "0.1.0", &[("math", "1.0.0")]);

        let projects = load_and_resolve(root.path()).unwrap();

        assert_eq!(projects.len(), 1);
        assert_eq!(projects[0].name(), "math");
    }

    #[test]
    fn test_resolve_all_multiple_siblings() {
        let root = tempdir().unwrap();
        create_dep(root.path(), "math", "1.0.0", &[]);
        create_dep(root.path(), "io", "2.0.0", &[]);
        create_dep(root.path(), "net", "0.5.0", &[]);
        write_manifest(
            root.path(),
            "my_app",
            "0.1.0",
            &[("math", "1.0.0"), ("io", "2.0.0"), ("net", "0.5.0")],
        );

        let projects = load_and_resolve(root.path()).unwrap();
        let names: Vec<&str> = projects.iter().map(|p| p.name()).collect();

        assert_eq!(projects.len(), 3);
        assert!(names.contains(&"math"));
        assert!(names.contains(&"io"));
        assert!(names.contains(&"net"));
    }

    #[test]
    fn test_resolve_all_transitive_dependencies() {
        let root = tempdir().unwrap();

        let math_dir = root.path().join("lib").join("math@1.0.0");
        fs::create_dir_all(&math_dir).unwrap();
        write_manifest(&math_dir, "math", "1.0.0", &[("core", "0.1.0")]);
        create_dep(&math_dir, "core", "0.1.0", &[]);

        write_manifest(root.path(), "my_app", "0.1.0", &[("math", "1.0.0")]);

        let projects = load_and_resolve(root.path()).unwrap();
        let names: Vec<&str> = projects.iter().map(|p| p.name()).collect();

        assert_eq!(projects.len(), 2);
        assert!(names.contains(&"math"));
        assert!(names.contains(&"core"));
    }

    #[test]
    fn test_resolve_all_deep_transitive_chain() {
        let root = tempdir().unwrap();

        let a_dir = root.path().join("lib").join("a@1.0.0");
        fs::create_dir_all(&a_dir).unwrap();
        write_manifest(&a_dir, "a", "1.0.0", &[("b", "1.0.0")]);

        let b_dir = a_dir.join("lib").join("b@1.0.0");
        fs::create_dir_all(&b_dir).unwrap();
        write_manifest(&b_dir, "b", "1.0.0", &[("c", "1.0.0")]);

        let c_dir = b_dir.join("lib").join("c@1.0.0");
        fs::create_dir_all(&c_dir).unwrap();
        write_manifest(&c_dir, "c", "1.0.0", &[]);

        write_manifest(root.path(), "my_app", "0.1.0", &[("a", "1.0.0")]);

        let projects = load_and_resolve(root.path()).unwrap();
        let names: Vec<&str> = projects.iter().map(|p| p.name()).collect();

        assert_eq!(projects.len(), 3);
        assert!(names.contains(&"a"));
        assert!(names.contains(&"b"));
        assert!(names.contains(&"c"));
    }

    #[test]
    fn test_resolve_all_missing_dependency_error() {
        let root = tempdir().unwrap();
        write_manifest(root.path(), "my_app", "0.1.0", &[("missing", "9.9.9")]);

        let err = load_and_resolve(root.path()).unwrap_err();

        match err {
            ManifestError::MissingDependency(dep_id, chain) => {
                assert_eq!(dep_id, "missing@9.9.9");
                assert!(chain.contains("my_app@0.1.0"));
            }
            other => panic!("Expected MissingDependency, got: {:?}", other),
        }
    }

    #[test]
    fn test_resolve_all_transitive_missing_dependency_error() {
        let root = tempdir().unwrap();

        let math_dir = root.path().join("lib").join("math@1.0.0");
        fs::create_dir_all(&math_dir).unwrap();
        write_manifest(&math_dir, "math", "1.0.0", &[("core", "0.1.0")]);

        write_manifest(root.path(), "my_app", "0.1.0", &[("math", "1.0.0")]);

        let err = load_and_resolve(root.path()).unwrap_err();

        match err {
            ManifestError::MissingDependency(dep_id, chain) => {
                assert_eq!(dep_id, "core@0.1.0");
                assert!(chain.contains("my_app@0.1.0"));
                assert!(chain.contains("math@1.0.0"));
            }
            other => panic!("Expected MissingDependency, got: {:?}", other),
        }
    }

    #[test]
    fn test_resolve_all_deep_transitive_missing_error() {
        let root = tempdir().unwrap();

        let a_dir = root.path().join("lib").join("a@1.0.0");
        fs::create_dir_all(&a_dir).unwrap();
        write_manifest(&a_dir, "a", "1.0.0", &[("b", "1.0.0")]);

        let b_dir = a_dir.join("lib").join("b@1.0.0");
        fs::create_dir_all(&b_dir).unwrap();
        write_manifest(&b_dir, "b", "1.0.0", &[("ghost", "3.0.0")]);

        write_manifest(root.path(), "my_app", "0.1.0", &[("a", "1.0.0")]);

        let err = load_and_resolve(root.path()).unwrap_err();

        match err {
            ManifestError::MissingDependency(dep_id, chain) => {
                assert_eq!(dep_id, "ghost@3.0.0");
                assert!(chain.contains("my_app@0.1.0"));
                assert!(chain.contains("a@1.0.0"));
                assert!(chain.contains("b@1.0.0"));
            }
            other => panic!("Expected MissingDependency, got: {:?}", other),
        }
    }
}
