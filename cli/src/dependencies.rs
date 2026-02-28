use std::path::PathBuf;

pub struct DependencyResolver {
    project_root: PathBuf,
    // NOTE: We could add a global cache location here later
}

impl DependencyResolver {
    pub fn new(project_root: PathBuf) -> Self {
        Self { project_root }
    }

    /// Takes a dependency name and version, and figures out where it lives
    pub fn locate(&self, name: &str, version: &str) -> Option<PathBuf> {
        let folder_name = format!("{}@{}", name, version);

        // Try local project "lib" folder first (local dependencies have higher importance)
        let local_path = self.project_root.join("lib").join(&folder_name);

        if local_path.join(crate::manifest::MANIFEST_FILE).exists() {
            return Some(local_path);
        }

        // NOTE: We could check a global cache here later

        None
    }
}
