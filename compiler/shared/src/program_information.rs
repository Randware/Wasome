use std::path::{Path, PathBuf};
use relative_path::{RelativePath, RelativePathBuf};

pub struct ProgramInformation {
    name: String,
    path: PathBuf,
    dependencies: Vec<Dependency>
}

impl ProgramInformation {
    pub fn new(name: String, path: PathBuf, dependencies: Vec<Dependency>) -> Self {
        Self { name, path, dependencies }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn dependencies(&self) -> &Vec<Dependency> {
        &self.dependencies
    }
}
pub struct Dependency {
    name: String,
    path: RelativePathBuf
}

impl Dependency {
    pub fn new(name: String, path: RelativePathBuf) -> Self {
        Self { name, path }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &RelativePath {
        &self.path
    }
}