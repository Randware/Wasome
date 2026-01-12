use std::path::{Path, PathBuf};

pub struct ProgramInformation {
    name: String,
    path: PathBuf,
    dependencies: Vec<Dependency>,
    main_file: PathBuf
}

impl ProgramInformation {
    pub fn new(name: String, path: PathBuf, dependencies: Vec<Dependency>,
               main_file: PathBuf) -> Self {
        Self { name, path, dependencies, main_file }
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

    pub fn main_file(&self) -> &Path {
        &self.main_file
    }
}
pub struct Dependency {
    name: String,
    /// Relative to the program path
    path: PathBuf
}

impl Dependency {
    pub fn new(name: String, path: PathBuf) -> Self {
        Self { name, path }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}