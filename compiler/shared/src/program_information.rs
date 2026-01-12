use std::path::{Path, PathBuf};

/// Information about a program being compiled
pub struct ProgramInformation {
    /// The name of the program
    name: String,
    /// The path of the program root
    path: PathBuf,
    /// All dependencies
    dependencies: Vec<Dependency>,
    /// The main file
    ///
    /// Relative to the program root
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

/// A dependency
pub struct Dependency {
    /// The name of the dependency
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