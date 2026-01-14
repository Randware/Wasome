use std::path::{Path, PathBuf};

/// Information about a program being compiled
#[derive(Debug)]
pub struct ProgramInformation {
    /// The name of the program
    name: String,
    /// The path of the program root
    path: PathBuf,
    /// All projects
    ///
    /// Including the main project and dependencies
    projects: Vec<Project>,
    /// The project of the main file
    main_project: String,
    /// The main file
    ///
    /// Relative to the project root
    main_file: PathBuf
}

impl ProgramInformation {
    pub fn new(name: String, path: PathBuf, dependencies: Vec<Project>,
               main_project: String, main_file: PathBuf) -> Self {
        Self { name, path, projects: dependencies, main_project, main_file }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn projects(&self) -> &[Project] {
        &self.projects
    }

    pub fn main_file(&self) -> &Path {
        &self.main_file
    }

    pub fn main_project(&self) -> &str {
        &self.main_project
    }
}

/// A project
///
/// Both dependencies and the main project are represented by this
#[derive(Debug)]
pub struct Project {
    /// The name of the project
    name: String,
    /// Relative to the program path
    path: PathBuf
}

impl Project {
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