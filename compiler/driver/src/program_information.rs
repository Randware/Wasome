use std::fmt::Debug;
use std::path::{Path, PathBuf};

pub trait LoadInformation: Debug {
    /// The name of the program
    #[must_use]
    fn name(&self) -> &str;
    /// The path of the program root
    #[must_use]
    fn path(&self) -> &Path;
    /// All projects
    ///
    /// Including the main project and dependencies
    #[must_use]
    fn projects(&self) -> &[Project];
}

pub trait BinaryProgramInformation: Debug {
    /// The project of the main file
    ///
    /// A project with this name must exist in `projects`
    #[must_use]
    fn main_file(&self) -> &Path;
    /// The main file
    ///
    /// Relative to the project root
    /// May not be empty
    #[must_use]
    fn main_project(&self) -> &str;
}

pub trait CompileInformation: Debug {
    #[must_use]
    fn opt_level(&self) -> OptLevel;
}

pub trait LoadBinaryProgramInformation: LoadInformation + BinaryProgramInformation {}
impl<T: LoadInformation + BinaryProgramInformation> LoadBinaryProgramInformation for T {}

pub trait FullProgramInformation: LoadBinaryProgramInformation + CompileInformation {}
impl<T: LoadBinaryProgramInformation + CompileInformation> FullProgramInformation for T {}

#[derive(Debug)]
pub struct ConcreteLoadInformation {
    name: String,
    path: PathBuf,
    projects: Vec<Project>,
}

impl ConcreteLoadInformation {
    #[must_use]
    pub fn new(name: String, path: PathBuf, projects: Vec<Project>) -> Self {
        Self {
            name,
            path,
            projects,
        }
    }
}

impl LoadInformation for ConcreteLoadInformation {
    fn name(&self) -> &str {
        &self.name
    }

    fn path(&self) -> &Path {
        &self.path
    }

    fn projects(&self) -> &[Project] {
        &self.projects
    }
}

#[derive(Debug)]
pub struct ConcreteBinaryProgramInformation {
    main_project: String,
    main_file: PathBuf,
}

impl ConcreteBinaryProgramInformation {
    pub fn new(main_project: String, main_file: PathBuf) -> Self {
        Self {
            main_project,
            main_file,
        }
    }
}
impl BinaryProgramInformation for ConcreteBinaryProgramInformation {
    fn main_file(&self) -> &Path {
        &self.main_file
    }

    fn main_project(&self) -> &str {
        &self.main_project
    }
}

#[derive(Debug)]
pub struct ConcreteCompileInformation {
    opt_level: OptLevel,
}

impl ConcreteCompileInformation {
    pub fn new(opt_level: OptLevel) -> Self {
        Self { opt_level }
    }
}
impl CompileInformation for ConcreteCompileInformation {
    fn opt_level(&self) -> OptLevel {
        self.opt_level
    }
}

#[derive(Debug)]
pub struct ConcreteLoadBinaryProgramInformation {
    load: ConcreteLoadInformation,
    binary: ConcreteBinaryProgramInformation,
}

impl ConcreteLoadBinaryProgramInformation {
    /// Tries to create a new instance
    ///
    /// # Errors
    ///
    /// - `main_file` is empty
    /// - `main_project` is not in `projects`
    pub fn new(
        load: ConcreteLoadInformation,
        binary: ConcreteBinaryProgramInformation,
    ) -> Option<Self> {
        let main_file_empty = binary.main_file().iter().count() == 0;
        let main_project_not_found = !load
            .projects()
            .iter()
            .any(|project| project.name() == binary.main_project());
        if main_file_empty || main_project_not_found {
            None
        } else {
            Some(Self { load, binary })
        }
    }
}

impl LoadInformation for ConcreteLoadBinaryProgramInformation {
    fn name(&self) -> &str {
        self.load.name()
    }

    fn path(&self) -> &Path {
        self.load.path()
    }

    fn projects(&self) -> &[Project] {
        self.load.projects()
    }
}

impl BinaryProgramInformation for ConcreteLoadBinaryProgramInformation {
    fn main_file(&self) -> &Path {
        self.binary.main_file()
    }

    fn main_project(&self) -> &str {
        self.binary.main_project()
    }
}

/// Information about a program being compiled
///
/// Intended to be passed to the parser driver to properly load and parse the program
#[derive(Debug)]
pub struct ProgramInformation {
    load_binary: ConcreteLoadBinaryProgramInformation,
    compile: ConcreteCompileInformation,
}

impl ProgramInformation {
    #[must_use]
    pub fn new(
        load_binary: ConcreteLoadBinaryProgramInformation,
        compile: ConcreteCompileInformation,
    ) -> Self {
        Self {
            load_binary,
            compile,
        }
    }
}

impl LoadInformation for ProgramInformation {
    fn name(&self) -> &str {
        self.load_binary.name()
    }

    fn path(&self) -> &Path {
        self.load_binary.path()
    }

    fn projects(&self) -> &[Project] {
        self.load_binary.projects()
    }
}

impl BinaryProgramInformation for ProgramInformation {
    fn main_file(&self) -> &Path {
        self.load_binary.main_file()
    }

    fn main_project(&self) -> &str {
        self.load_binary.main_project()
    }
}

impl CompileInformation for ProgramInformation {
    fn opt_level(&self) -> OptLevel {
        self.compile.opt_level()
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
    path: PathBuf,
}

impl Project {
    #[must_use]
    pub const fn new(name: String, path: PathBuf) -> Self {
        Self { name, path }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLevel {
    // -O0: No optimization. Fastest compile, best for debugging
    O0,
    // -O1: Basic optimizations. Good for speeding up test runs
    O1,
    // -O2: Standard release. Fast execution, reasonable compile time
    O2,
    // -O3: Max speed. Aggressive inlining/unrolling. Can bloat binary
    O3,
    // -Os: Optimize for size. Like O2, but restricts code bloat
    Os,
    // -Oz: Minimum size at all costs. Disables unrolling
    Oz,
}
