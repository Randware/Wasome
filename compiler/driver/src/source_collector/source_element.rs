use ordered_hash_map::OrderedHashMap;
use source::types::FileID;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(PartialEq, Eq, Debug)]
pub struct WasomeProgram {
    location: WasomeSourceElementLocation,
    projects: OrderedHashMap<String, WasomeSourceDirectory>,
}

impl WasomeProgram {
    pub fn new(
        location: WasomeSourceElementLocation,
        projects: OrderedHashMap<String, WasomeSourceDirectory>,
    ) -> Self {
        Self { location, projects }
    }

    pub fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }

    pub fn projects(&self) -> &OrderedHashMap<String, WasomeSourceDirectory> {
        &self.projects
    }

    pub fn destructure(
        self,
    ) -> (
        WasomeSourceElementLocation,
        OrderedHashMap<String, WasomeSourceDirectory>,
    ) {
        (self.location, self.projects)
    }
}

impl HasWasomeSourceElementLocation for WasomeProgram {
    fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct WasomeSourceDirectory {
    location: WasomeSourceElementLocation,
    subdirs: Vec<WasomeSourceDirectory>,
    files: Vec<WasomeSourceFile>,
}

impl WasomeSourceDirectory {
    pub fn new(
        location: WasomeSourceElementLocation,
        subdirs: Vec<WasomeSourceDirectory>,
        files: Vec<WasomeSourceFile>,
    ) -> Result<Self, WasomeSourceDirectoryCreationError> {
        if let Some(dup) = duplicate_wasome_source_elements(subdirs.iter()) {
            return Err(WasomeSourceDirectoryCreationError::DuplicateDirectoryNames(
                dup,
            ));
        }
        if let Some(dup) = duplicate_wasome_source_elements(files.iter()) {
            return Err(WasomeSourceDirectoryCreationError::DuplicateFileNames(dup));
        }
        Ok(Self {
            location,
            subdirs,
            files,
        })
    }

    pub fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }

    pub fn subdirs(&self) -> &[WasomeSourceDirectory] {
        &self.subdirs
    }

    pub fn files(&self) -> &[WasomeSourceFile] {
        &self.files
    }

    pub fn destructure(
        self,
    ) -> (
        WasomeSourceElementLocation,
        Vec<WasomeSourceDirectory>,
        Vec<WasomeSourceFile>,
    ) {
        (self.location, self.subdirs, self.files)
    }
}

impl HasWasomeSourceElementLocation for WasomeSourceDirectory {
    fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }
}

#[derive(Debug)]
pub enum WasomeSourceDirectoryCreationError {
    DuplicateFileNames(String),
    DuplicateDirectoryNames(String),
}
#[derive(PartialEq, Eq, Debug)]
pub struct WasomeSourceFile {
    location: WasomeSourceElementLocation,
    file: FileID,
}

impl WasomeSourceFile {
    pub fn new(location: WasomeSourceElementLocation, file: FileID) -> Self {
        Self { location, file }
    }

    pub fn file(&self) -> FileID {
        self.file
    }

    pub fn destructure(self) -> (WasomeSourceElementLocation, FileID) {
        (self.location, self.file)
    }
}

impl HasWasomeSourceElementLocation for WasomeSourceFile {
    fn location(&self) -> &WasomeSourceElementLocation {
        &self.location
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct WasomeSourceElementLocation {
    name: String,
    // Relative to the program root
    path: PathBuf,
}

impl WasomeSourceElementLocation {
    pub fn new(name: String, path: PathBuf) -> Self {
        Self { name, path }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn destructure(self) -> (String, PathBuf) {
        (self.name, self.path)
    }
}

pub trait HasWasomeSourceElementLocation {
    fn location(&self) -> &WasomeSourceElementLocation;
}

fn duplicate_wasome_source_elements<
    'a,
    Element: HasWasomeSourceElementLocation + 'a,
    Elements: ExactSizeIterator<Item = &'a Element>,
>(
    to_check: Elements,
) -> Option<String> {
    let mut found = HashSet::with_capacity(to_check.len());
    for e in to_check {
        let name = e.location().name();
        if found.contains(name) {
            return Some(name.to_owned());
        }
        found.insert(name);
    }
    None
}
