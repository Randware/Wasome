use std::ops::Deref;
use ast::directory::Directory;
use ast::file::File;
use ast::{ASTNode, UntypedAST};
use std::path::PathBuf;

#[derive(Debug)]
pub(crate) struct DirectoryBuilder {
    location: PathBuf,
    name: String,
    subdirectories: Vec<DirectoryBuilder>,
    files: Vec<ASTNode<File<UntypedAST>, PathBuf>>,
}

impl DirectoryBuilder {
    pub fn new(name: String, location: PathBuf) -> Self {
        Self {
            name,
            location,
            subdirectories: Vec::new(),
            files: Vec::new(),
        }
    }

    pub fn add_file_directly(&mut self, to_add: ASTNode<File<UntypedAST>, PathBuf>) {
        self.files.push(to_add)
    }

    fn add_subdirectory_directly(&mut self, to_add: DirectoryBuilder) {
        self.subdirectories.push(to_add)
    }

    pub fn add_file(&mut self, to_add: ASTNode<File<UntypedAST>, PathBuf>, path: &[String]) {
        self.subdir_by_path(path).add_file_directly(to_add)
    }

    pub fn subdir_by_path(&mut self, path: &[String]) -> &mut DirectoryBuilder {
        match path.first() {
            None => self,
            Some(dir_name) => {
                self.ensure_subdir_exists(dir_name.to_owned());
                self.subdir_by_name_mut(dir_name)
                    .unwrap()
                    .subdir_by_path(&path[1..])
            }
        }
    }
    
    pub fn file_by_path(&mut self, path: &[String]) -> Option<&File<UntypedAST>> {
        self.subdir_by_path(&path[0..path.len()-1]).file_by_name(path.last()?)
    }

    pub fn build(self) -> ASTNode<Directory<UntypedAST>, PathBuf> {
        ASTNode::new(
            Directory::new(
                self.name,
                self.subdirectories
                    .into_iter()
                    .map(|subdir| subdir.build())
                    .collect(),
                self.files,
            ),
            self.location,
        )
    }

    fn ensure_subdir_exists(&mut self, name: String) {
        if self.subdir_by_name(&name).is_none() {
            self.create_subdir(name);
        }
    }

    fn subdir_by_name(&self, name: &str) -> Option<&DirectoryBuilder> {
        // Subdirs must be unique by name
        self.subdirectories
            .iter()
            .filter(|subdir| subdir.name == name)
            .next()
    }

    fn subdir_by_name_mut(&mut self, name: &str) -> Option<&mut DirectoryBuilder> {
        // Subdirs must be unique by name
        self.subdirectories
            .iter_mut()
            .filter(|subdir| subdir.name == name)
            .next()
    }

    fn create_subdir(&mut self, name: String) {
        let mut subdir_path = self.location.clone();
        subdir_path.push(&name);
        self.subdirectories.push(Self::new(name, subdir_path))
    }
    
    fn file_by_name(&self, name: &str) -> Option<&File<UntypedAST>> {
        self.files.iter().filter(|file| file.name() == name).map(|file| file.deref()).next()
    }
}
