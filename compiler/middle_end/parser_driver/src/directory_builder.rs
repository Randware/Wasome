use std::ops::Deref;
use ast::directory::Directory;
use ast::file::File;
use ast::{ASTNode, UntypedAST};
use std::path::PathBuf;

/// Builds a directory
#[derive(Debug)]
pub(crate) struct DirectoryBuilder {
    /// The location of the directory
    ///
    /// Relative to the program root
    location: PathBuf,
    /// The name of the directory
    name: String,
    /// The subdirectories
    ///
    /// They are implicitly created and don't need manual handling by the customer
    ///
    /// Duplicates are forbidden
    subdirectories: Vec<DirectoryBuilder>,
    /// The files
    ///
    /// The position is relative to the program root
    /// Duplicates are forbidden
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

    /// Attempts to add a file
    ///
    /// Checks if a file with this name already exists
    ///
    /// # Parameter
    ///
    /// - **to_add** - The file to add
    ///
    /// # Return
    ///
    /// - **Some(())** - If adding was successful
    /// - **None** - If a file with this name already exists
    pub fn add_file_directly(&mut self, to_add: ASTNode<File<UntypedAST>, PathBuf>) -> Option<()> {
        match self.file_by_name(to_add.name()) {
            None => {
                self.files.push(to_add);
                Some(())
            }
            Some(_) => None
        }
    }

    /// Attempts to add a directory
    ///
    /// Checks if a directory with this name already exists
    ///
    /// This is private as directories are internally managed
    ///
    /// # Parameter
    ///
    /// - **to_add** - The directory to add
    ///
    /// # Return
    ///
    /// - **Some(())** - If adding was successful
    /// - **None** - If a directory with this name already exists
    fn add_subdirectory_directly(&mut self, to_add: DirectoryBuilder) -> Option<()> {
        match self.subdir_by_name(&to_add.name) {
            None => {
                self.subdirectories.push(to_add);
                Some(())
            }
            Some(_) => None
        }
    }

    /// Attempts to add a file in a provided directory
    ///
    /// Checks if a file with this name already exists
    ///
    ///
    /// # Parameters
    ///
    /// - **to_add** - The file to add
    ///     - Its name is supposed to be without a file extension
    /// - **path** - The path to traverse before adding
    ///
    /// # Return
    ///
    /// - **Some(())** - If adding was successful
    /// - **None** - If a file with this name already exists
    pub fn add_file(&mut self, to_add: ASTNode<File<UntypedAST>, PathBuf>, path: &[String]) -> Option<()> {
        self.subdir_by_path(path).add_file_directly(to_add)
    }

    /// Gets or adds the directory specified by path
    ///
    /// Adds all required directories
    ///
    /// # Parameters
    ///
    /// - **path** - The path to lookup.
    ///     - Empty means `self`
    ///
    /// # Return
    ///
    /// **The directory**
    pub fn subdir_by_path(&mut self, path: &[String]) -> &mut DirectoryBuilder {
        match path.first() {
            None => self,
            Some(dir_name) => {
                self.ensure_subdir_exists(dir_name.to_owned());
                self.subdir_by_name_mut(dir_name)
                    .unwrap()
                    // The length is always at least one
                    .subdir_by_path(&path[1..])
            }
        }
    }

    /// Gets the directory specified by path
    ///
    /// Unlike [`Self::subdir_by_path`], this does not add nonexisting directories
    ///
    /// This is private as the existence (or nonexistence) of specific directories is an
    /// implementation detail and being exposed here
    ///
    /// # Parameter
    ///
    /// - **path** - The path to lookup.
    ///     - Empty means `self`
    ///
    /// # Return
    ///
    /// - **None** - The directory does not exist
    /// - **Some(directory)** - The directory was found
    fn subdir_by_path_nonmutating(&self, path: &[String]) -> Option<&DirectoryBuilder> {
        match path.first() {
            None => Some(self),
            Some(dir_name) => {
                self.subdir_by_name(dir_name)?
                    // The length is always at least one
                    .subdir_by_path_nonmutating(&path[1..])
            }
        }
    }

    /// Gets the file specified by a path
    ///
    /// # Parameter
    ///
    /// - **path** - The path to lookup.
    ///     - Empty paths are not allowed as they can never reference a file
    ///
    /// # Return
    ///
    /// - **None** - The file does not exist. This includes empty paths
    /// - **Some(file)** - The file was found
    pub fn file_by_path(&self, path: &[String]) -> Option<&File<UntypedAST>> {
        if path.len() == 0 {
            return None;
        }
        // Unwrap will never panic as the length was checked to be at least 1
        self.subdir_by_path_nonmutating(&path[0..path.len()-1])?.file_by_name(path.last().unwrap())
    }

    /// Turns self into a [`Directory`]
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

    /// Ensures that a subdir with a specific name exists
    ///
    /// If it does, this does nothing
    /// If it doesn't, it is added
    fn ensure_subdir_exists(&mut self, name: String) {
        if self.subdir_by_name(&name).is_none() {
            self.create_subdir(name);
        }
    }

    /// Gets the subdir specified by name
    ///
    /// # Parameter
    ///
    /// - **name** - The name if the subdir to lookup.
    ///
    /// # Return
    ///
    /// - **None** - The subdir does not exist
    /// - **Some(subdir)** - The subdir was found
    fn subdir_by_name(&self, name: &str) -> Option<&DirectoryBuilder> {
        // Subdirs must be unique by name
        self.subdirectories
            .iter()
            .filter(|subdir| subdir.name == name)
            .next()
    }

    /// Like [`Self::subdir_by_name`], but with `&mut` instead of `&`
    fn subdir_by_name_mut(&mut self, name: &str) -> Option<&mut DirectoryBuilder> {
        // Subdirs must be unique by name
        self.subdirectories
            .iter_mut()
            .filter(|subdir| subdir.name == name)
            .next()
    }

    /// Like [`Self::add_subdirectory_directly`], but it only requires a name and handles the rest
    /// on its own
    fn create_subdir(&mut self, name: String) -> Option<()> {
        let mut subdir_path = self.location.clone();
        subdir_path.push(&name);
        self.add_subdirectory_directly(Self::new(name, subdir_path))
    }

    /// Gets the file specified by name
    ///
    /// # Parameter
    ///
    /// - **name** - The name if the file to lookup.
    ///
    /// # Return
    ///
    /// - **None** - The file does not exist
    /// - **Some(file)** - The file was found
    fn file_by_name(&self, name: &str) -> Option<&File<UntypedAST>> {
        self.files.iter().filter(|file| file.name() == name).map(|file| file.deref()).next()
    }
}
