use ast::directory::Directory;
use ast::file::File;
use ast::{ASTNode, UntypedAST};
use std::ops::Deref;
use std::path::PathBuf;

/// Builds an untyped directory
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
    /// They are implicitly created and don't need manual handling by the caller
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
            Some(_) => None,
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
            Some(_) => None,
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
    pub fn add_file(
        &mut self,
        to_add: ASTNode<File<UntypedAST>, PathBuf>,
        path: &[String],
    ) -> Option<()> {
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
    pub fn subdir_by_path_nonmutating(&self, path: &[String]) -> Option<&DirectoryBuilder> {
        match path.first() {
            None => Some(self),
            Some(dir_name) => {
                self.subdir_by_name(dir_name)?
                    // The length is always at least one
                    .subdir_by_path_nonmutating(&path[1..])
            }
        }
    }

    /// Gets the file specified by a path and a filename
    ///
    /// # Parameter
    ///
    /// - **path** - The path to lookup.
    /// - **filename** - The name of the file
    ///     - Excluding the file extension
    ///
    /// # Return
    ///
    /// - **None** - The file does not exist.
    /// - **Some(file)** - The file was found
    pub fn file_by_path_name(&self, path: &[String], filename: &str) -> Option<&File<UntypedAST>> {
        self.subdir_by_path_nonmutating(path)?
            .file_by_name(filename)
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

    /// Gets the file specified by name
    ///
    /// # Parameter
    ///
    /// - **name** - The name of the file to lookup.
    ///
    /// # Return
    ///
    /// - **None** - The file does not exist
    /// - **Some(file)** - The file was found
    pub fn file_by_name(&self, name: &str) -> Option<&File<UntypedAST>> {
        self.files
            .iter()
            .filter(|file| file.name() == name)
            .map(|file| file.deref())
            .next()
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
    /// - **name** - The name of the subdir to lookup.
    ///
    /// # Return
    ///
    /// - **None** - The subdir does not exist
    /// - **Some(subdir)** - The subdir was found
    fn subdir_by_name(&self, name: &str) -> Option<&DirectoryBuilder> {
        // Subdirs must be unique by name
        self.subdirectories
            .iter()
            .find(|subdir| subdir.name == name)
    }

    /// Like [`Self::subdir_by_name`], but with `&mut` instead of `&`
    fn subdir_by_name_mut(&mut self, name: &str) -> Option<&mut DirectoryBuilder> {
        // Subdirs must be unique by name
        self.subdirectories
            .iter_mut()
            .find(|subdir| subdir.name == name)
    }

    /// Like [`Self::add_subdirectory_directly`], but it only requires a name and handles the rest
    /// on its own
    fn create_subdir(&mut self, name: String) -> Option<()> {
        let mut subdir_path = self.location.clone();
        subdir_path.push(&name);
        self.add_subdirectory_directly(Self::new(name, subdir_path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::ASTNode;
    use ast::UntypedAST;
    use ast::file::File;
    use std::path::PathBuf;

    fn create_dummy_file(name: &str) -> ASTNode<File<UntypedAST>, PathBuf> {
        ASTNode::new(
            File::new(name.to_string(), Vec::new(), Vec::new(), Vec::new(), Vec::new()),
            PathBuf::from(name),
        )
    }

    #[test]
    fn test_new() {
        let builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        assert_eq!(builder.name, "root");
        assert_eq!(builder.location, PathBuf::from("root"));
        assert!(builder.subdirectories.is_empty());
        assert!(builder.files.is_empty());
    }

    #[test]
    fn test_add_file_directly() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        let file = create_dummy_file("test");

        // Add successful
        assert_eq!(builder.add_file_directly(file), Some(()));
        assert_eq!(builder.files.len(), 1);
        assert_eq!(builder.files[0].name(), "test");

        // Add duplicate
        let file_dup = create_dummy_file("test");
        assert_eq!(builder.add_file_directly(file_dup), None);
        // Verify collection hasn't changed
        assert_eq!(builder.files.len(), 1);
        assert_eq!(builder.files[0].name(), "test");
    }

    #[test]
    fn test_add_subdirectory_directly() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        let subdir = DirectoryBuilder::new("sub".to_string(), PathBuf::from("root/sub"));

        // Add successful
        assert_eq!(builder.add_subdirectory_directly(subdir), Some(()));
        assert_eq!(builder.subdirectories.len(), 1);
        assert_eq!(builder.subdirectories[0].name, "sub");
        assert_eq!(
            builder.subdirectories[0].location,
            PathBuf::from("root/sub")
        );

        // Add duplicate
        let subdir_dup = DirectoryBuilder::new("sub".to_string(), PathBuf::from("root/sub"));
        assert_eq!(builder.add_subdirectory_directly(subdir_dup), None);
        // Verify collection hasn't changed
        assert_eq!(builder.subdirectories.len(), 1);
        assert_eq!(builder.subdirectories[0].name, "sub");
    }

    #[test]
    fn test_add_file_nested() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));

        // Add to root (empty path)
        let f1 = create_dummy_file("f1");
        assert_eq!(builder.add_file(f1, &[]), Some(()));
        assert_eq!(builder.files.len(), 1);
        assert_eq!(builder.files[0].name(), "f1");

        // Add to existing subdir
        builder.create_subdir("sub".to_string());
        let f2 = create_dummy_file("f2");
        assert_eq!(builder.add_file(f2, &["sub".to_string()]), Some(()));

        let sub = builder.subdir_by_name("sub").unwrap();
        assert_eq!(sub.files.len(), 1);
        assert_eq!(sub.files[0].name(), "f2");

        // Add to new nested subdir (should create intermediate)
        let f3 = create_dummy_file("f3");
        assert_eq!(
            builder.add_file(f3, &["sub".to_string(), "nested".to_string()]),
            Some(())
        );

        let sub = builder.subdir_by_name("sub").unwrap();
        let nested = sub.subdir_by_name("nested").unwrap();
        assert_eq!(nested.files.len(), 1);
        assert_eq!(nested.files[0].name(), "f3");
    }

    #[test]
    fn test_add_file_duplicate_nested() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        let f1 = create_dummy_file("f1");
        builder.add_file(f1, &["sub".to_string()]);

        let f1_dup = create_dummy_file("f1");
        assert_eq!(builder.add_file(f1_dup, &["sub".to_string()]), None);

        // Verify integrity
        let sub = builder.subdir_by_name("sub").unwrap();
        assert_eq!(sub.files.len(), 1);
        assert_eq!(sub.files[0].name(), "f1");
    }

    #[test]
    fn test_subdir_by_path() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));

        // Root
        let root_ref = builder.subdir_by_path(&[]);
        assert_eq!(root_ref.name, "root");

        // Create deep path
        let deep = builder.subdir_by_path(&["a".to_string(), "b".to_string()]);
        assert_eq!(deep.name, "b");

        // Verify structure
        let a = builder.subdir_by_name("a").unwrap();
        assert_eq!(a.name, "a");
        let b = a.subdir_by_name("b").unwrap();
        assert_eq!(b.name, "b");
    }

    #[test]
    fn test_subdir_by_path_nonmutating() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        builder.subdir_by_path(&["a".to_string()]);

        assert!(builder.subdir_by_path_nonmutating(&[]).is_some());
        assert!(
            builder
                .subdir_by_path_nonmutating(&["a".to_string()])
                .is_some()
        );
        assert!(
            builder
                .subdir_by_path_nonmutating(&["b".to_string()])
                .is_none()
        );
        assert!(
            builder
                .subdir_by_path_nonmutating(&["a".to_string(), "b".to_string()])
                .is_none()
        );
    }

    #[test]
    fn test_file_by_path() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        let f1 = create_dummy_file("f1");
        builder.add_file(f1, &["a".to_string()]); // puts f1 in a

        // Found
        let found = builder.file_by_path_name(&["a".to_string()], "f1");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name(), "f1");

        // Not found (wrong name)
        assert!(
            builder
                .file_by_path_name(&["a".to_string()], "f2")
                .is_none()
        );

        // Not found (wrong path)
        assert!(
            builder
                .file_by_path_name(&["b".to_string()], "f1")
                .is_none()
        );

        // File in root
        let f2 = create_dummy_file("f2");
        builder.add_file_directly(f2);
        assert!(builder.file_by_path_name(&[], "f2").is_some());
    }

    #[test]
    fn test_build() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        let f1 = create_dummy_file("f1");
        builder.add_file(f1, &["sub".to_string()]);

        let dir_node = builder.build();
        let dir = dir_node.deref();

        assert_eq!(dir.name(), "root");
        assert_eq!(dir.subdirectories().len(), 1);
        assert_eq!(dir.files().len(), 0);

        let sub = &dir.subdirectories()[0];
        assert_eq!(sub.name(), "sub");
        assert_eq!(sub.files().len(), 1);
        assert_eq!(sub.files()[0].name(), "f1");
    }

    #[test]
    fn test_ensure_subdir_exists() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        builder.ensure_subdir_exists("sub".to_string());

        assert_eq!(builder.subdirectories.len(), 1);
        assert_eq!(builder.subdirectories[0].name, "sub");

        // Ensure again
        builder.ensure_subdir_exists("sub".to_string());
        assert_eq!(builder.subdirectories.len(), 1);
        assert_eq!(builder.subdirectories[0].name, "sub");
    }

    #[test]
    fn test_create_subdir() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        assert_eq!(builder.create_subdir("sub".to_string()), Some(()));
        assert_eq!(builder.subdirectories.len(), 1);
        assert_eq!(builder.subdirectories[0].name, "sub");

        // Create duplicate
        assert_eq!(builder.create_subdir("sub".to_string()), None);
        assert_eq!(builder.subdirectories.len(), 1);
    }

    #[test]
    fn test_subdir_by_name_mut() {
        let mut builder = DirectoryBuilder::new("root".to_string(), PathBuf::from("root"));
        builder.create_subdir("sub".to_string());

        assert!(builder.subdir_by_name_mut("sub").is_some());
        assert!(builder.subdir_by_name_mut("nonexistent").is_none());
    }
}
