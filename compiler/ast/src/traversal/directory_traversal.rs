use crate::directory::Directory;
use crate::symbol::Symbol;
use crate::top_level::{Import, ImportRoot};
use crate::traversal::file_traversal::FileTraversalHelper;
use crate::{AST, ASTNode, ASTType};
use std::ops::Deref;
use std::path::PathBuf;

/** This struct helps with traversing directories
It keeps a reference to a directory and its root (also a directory).

### Lifetimes

| Lifetime     | Purpose      |
| ------------- | ------------- |
| 'a | How long the traversal helper may life |
| 'b | How long the underlying data may life |
*/
#[derive(Debug)]
pub struct DirectoryTraversalHelper<'a, 'b, Type: ASTType> {
    inner: &'b ASTNode<Directory<Type>, PathBuf>,
    parent: Option<&'a DirectoryTraversalHelper<'a, 'b, Type>>,
}

impl<'a, 'b, Type: ASTType> DirectoryTraversalHelper<'a, 'b, Type> {
    /** Creates a new DirectoryTraversalHelper that is the child of another
    */
    fn new_child(
        inner: &'b ASTNode<Directory<Type>, PathBuf>,
        parent: &'a DirectoryTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self {
            inner,
            parent: Some(parent),
        }
    }

    /** Creates a new DirectoryTraversalHelper that is the root (has no parent)
    */
    pub fn new_root(inner: &'b ASTNode<Directory<Type>, PathBuf>) -> Self {
        Self {
            inner,
            parent: None,
        }
    }

    /** Creates a new DirectoryTraversalHelper from an ast.
    The result will be a root
    */
    pub fn new_from_ast(ast: &'b AST<Type>) -> Self {
        Self::new_root(ast.deref())
    }

    /** Gets the inner directory
    */
    pub fn inner(&self) -> &'b ASTNode<Directory<Type>, PathBuf> {
        self.inner
    }
    /** Gets the length of the subdirectories
     */
    pub fn len_subdirectories(&self) -> usize {
        self.inner.subdirectories().len()
    }
    /** Gets the subdirectory at a specific index
    ### Panics
    Panics if ```index > self.len_subdirectories()```
    */
    pub fn index_subdirectory(&self, index: usize) -> DirectoryTraversalHelper<'_, 'b, Type> {
        DirectoryTraversalHelper::new_child(&self.inner.subdirectories()[index], self)
    }
    /** Gets the subdirectory with the specified name.
    Returns None if it doesn't exist
     */
    pub fn specific_subdirectory(
        &self,
        name: &str,
    ) -> Option<DirectoryTraversalHelper<'_, 'b, Type>> {
        self.subdirectories_iterator()
            .find(|subdir| subdir.inner.name() == name)
    }
    /** Gets an iterator over all subdirectories
    */
    pub fn subdirectories_iterator<'c>(
        &'c self,
    ) -> impl Iterator<Item = DirectoryTraversalHelper<'c, 'b, Type>> + 'c {
        self.inner
            .subdirectories()
            .iter()
            .map(move |subdirectory| DirectoryTraversalHelper::new_child(subdirectory, self))
    }
    /** Gets the number of contained files
    */
    pub fn len_files(&self) -> usize {
        self.inner.files().len()
    }
    /** Gets the file at a specific index
    ### Panics
       Panics if ```index > self.len_files()```
    */
    pub fn index_file(&self, index: usize) -> FileTraversalHelper<'_, 'b, Type> {
        FileTraversalHelper::new(&self.inner.files()[index], self)
    }
    /** Gets the file with the specified name
    Returns None if it doesn't exist
    */
    pub fn specific_file(&self, name: &str) -> Option<FileTraversalHelper<'_, 'b, Type>> {
        self.files_iterator()
            .find(|file| file.inner().name() == name)
    }
    /** Gets an iterator over all files
    */
    pub fn files_iterator<'c>(
        &'c self,
    ) -> impl Iterator<Item = FileTraversalHelper<'c, 'b, Type>> + 'c {
        self.inner
            .files()
            .iter()
            .map(|file| FileTraversalHelper::new(file, self))
    }
    /** Gets the symbol imported by a specific import
    Returns None if it doesn't exist
    */
    pub(crate) fn resolve_import(&self, to_resolve: &Import) -> Option<Symbol<'b, Type>> {
        let root = match to_resolve.root() {
            ImportRoot::CurrentDirectory => self,
            ImportRoot::ProjectRoot => self.get_root(),
        };
        root.get_symbol_for_path(to_resolve.path())
    }

    fn get_symbol_for_path(&self, path: &[String]) -> Option<Symbol<'b, Type>> {
        match path.len() {
            len if len < 2 => None, //Empty or too short path
            2 => self
                .specific_file(&path[0])?
                .inner()
                .get_top_level_symbol(&path[1]),
            len => self
                .specific_subdirectory(&path[0])?
                .get_symbol_for_path(&path[1..len]),
        }
    }

    fn get_root(&self) -> &DirectoryTraversalHelper<'a, 'b, Type> {
        self.parent
            .as_ref()
            .map(|parent| parent.get_root())
            .unwrap_or(self)
    }
}
