use crate::directory::Directory;
use crate::symbol::Symbol;
use crate::top_level::{Import, ImportRoot};
use crate::traversal::file_traversal::FileTraversalHelper;
use crate::{AST, ASTNode, ASTType};
use std::ops::Deref;
use std::path::PathBuf;

#[derive(Debug)]
pub struct DirectoryTraversalHelper<'a, 'b, Type: ASTType> {
    inner: &'b ASTNode<Directory<Type>, PathBuf>,
    parent: Option<&'a DirectoryTraversalHelper<'a, 'b, Type>>,
}

impl<'a, 'b, Type: ASTType> DirectoryTraversalHelper<'a, 'b, Type> {
    pub fn new_child(
        inner: &'b ASTNode<Directory<Type>, PathBuf>,
        parent: &'a DirectoryTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self {
            inner,
            parent: Some(parent),
        }
    }

    pub fn new_root(inner: &'b ASTNode<Directory<Type>, PathBuf>) -> Self {
        Self {
            inner,
            parent: None,
        }
    }

    pub fn new_from_ast(ast: &'b AST<Type>) -> Self {
        Self::new_root(ast.deref())
    }

    pub fn inner(&self) -> &'b ASTNode<Directory<Type>, PathBuf> {
        self.inner
    }
    pub fn len_subdirectories(&self) -> usize {
        self.inner.subdirectories().len()
    }
    pub fn index_subdirectory(&self, index: usize) -> DirectoryTraversalHelper<'_, 'b, Type> {
        DirectoryTraversalHelper::new_child(&self.inner.subdirectories()[index], self)
    }

    pub fn specific_subdirectory(
        &self,
        name: &str,
    ) -> Option<DirectoryTraversalHelper<'_, 'b, Type>> {
        self.subdirectories_iterator()
            .find(|subdir| subdir.inner.name() == name)
    }
    pub fn subdirectories_iterator<'c>(
        &'c self,
    ) -> impl Iterator<Item = DirectoryTraversalHelper<'c, 'b, Type>> + 'c {
        self.inner
            .subdirectories()
            .iter()
            .map(move |subdirectory| DirectoryTraversalHelper::new_child(subdirectory, self))
    }
    pub fn len_files(&self) -> usize {
        self.inner.files().len()
    }
    pub fn index_file(&self, index: usize) -> FileTraversalHelper<'_, 'b, Type> {
        FileTraversalHelper::new(&self.inner.files()[index], self)
    }

    pub fn specific_file(&self, name: &str) -> Option<FileTraversalHelper<'_, 'b, Type>> {
        self.files_iterator()
            .find(|file| file.inner().name() == name)
    }

    pub fn files_iterator<'c>(
        &'c self,
    ) -> impl Iterator<Item = FileTraversalHelper<'c, 'b, Type>> + 'c {
        self.inner
            .files()
            .iter()
            .map(|file| FileTraversalHelper::new(file, self))
    }

    pub fn resolve_import(&self, to_resolve: &Import) -> Option<Symbol<'b, Type>> {
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
