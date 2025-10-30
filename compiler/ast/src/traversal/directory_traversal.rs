use crate::directory::Directory;
use crate::statement::Statement;
use crate::traversal::file_traversal::FileTraversalHelper;
use crate::traversal::statement_traversal::StatementTraversalHelper;
use crate::{AST, ASTNode, ASTType};
use std::ops::Deref;
use std::path::PathBuf;

#[derive(Debug)]
pub struct DirectoryTraversalHelper<'a, Type: ASTType> {
    inner: &'a ASTNode<Directory<Type>, PathBuf>,
    parent: Option<&'a DirectoryTraversalHelper<'a, Type>>,
}

impl<'a, Type: ASTType> DirectoryTraversalHelper<'a, Type> {
    pub fn new_child(
        inner: &'a ASTNode<Directory<Type>, PathBuf>,
        parent: &'a DirectoryTraversalHelper<'a, Type>,
    ) -> Self {
        Self {
            inner,
            parent: Some(parent),
        }
    }

    pub fn new_root(inner: &'a ASTNode<Directory<Type>, PathBuf>) -> Self {
        Self {
            inner,
            parent: None,
        }
    }

    pub fn new_from_ast(ast: &'a AST<Type>) -> Self {
        Self::new_root(ast.deref())
    }

    pub fn inner(&self) -> &'a ASTNode<Directory<Type>, PathBuf> {
        self.inner
    }
    pub fn len_subdirectories(&self) -> usize {
        self.inner.subdirectories().len()
    }
    pub fn index_subdirectory(&self, index: usize) -> DirectoryTraversalHelper<'_, Type> {
        DirectoryTraversalHelper::new_child(&self.inner.subdirectories()[index], self)
    }

    pub fn specific_subdirectory(&self, name: &str) -> Option<DirectoryTraversalHelper<'_, Type>> {
        self.subdirectories_iterator()
            .filter(|subdir| subdir.name() == name)
            .next()
    }
    pub fn subdirectories_iterator<'b>(
        &'b self,
    ) -> impl Iterator<Item = DirectoryTraversalHelper<'b, Type>> + 'b {
        self.inner
            .subdirectories()
            .iter()
            .map(|subdirectory| DirectoryTraversalHelper::new_child(subdirectory, self))
    }
    pub fn len_files(&self) -> usize {
        self.inner.files().len()
    }
    pub fn index_file(&self, index: usize) -> FileTraversalHelper<'_, Type> {
        FileTraversalHelper::new(&self.inner.files()[index], self)
    }

    pub fn specific_file(&self, name: &str) -> Option<FileTraversalHelper<'_, Type>> {
        self.files_iterator()
            .filter(|file| file.name() == name)
            .next()
    }

    pub fn files_iterator<'b>(
        &'b self,
    ) -> impl Iterator<Item = FileTraversalHelper<'b, Type>> + 'b {
        self.inner
            .files()
            .iter()
            .map(|file| FileTraversalHelper::new(file, self))
    }
}

impl<Type: ASTType> Deref for DirectoryTraversalHelper<'_, Type> {
    type Target = Directory<Type>;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}
