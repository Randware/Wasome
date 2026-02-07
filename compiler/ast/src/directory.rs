use crate::file::File;
use crate::symbol::DirectlyAvailableSymbol;
use crate::top_level::Import;
use crate::{ASTNode, ASTType, SemanticEq};
use std::ops::Deref;
use std::path::PathBuf;
use source::types::FileID;

/// A directory containing code.
///
/// Directories are also known as modules
///
/// Directories are located in directories
/// or are root of the ast
/// + In the latter case, they represent the entire bundle to compile instead
///     + Directories contained in these directories represent a single wasome project instead
///
/// # Contents
///
/// It has a name, subdirectories and files
/// + subdirectories and files may both be empty vecs
#[derive(Debug, PartialEq)]
pub struct Directory<Type: ASTType> {
    name: String,
    subdirectories: Vec<ASTNode<Directory<Type>, PathBuf>>,
    files: Vec<ASTNode<File<Type>, FileID>>,
}

impl<Type: ASTType> Directory<Type> {
    pub fn new(
        name: String,
        subdirectories: Vec<ASTNode<Directory<Type>, PathBuf>>,
        files: Vec<ASTNode<File<Type>, FileID>>,
    ) -> Self {
        Self {
            name,
            subdirectories,
            files,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn subdirectories(&self) -> &[ASTNode<Directory<Type>, PathBuf>] {
        &self.subdirectories
    }

    pub fn files(&self) -> &[ASTNode<File<Type>, FileID>] {
        &self.files
    }

    /// Searches this directory for a file with the provided name
    ///
    /// The names have to match exactly, regex or similar is not supported
    ///
    /// # Params
    ///
    /// name: The name of the file to find
    ///
    /// # Return
    ///
    /// None if the file does not exist
    /// Some(file) if the file does exist
    pub fn file_by_name(&self, name: &str) -> Option<&ASTNode<File<Type>, FileID>> {
        self.files_iterator().find(|file| file.name() == name)
    }

    /// Gets an iterator over all files contained in this directory
    /// # Return
    /// The Iterator
    pub fn files_iterator(&self) -> impl Iterator<Item = &ASTNode<File<Type>, FileID>> {
        self.files().iter()
    }

    /// Searches this directory for a subdirectory with the provided name
    ///
    /// The names have to match exactly, regex or similar is not supported
    ///
    /// # Params
    ///
    /// name: The name of the directory to find
    ///
    /// # Return
    ///
    /// None if the directory does not exist
    /// Some(directory) if the directory does exist
    pub fn subdirectory_by_name(&self, name: &str) -> Option<&ASTNode<Directory<Type>, PathBuf>> {
        self.subdirectories_iterator()
            .find(|subdir| subdir.inner.name() == name)
    }

    /// Gets an iterator over all subdirectories contained in this directory
    /// # Return
    /// The Iterator
    pub fn subdirectories_iterator(
        &self,
    ) -> impl Iterator<Item = &ASTNode<Directory<Type>, PathBuf>> {
        self.subdirectories().iter()
    }

    /// Looks the symbol specified by path up
    ///
    /// ### Return
    ///
    /// None if the import could not be resolved.
    /// * This is not using an empty iterator
    ///     1. Makes it easier to spot problematic imports
    ///     2. Saves a heap allocation as we'd need a trait object
    ///
    /// Otherwise n iterator with all `pub` symbols in the found file is returned
    /// * Note this may be empty if the iterator does not provide any `pub` symbols.
    pub(crate) fn get_symbols_for_path(
        &self,
        path: &[String],
    ) -> Option<impl Iterator<Item = DirectlyAvailableSymbol<'_, Type>>> {
        match path.len() {
            0 => Some(self.files().iter().flat_map(|file| file.symbols_public())),
            len => self
                .subdirectory_by_name(&path[0])?
                .get_symbols_for_path(&path[1..len]),
        }
    }

    // Rustrover is wrong, eliding the lifetimes causes errors
    /// Recursively finds all imports and calls callback for them.
    ///
    /// # Callback behavior
    ///
    /// For each import, the provided callback will be called with the import as
    /// the first parameter and the directory directly containing the file of the import
    /// will be the second parameter.
    ///
    /// # Params
    /// callback: The callback. It is a mutable reference to allow for easier recursion
    pub(crate) fn traverse_imports<'a>(
        &'a self,
        callback: &mut impl FnMut(&'a ASTNode<Import>, &'a Directory<Type>),
    ) {
        self.files_iterator().for_each(|file| {
            file.imports()
                .iter()
                .for_each(|import| callback(import, self))
        });
        self.subdirectories_iterator()
            .for_each(|subdir| subdir.deref().traverse_imports(callback));
    }
}

impl<Type: ASTType> SemanticEq for Directory<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.subdirectories().semantic_eq(other.subdirectories())
            && self.files().semantic_eq(other.files())
    }
}
