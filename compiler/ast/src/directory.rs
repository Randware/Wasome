use crate::file::File;
use crate::symbol::DirectlyAvailableSymbol;
use crate::top_level::Import;
use crate::{ASTNode, ASTType, SemanticEquality};
use std::ops::Deref;
use std::path::PathBuf;

/** A directory containing files and other directories
*/
#[derive(Debug, PartialEq)]
pub struct Directory<Type: ASTType> {
    name: String,
    subdirectories: Vec<ASTNode<Directory<Type>, PathBuf>>,
    files: Vec<ASTNode<File<Type>, PathBuf>>,
}

impl<Type: ASTType> Directory<Type> {
    pub fn new(
        name: String,
        subdirectories: Vec<ASTNode<Directory<Type>, PathBuf>>,
        files: Vec<ASTNode<File<Type>, PathBuf>>,
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

    pub fn files(&self) -> &[ASTNode<File<Type>, PathBuf>] {
        &self.files
    }

    /** Gets the file with the specified name
          Returns None if it doesn't exist
    */
    pub fn file_by_name(&self, name: &str) -> Option<&ASTNode<File<Type>, PathBuf>> {
        self.files_iterator().find(|file| file.name() == name)
    }

    /** Gets an iterator over all files
     */
    pub fn files_iterator(&self) -> impl Iterator<Item = &ASTNode<File<Type>, PathBuf>> {
        self.files().iter()
    }

    /** Gets the subdirectory with the specified name.
       Returns None if it doesn't exist
    */
    pub fn subdirectory_by_name(&self, name: &str) -> Option<&ASTNode<Directory<Type>, PathBuf>> {
        self.subdirectories_iterator()
            .find(|subdir| subdir.inner.name() == name)
    }
    /** Gets an iterator over all subdirectories
     */
    pub fn subdirectories_iterator(
        &self,
    ) -> impl Iterator<Item = &ASTNode<Directory<Type>, PathBuf>> {
        self.subdirectories().iter()
    }

    /** Looks the symbol specified by path up
     */
    pub(crate) fn get_symbol_for_path(
        &self,
        path: &[String],
    ) -> Option<DirectlyAvailableSymbol<'_, Type>> {
        match path.len() {
            len if len < 2 => return None, //Empty or too short path
            // Symbols can either come directly from files or from structs
            2 | 3 => {
                if let Some(symbol) = self
                    .file_by_name(&path[0])
                    .and_then(|file| file.symbol_public(&path[1..]))
                {
                    return Some(symbol);
                }
            }
            _ => (),
        }
        self.subdirectory_by_name(&path[0])?
            .get_symbol_for_path(&path[1..path.len()])
    }

    // Rustrover is wrong, eliding the lifetimes causes errors
    /** Recursively finds all imports and calls callback for them. The second parameter for the callback is the direct parent directory of the import
     */
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

impl<Type: ASTType> SemanticEquality for Directory<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self
                .subdirectories()
                .semantic_equals(other.subdirectories())
            && self.files().semantic_equals(other.files())
    }
}
