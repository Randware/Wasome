use crate::file::File;
use crate::{ASTNode, ASTType, SemanticEquality};
use std::path::PathBuf;

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
