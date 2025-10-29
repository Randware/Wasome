use std::path::PathBuf;
use crate::{ASTNode, ASTType};
use crate::file::File;

#[derive(Debug, PartialEq)]
pub struct Directory<Type: ASTType>
{
    name: String,
    subdirectories: Box<Vec<ASTNode<Directory<Type>, PathBuf>>>,
    files: Vec<ASTNode<File<Type>, PathBuf>>
}

impl<Type: ASTType> Directory<Type>
{
    pub fn new(name: String, subdirectories: Vec<ASTNode<Directory<Type>, PathBuf>>, files: Vec<ASTNode<File<Type>, PathBuf>>) -> Self {
        Self { name, subdirectories: Box::new(subdirectories), files }
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