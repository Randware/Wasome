use crate::ASTType;
use crate::file::File;

pub struct Directory<Type: ASTType>
{
    name: String,
    subdirectories: Box<Vec<Directory<Type>>>,
    files: Vec<File<Type>>
}

impl<Type: ASTType> Directory<Type>
{
    pub fn new(name: String, subdirectories: Vec<Directory<Type>>, files: Vec<File<Type>>) -> Self {
        Self { name, subdirectories: Box::new(subdirectories), files }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn subdirectories(&self) -> &[Directory<Type>] {
        &self.subdirectories
    }

    pub fn files(&self) -> &[File<Type>] {
        &self.files
    }
}