use crate::ASTType;
use crate::block::FunctionBlock;

#[derive(Debug, PartialEq)]
pub struct File<Type: ASTType>
{
    name: String,
    imports: Vec<Type::ImportType>,
    functions: FunctionBlock<Type>
}

impl<Type: ASTType> File<Type>
{
    pub fn new(name: String, imports: Vec<Type::ImportType>, functions: FunctionBlock<Type>) -> Self {
        Self { name, imports, functions }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn functions(&self) -> &FunctionBlock<Type> {
        &self.functions
    }
}