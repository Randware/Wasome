use std::ops::Deref;
use crate::ASTType;
use crate::statement::Statement;

/** This represents a codeblock as per section 4 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct CodeBlock<Type: ASTType>
{
    statements: Vec<Statement<Type>>
}

impl<Type: ASTType> CodeBlock<Type>
{
    pub fn new(statements: Vec<Statement<Type>>) -> Self
    {
        Self {
            statements
        }
    }
}

impl<Type: ASTType> Deref for CodeBlock<Type>
{
    type Target = [Statement<Type>];

    fn deref(&self) -> &Self::Target
    {
        &self.statements
    }
}