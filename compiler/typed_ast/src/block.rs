use std::ops::Deref;
use crate::statement::Statement;

/** This represents a codeblock as per section 4 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct CodeBlock
{
    statements: Vec<Statement>
}

impl CodeBlock
{
    pub fn new(statements: Vec<Statement>) -> Self
    {
        Self {
            statements
        }
    }
}

impl Deref for CodeBlock
{
    type Target = [Statement];

    fn deref(&self) -> &Self::Target
    {
        &self.statements
    }
}