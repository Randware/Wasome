use crate::ASTType;
use crate::statement::StatementNode;
use std::ops::Deref;

/** This represents a codeblock as per section 4 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct CodeBlock<Type: ASTType> {
    statements: Vec<StatementNode<Type>>,
}

impl<Type: ASTType> CodeBlock<Type> {
    pub fn new(statements: Vec<StatementNode<Type>>) -> Self {
        Self { statements }
    }
}

impl<Type: ASTType> Deref for CodeBlock<Type> {
    type Target = [StatementNode<Type>];

    fn deref(&self) -> &Self::Target {
        &self.statements
    }
}
