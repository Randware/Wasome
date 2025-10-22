use crate::statement::StatementNode;
use crate::{ASTType, SemanticEquality};
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

impl<Type: ASTType> SemanticEquality for CodeBlock<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.statements.semantic_equals(&other.statements)
    }
}
