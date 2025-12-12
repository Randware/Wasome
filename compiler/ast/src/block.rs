use crate::statement::Statement;
use crate::{ASTNode, ASTType, SemanticEquality};
use std::ops::Deref;

/** This represents a codeblock as per section 4 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct CodeBlock<Type: ASTType> {
    statements: Vec<ASTNode<Statement<Type>>>,
}

impl<Type: ASTType> CodeBlock<Type> {
    pub fn new(statements: Vec<ASTNode<Statement<Type>>>) -> Self {
        Self { statements }
    }
}

impl<Type: ASTType> Deref for CodeBlock<Type> {
    type Target = [ASTNode<Statement<Type>>];

    fn deref(&self) -> &Self::Target {
        &self.statements
    }
}

impl<Type: ASTType> SemanticEquality for CodeBlock<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.statements.semantic_equals(&other.statements)
    }
}
