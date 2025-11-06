use crate::statement::Statement;
use crate::{ASTNode, ASTType, SemanticEquality};
use std::fmt::Debug;
use std::ops::Deref;

/** This represents a codeblock as per section 4 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct CodeBlock<Type: ASTType> {
    contents: Vec<ASTNode<Statement<Type>>>,
}

impl<Type: ASTType> CodeBlock<Type> {
    pub fn new(contents: Vec<ASTNode<Statement<Type>>>) -> Self {
        Self { contents }
    }
}

impl<Type: ASTType> Deref for CodeBlock<Type> {
    type Target = [ASTNode<Statement<Type>>];

    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl<Type: ASTType> SemanticEquality for CodeBlock<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.contents.semantic_equals(&other.contents)
    }
}
