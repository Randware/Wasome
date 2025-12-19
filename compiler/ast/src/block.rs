use crate::statement::Statement;
use crate::top_level::Function;
use crate::{ASTNode, ASTType, SemanticEquality};
use std::fmt::Debug;
use std::ops::Deref;

#[derive(Debug, PartialEq)]
pub struct SingleContentBlock<Content: Debug + PartialEq> {
    contents: Vec<ASTNode<Content>>,
}

impl<Content: Debug + PartialEq> SingleContentBlock<Content> {
    pub fn new(contents: Vec<ASTNode<Content>>) -> Self {
        Self { contents }
    }
}

impl<Content: Debug + PartialEq> Deref for SingleContentBlock<Content> {
    type Target = [ASTNode<Content>];

    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl<Content: Debug + PartialEq + SemanticEquality> SemanticEquality
    for SingleContentBlock<Content>
{
    fn semantic_equals(&self, other: &Self) -> bool {
        self.contents.semantic_equals(&other.contents)
    }
}

/// This represents a codeblock as per section 4 of the lang spec
///
/// Even if unenforced, the trait bound still serves documentation purposes.
#[allow(type_alias_bounds)]
pub type CodeBlock<Type: ASTType> = SingleContentBlock<Statement<Type>>;

/// This contains functions. When inside a composite, they are methods.
///
/// Even if unenforced, the trait bound still serves documentation purposes.
#[allow(type_alias_bounds)]
pub type FunctionBlock<Type: ASTType> = SingleContentBlock<Function<Type>>;
