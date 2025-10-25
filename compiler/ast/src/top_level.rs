use crate::statement::Statement;
use crate::symbol::FunctionSymbol;
use crate::{ASTNode, ASTType, SemanticEquality};
use std::fmt::Debug;
use std::rc::Rc;

/** This is an arbitiary top-level construct
For now, there are only functions
*/
#[derive(Debug, PartialEq)]
pub enum TopLevelElement<Type: ASTType> {
    Function(Function<Type>),
}

impl<Type: ASTType> SemanticEquality for TopLevelElement<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        let TopLevelElement::Function(self_function) = self;
        let TopLevelElement::Function(other_function) = other;
        self_function.semantic_equals(other_function)
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<Type: ASTType> {
    declaration: Rc<FunctionSymbol<Type>>,
    implementation: ASTNode<Statement<Type>>,
}

impl<Type: ASTType> Function<Type> {
    pub fn new(
        declaration: Rc<FunctionSymbol<Type>>,
        implementation: ASTNode<Statement<Type>>,
    ) -> Self {
        Self {
            declaration,
            implementation,
        }
    }

    pub fn declaration(&self) -> &FunctionSymbol<Type> {
        &self.declaration
    }

    /** Gets the declaration by cloning the rc
     */
    pub fn declaration_owned(&self) -> Rc<FunctionSymbol<Type>> {
        self.declaration.clone()
    }

    pub fn implementation(&self) -> &ASTNode<Statement<Type>> {
        &self.implementation
    }
}

impl<Type: ASTType> SemanticEquality for Function<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.declaration == other.declaration
            && self.implementation.semantic_equals(&other.implementation)
    }
}
