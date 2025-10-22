use crate::id::Id;
use crate::statement::StatementNode;
use crate::symbol::FunctionSymbol;
use crate::{ASTType, SemanticEquality};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct TopLevelElementNode<Type: ASTType> {
    inner: TopLevelElement<Type>,
    id: Id,
}

impl<Type: ASTType> TopLevelElementNode<Type> {
    pub fn new(inner: TopLevelElement<Type>) -> Self {
        Self {
            inner,
            id: Id::new(),
        }
    }
}

impl<Type: ASTType> Deref for TopLevelElementNode<Type> {
    type Target = TopLevelElement<Type>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<Type: ASTType> SemanticEquality for TopLevelElementNode<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.inner.semantic_equals(&other.inner)
    }
}

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
    implementation: StatementNode<Type>,
}

impl<Type: ASTType> Function<Type> {
    pub fn new(declaration: Rc<FunctionSymbol<Type>>, implementation: StatementNode<Type>) -> Self {
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

    pub fn implementation(&self) -> &StatementNode<Type> {
        &self.implementation
    }
}

impl<Type: ASTType> SemanticEquality for Function<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.declaration == other.declaration
            && self.implementation.semantic_equals(&other.implementation)
    }
}
