use crate::ASTType;
use crate::statement::{Statement, StatementNode};
use crate::symbol::FunctionSymbol;
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub struct TopLevelElementNode<Type: ASTType> {
    inner: TopLevelElement<Type>,
}

impl<Type: ASTType> TopLevelElementNode<Type> {
    pub fn new(inner: TopLevelElement<Type>) -> Self {
        Self { inner }
    }
}

impl<Type: ASTType> Deref for TopLevelElementNode<Type> {
    type Target = TopLevelElement<Type>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/** This is an arbitiary top-level construct
For now, there are only functions
*/
#[derive(Debug)]
pub enum TopLevelElement<Type: ASTType> {
    Function(Function<Type>),
}

#[derive(Debug)]
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
