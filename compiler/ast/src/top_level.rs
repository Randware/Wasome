use crate::statement::Statement;
use crate::symbol::FunctionSymbol;
use crate::{ASTNode, ASTType, SemanticEquality};
use std::fmt::Debug;
use std::rc::Rc;

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

/** An import
In the typed AST, this has no semantic meaning and is only there to not lose any information
*/
#[derive(Debug, PartialEq)]
pub struct Import {
    root: ImportRoot,
    path: Vec<String>, // Maybe OSString TODO
}

impl Import {
    pub fn new(root: ImportRoot, path: Vec<String>) -> Self {
        Self { root, path }
    }

    pub fn root(&self) -> &ImportRoot {
        &self.root
    }

    pub fn path(&self) -> &Vec<String> {
        &self.path
    }
}

impl SemanticEquality for Import {
    fn semantic_equals(&self, other: &Self) -> bool {
        // Semantic equality is equal to regular equality
        self == other
    }
}

#[derive(Debug, PartialEq)]
pub enum ImportRoot {
    CurrentDirectory,
    ProjectRoot,
}
