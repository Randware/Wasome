use crate::ASTType;
use crate::statement::Statement;
use crate::symbol::FunctionSymbol;
use std::fmt::Debug;
use std::rc::Rc;

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
    implementation: Statement<Type>,
}

impl<Type: ASTType> Function<Type> {
    pub fn new(declaration: Rc<FunctionSymbol<Type>>, implementation: Statement<Type>) -> Self {
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

    pub fn implementation(&self) -> &Statement<Type> {
        &self.implementation
    }
}
