use std::rc::Rc;
use crate::statement::{Statement, StatementRef};
use crate::symbol::FunctionSymbol;

#[derive(Debug)]
pub struct Function
{
    declaration: Rc<FunctionSymbol>,
    implementation: Statement
}

impl Function
{
    pub fn new(declaration: Rc<FunctionSymbol>, implementation: Statement) -> Self
    {
        Self
        {
            declaration,
            implementation
        }
    }

    pub fn declaration(&self) -> &FunctionSymbol
    {
        &self.declaration
    }

    /** Gets the declaration by cloning the rc
    */
    pub fn declaration_owned(&self) -> Rc<FunctionSymbol>
    {
        self.declaration.clone()
    }

    pub fn implementation(&self) -> &Statement
    {
        &self.implementation
    }

    /** Gets a StatementRef for the top level statement in this function
    This is the intended way to traverse a function
    */
    pub fn ref_to_implementation(&self) -> StatementRef
    {
        StatementRef::new_root(self)
    }
}