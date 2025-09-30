use std::fmt::Debug;
use std::rc::Rc;
use crate::statement::Statement;
use crate::symbol::FunctionSymbol;

/** This is an arbitiary top-level construct
For now, there are only functions
*/
#[derive(Debug)]
pub enum TopLevelElement
{
    Function(Function)
}

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
        Self {
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
}