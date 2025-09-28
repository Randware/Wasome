use crate::top_level::Function;

pub mod expression;
pub mod statement;
pub mod block;
pub mod top_level;
pub mod symbol;
pub mod data_type;

pub struct AST
{
    functions: Vec<Function>
}

impl AST
{
    pub fn new(functions: Vec<Function>) -> Self
    {
        Self
        {
            functions
        }
    }

    pub fn functions(&self) -> &[Function]
    {
        &self.functions
    }
}


/** This compares two values
This is useful for returning with the ? operator if values are not equal
@params
left, right: The values to compare
@return
None if not equal
Some if equal
*/
fn eq_return_option<T: PartialEq>(left: T, right: T) -> Option<()>
{
    if left == right
    {
        return Some(())
    }
    None
}
