use crate::data_type::{DataType, Type};

pub trait SymbolTable<'a>: Iterator<Item=Symbol<'a>>
{

}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a>
{
    Function(&'a FunctionSymbol),
    Variable(&'a VariableSymbol)
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionSymbol
{

}

#[derive(Debug, Eq, PartialEq)]
pub struct VariableSymbol
{
    name: String,
    data_type: DataType
}

impl VariableSymbol
{
    pub fn new(name: String, data_type: DataType) -> Self
    {
        Self {
            name,
            data_type
        }
    }

    pub fn name(&self) -> &str
    {
        &self.name
    }
}

impl Type for VariableSymbol
{
    fn data_type(&self) -> DataType
    {
        self.data_type
    }
}