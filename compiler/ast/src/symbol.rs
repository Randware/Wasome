use crate::data_type::DataType;

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
    name: String,
    // None = no return type/void
    return_type: Option<DataType>,
    params: Vec<VariableSymbol>
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

    pub fn data_type(&self) -> DataType
    {
        self.data_type
    }
}

impl FunctionSymbol
{
    pub fn new(name: String, return_type: Option<DataType>, params: Vec<VariableSymbol>) -> Self
    {
        Self {
            name,
            return_type,
            params
        }
    }

    pub fn name(&self) -> &str
    {
        &self.name
    }

    pub fn params(&self) -> &[VariableSymbol]
    {
        &self.params
    }

    pub fn return_type(&self) -> Option<DataType>
    {
        self.return_type
    }
}