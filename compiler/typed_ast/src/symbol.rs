use std::rc::Rc;
use crate::ASTType;

/**  Any type that has symbols available for use
*/
pub trait SymbolTable<'a, Type: ASTType>: Iterator<Item=Symbol<'a, Type>>
{

}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a, Type: ASTType>
{
    Function(&'a FunctionSymbol<Type>),
    Variable(&'a VariableSymbol<Type>)
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionSymbol<Type: ASTType>
{
    name: String,
    // None = no return type/void
    return_type: Option<Type::GeneralDataType>,
    params: Vec<Rc<VariableSymbol<Type>>>
}

#[derive(Debug, Eq, PartialEq)]
pub struct VariableSymbol<Type: ASTType>
{
    name: String,
    data_type: Type::GeneralDataType
}

impl<Type: ASTType> VariableSymbol<Type>
{
    pub fn new(name: String, data_type: Type::GeneralDataType) -> Self
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

    pub fn data_type(&self) -> &Type::GeneralDataType
    {
        &self.data_type
    }
}

impl<Type: ASTType> FunctionSymbol<Type>
{
    pub fn new(name: String, return_type: Option<Type::GeneralDataType>, params: Vec<Rc<VariableSymbol<Type>>>) -> Self
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

    pub fn params(&self) -> &[Rc<VariableSymbol<Type>>]
    {
        &self.params
    }

    pub fn return_type(&self) -> Option<&Type::GeneralDataType>
    {
        self.return_type.as_ref()
    }
}