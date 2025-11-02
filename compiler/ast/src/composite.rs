use std::rc::Rc;
use crate::{ASTNode, ASTType};
use crate::symbol::{EnumSymbol, StructSymbol};
use crate::top_level::Function;

/** An enum
*/
#[derive(Debug, PartialEq)]
pub struct Enum<Type: ASTType>
{
    symbol: Rc<EnumSymbol<Type>>
}

impl<Type: ASTType> Enum<Type>
{
    pub fn new(symbol: Rc<EnumSymbol<Type>>) -> Self {
        Self { symbol }
    }

    pub fn symbol(&self) -> &EnumSymbol<Type> {
        &self.symbol
    }

    pub fn symbol_owned(&self) -> Rc<EnumSymbol<Type>> {
        self.symbol.clone()
    }
}

/** A variant of an enum
*/
#[derive(Debug, PartialEq)]
pub struct EnumVariant<Type: ASTType>
{
    name: String,
    fields: Vec<ASTNode<Type::GeneralDataType>>
}

impl<Type: ASTType> EnumVariant<Type>
{
    pub fn new(name: String, fields: Vec<ASTNode<Type::GeneralDataType>>) -> Self {
        Self { name, fields }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn fields(&self) -> &Vec<ASTNode<Type::GeneralDataType>> {
        &self.fields
    }
}

/** A struct
*/
#[derive(Debug, PartialEq)]
pub struct Struct<Type: ASTType>
{
    symbol: Rc<StructSymbol<Type>>,
    functions: Vec<Function<Type>>,
}

impl<Type: ASTType> Struct<Type>
{
    pub fn new(symbol: Rc<StructSymbol<Type>>, functions: Vec<Function<Type>>) -> Self {
        Self { symbol, functions }
    }

    pub fn symbol(&self) -> &StructSymbol<Type> {
        &self.symbol
    }

    pub fn symbol_owned(&self) -> Rc<StructSymbol<Type>> {
        self.symbol.clone()
    }

    pub fn functions(&self) -> &Vec<Function<Type>> {
        &self.functions
    }
}

/** A field of a struct
*/
#[derive(Debug, PartialEq)]
pub struct StructField<Type: ASTType>
{
    name: String,
    data_type: Type::GeneralDataType
}

impl<Type: ASTType> StructField<Type>
{
    pub fn new(name: String, data_type: Type::GeneralDataType) -> Self {
        Self { name, data_type }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn data_type(&self) -> &Type::GeneralDataType {
        &self.data_type
    }
}