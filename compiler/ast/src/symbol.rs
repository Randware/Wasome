use crate::data_type::{DataType, Typed};
use crate::expression::Expression;
use crate::id::Id;
use crate::{ASTNode, ASTType, SemanticEquality, TypedAST, UntypedAST};
use std::rc::Rc;

/// Has symbols available to use
/// 
/// # Composition of the provided data
/// 
/// The provided data consists of two parts:
/// 1. The required prefix
///
///     Specifies what prefix has to be used when using this symbol.
///     For example, here "math" is the prefix:
///
///     math.floor(10.5)
///
///     This is none if no prefix is required. It is used for the module name when the symbol was imported.
///
/// 2. The symbol
///     This is simply the symbol that is available.
pub trait SymbolTable<'a, Type: ASTType>: Iterator<Item = (Option<&'a str>, Symbol<'a, Type>)> {}

/// A reference to a symbol
/// 
/// The data is only owned to allow for efficient creation 
/// of instanced of this without giving up type safety 
/// when storing concrete symbols (e.g.: VariableSymbols)
#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a, Type: ASTType> {
    Function(&'a FunctionSymbol<Type>),
    Variable(&'a VariableSymbol<Type>),
}

impl<'a, Type: ASTType> Symbol<'a, Type>
{
    /// Gets the name of a symbol. This name is not directly stored in the enum but in the variant.
    /// # Return
    /// The name
    pub fn name(&self) -> &str {
        match self {
            Symbol::Function(func) => func.name(),
            Symbol::Variable(var) => var.name()
        }
    }
}

/// A function symbol
///
/// # Equality
///
/// Two different FunctionSymbols are never equal
/// - Note that semantic equality is the same as regular equality
///     - Semantic equality checks if two ast parts have the same semantic meaning. Two different symbols
///       on their own have diffrent semantic meanings even if their names are equal
#[derive(Debug, Eq, PartialEq)]
pub struct FunctionSymbol<Type: ASTType> {
    id: Id,
    name: String,
    // None = no return type/void
    return_type: Option<Type::GeneralDataType>,
    params: Vec<Rc<VariableSymbol<Type>>>,
}

/// A variable symbol
/// 
/// # Equality
///
/// Two different VariableSymbols are never equal
/// - Note that semantic equality is the same as regular equality
///     - Semantic equality checks if two ast parts have the same semantic meaning. Two different symbols
///       on their own have diffrent semantic meanings even if their names are equal
#[derive(Debug, Eq, PartialEq)]
pub struct VariableSymbol<Type: ASTType> {
    id: Id,
    name: String,
    data_type: Type::GeneralDataType,
}

impl<Type: ASTType> VariableSymbol<Type> {
    pub fn new(name: String, data_type: Type::GeneralDataType) -> Self {
        Self {
            id: Id::new(),
            name,
            data_type,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn data_type(&self) -> &Type::GeneralDataType {
        &self.data_type
    }
}

impl Typed for VariableSymbol<TypedAST> {
    fn data_type(&self) -> DataType {
        self.data_type
    }
}

impl<Type: ASTType> FunctionSymbol<Type> {
    pub fn new(
        name: String,
        return_type: Option<Type::GeneralDataType>,
        params: Vec<Rc<VariableSymbol<Type>>>,
    ) -> Self {
        Self {
            id: Id::new(),
            name,
            return_type,
            params,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> &[Rc<VariableSymbol<Type>>] {
        &self.params
    }

    pub fn return_type(&self) -> Option<&Type::GeneralDataType> {
        self.return_type.as_ref()
    }
}
