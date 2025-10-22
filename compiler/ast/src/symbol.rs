use crate::data_type::Typed;
use crate::expression::ExpressionNode;
use crate::id::Id;
use crate::{ASTType, SemanticEquality, TypedAST, UntypedAST};
use std::rc::Rc;

/**  Any type that has symbols available for use
*/
pub trait SymbolTable<'a, Type: ASTType>: Iterator<Item = Symbol<'a, Type>> {}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a, Type: ASTType> {
    Function(&'a FunctionSymbol<Type>),
    Variable(&'a VariableSymbol<Type>),
}

/** A function symbol
# Equality
Two different FunctionSymbols are never equal
*/
#[derive(Debug, Eq, PartialEq)]
pub struct FunctionSymbol<Type: ASTType> {
    id: Id,
    name: String,
    // None = no return type/void
    return_type: Option<Type::GeneralDataType>,
    params: Vec<Rc<VariableSymbol<Type>>>,
}

/** A variable symbol
# Equality
Two different VariableSymbols are never equal
*/
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

/** A function call with params
*/
#[derive(Debug, PartialEq)]
pub struct FunctionCall<Type: ASTType> {
    function: Type::FunctionCallSymbol,
    args: Vec<ExpressionNode<Type>>,
}

impl<Type: ASTType> FunctionCall<Type> {
    pub fn function(&self) -> &Type::FunctionCallSymbol {
        &self.function
    }

    pub fn args(&self) -> &Vec<ExpressionNode<Type>> {
        &self.args
    }
}

impl<Type: ASTType> SemanticEquality for FunctionCall<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.function == other.function && self.args.semantic_equals(&other.args)
    }
}

impl FunctionCall<TypedAST> {
    /** Creates a new function call
    Checks if the provided and expected params are the same number and have the same data types
    Returns None if these checks failed
    Some(new instance) otherwise
    */
    pub fn new(
        function: Rc<FunctionSymbol<TypedAST>>,
        args: Vec<ExpressionNode<TypedAST>>,
    ) -> Option<Self> {
        if function.params().len() != args.len()
            || !function
                .params()
                .iter()
                .zip(args.iter())
                .all(|(expected, provided)| *expected.data_type() == provided.data_type())
        {
            return None;
        }
        Some(Self { function, args })
    }
}

impl FunctionCall<UntypedAST> {
    /** Creates a new function call
     */
    pub fn new(function: String, args: Vec<ExpressionNode<UntypedAST>>) -> Self {
        Self { function, args }
    }
}
