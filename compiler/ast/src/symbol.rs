use crate::data_type::{DataType, Typed};
use crate::id::Id;
use crate::{ASTType, SemanticEquality, TypedAST};
use std::rc::Rc;

/// Has symbols available to use
///
/// # Composition of the provided data
///
/// The provided data consists of two parts:
/// 1. The module usage name
///    Specifies in what module name or alias given by the import the symbol is.
///    For example, here "math" is the prefix:
///
///    math.floor(10.5)
///
///    This is none if no prefix is required. It is used for the module name when the symbol was imported.
///
/// 2. The symbol
///    This is simply the symbol that is available.
///
/// # Equality
///
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEquality`] for the usual
/// PartialEq behavior instead
pub trait SymbolTable<'a, Type: ASTType>:
    Iterator<Item = (Option<&'a ModuleUsageNameSymbol>, Symbol<'a, Type>)>
{
}

/// A reference to a symbol
///
/// The data is only owned to allow for efficient creation
/// of instanced of this without giving up type safety
/// when storing concrete symbols (e.g.: VariableSymbols)
#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a, Type: ASTType> {
    Function(&'a FunctionSymbol<Type>),
    Variable(&'a VariableSymbol<Type>),
    ModuleUsageName(&'a ModuleUsageNameSymbol),
}

impl<'a, Type: ASTType> Symbol<'a, Type> {
    /// Gets the name of a symbol. This name is not directly stored in the enum but in the variant.
    /// # Return
    /// The name
    pub fn name(&self) -> &str {
        match self {
            Symbol::Function(func) => func.name(),
            Symbol::Variable(var) => var.name(),
            Symbol::ModuleUsageName(mun) => mun.name(),
        }
    }
}

impl<Type: ASTType> SemanticEquality for Symbol<'_, Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        use Symbol as S;
        match (self, other) {
            (S::Function(lhs), S::Function(rhs)) => lhs.semantic_equals(rhs),
            (S::Variable(lhs), S::Variable(rhs)) => lhs.semantic_equals(rhs),
            (S::ModuleUsageName(lhs), S::ModuleUsageName(rhs)) => lhs.semantic_equals(rhs),
            _ => false,
        }
    }
}

/// A module usage name symbol
///
/// It stores the usage name of a module, that is required to access imported symbols from this module.
/// For example, in the following snippet, `trigonometry` is a module usage name
/// `trigonometry.sin_degrees(20.0)`
///
/// # Equality
///
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEquality`] for the usual
/// PartialEq behavior instead
#[derive(Debug, Eq, PartialEq)]
pub struct ModuleUsageNameSymbol {
    id: Id,
    name: String,
}

impl ModuleUsageNameSymbol {
    pub fn new(name: String) -> Self {
        Self {
            id: Id::new(),
            name,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl SemanticEquality for ModuleUsageNameSymbol {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name().semantic_equals(other.name())
    }
}

/// A function symbol
///
/// # Equality
///
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEquality`] for the usual
/// PartialEq behavior instead
#[derive(Debug, Eq, PartialEq)]
pub struct FunctionSymbol<Type: ASTType> {
    id: Id,
    name: String,
    // None = no return type/void
    return_type: Option<Type::GeneralDataType>,
    params: Vec<Rc<VariableSymbol<Type>>>,
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

impl<Type: ASTType> SemanticEquality for FunctionSymbol<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name().semantic_equals(other.name())
            && self.return_type().semantic_equals(&other.return_type())
            && self.params().semantic_equals(self.params())
    }
}

/// A variable symbol
///
/// # Equality
///
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEquality`] for the usual
/// PartialEq behavior instead
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

impl<Type: ASTType> SemanticEquality for VariableSymbol<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name().semantic_equals(other.name())
            && self.data_type().semantic_equals(other.data_type())
    }
}
