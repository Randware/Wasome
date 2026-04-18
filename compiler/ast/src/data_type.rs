use crate::symbol::{EnumSymbol, StructSymbol};
use crate::{SemanticEq, TypedAST};
use std::rc::Rc;

/// A data type
///
/// This only exists in the typed AST
/// Therefore, having type parameters here does not make sense as they are part of the composite
/// identifier (and not data types) in the typed AST
///
/// Despite not being [`Copy`], this is still fairly cheap to clone.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataType {
    Char,
    U8,
    S8,
    U16,
    S16,
    U32,
    S32,
    U64,
    S64,
    Bool,
    F32,
    F64,
    Struct(Rc<StructSymbol<TypedAST>>),
    Enum(Rc<EnumSymbol<TypedAST>>),
}

impl DataType {
    pub fn is_float(&self) -> bool {
        match self {
            DataType::F32 |
            DataType::F64 => true,
            _ => false
        }
    }

    pub fn is_sint(&self) -> bool {
        match self {
            DataType::S8 |
            DataType::S16 |
            DataType::S32 |
            DataType::S64 => true,
            _ => false
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            DataType::Bool => true,
            _ => false
        }
    }

    pub fn is_prt(&self) -> bool {
        match self {
            DataType::Struct(_) | DataType::Enum(_) => true,
            _ => false
        }
    }
    
    pub fn size_bytes(&self) -> usize {
        match self {
            DataType::Bool |
            DataType::U8 |
            DataType::S8 => 1,
            DataType::U16 |
            DataType::S16 => 2,
            DataType::Char |
            DataType::U32 |
            DataType::S32 |
            DataType::F32 => 4,
            DataType::U64 |
            DataType::S64 |
            DataType::F64 |
            DataType::Struct(_) |
            DataType::Enum(_) => 4
        }
    }
}

/// A syntax element in wasome with a data type.
///
/// This trait only is ever implemented for parts of the typed AST
///
/// Examples include expressions and operators
pub trait Typed {
    /// Gets the data type
    #[must_use]
    fn data_type(&self) -> DataType;
}

impl SemanticEq for DataType {
    fn semantic_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Struct(self_struct), Self::Struct(other_struct)) => {
                self_struct.semantic_eq(other_struct)
            }
            (Self::Enum(self_enum), Self::Enum(other_enum)) => self_enum.semantic_eq(other_enum),
            _ => self == other,
        }
    }
}

/// A data type in an untyped AST
///
/// This is context-dependant
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct UntypedDataType {
    name: String,
    type_parameters: Vec<UntypedDataType>,
}

impl UntypedDataType {
    #[must_use]
    pub const fn new(name: String, type_parameters: Vec<Self>) -> Self {
        Self {
            name,
            type_parameters,
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn type_parameters(&self) -> &[Self] {
        &self.type_parameters
    }
}

impl SemanticEq for UntypedDataType {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.type_parameters().semantic_eq(other.type_parameters())
    }
}
