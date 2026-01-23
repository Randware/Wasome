use crate::symbol::{EnumSymbol, StructSymbol};
use crate::{SemanticEq, TypedAST};
use std::rc::Rc;

/// A data type
///
/// This only exists in the typed AST
/// Therefore, having type parameters here does not make sense as they are part of the composite
/// identifier (and not data types) in the typed AST
#[derive(Debug, Clone, PartialEq)]
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

/// A syntax element in wasome with a data type.
///
/// This trait only is ever implemented for parts of the typed AST
///
/// Examples include expressions and operators
pub trait Typed {
    /// Gets the data type
    fn data_type(&self) -> DataType;
}

impl SemanticEq for DataType {
    fn semantic_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DataType::Struct(self_struct), DataType::Struct(other_struct)) => {
                self_struct.semantic_eq(other_struct)
            }
            (DataType::Enum(self_enum), DataType::Enum(other_enum)) => {
                self_enum.semantic_eq(other_enum)
            }
            _ => self == other,
        }
    }
}
