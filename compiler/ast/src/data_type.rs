use crate::SemanticEquality;

/// A data type
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq)]
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

impl SemanticEquality for DataType {
    fn semantic_equals(&self, other: &Self) -> bool {
        // No ids
        self == other
    }
}
