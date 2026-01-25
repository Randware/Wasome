use crate::SemanticEq;
use crate::data_type::DataType;
use crate::symbol::UntypedTypeParameterSymbol;
use std::rc::Rc;

/// A type parameter in an untyped AST
///
/// This is a data type that is available in the composite that uses it
#[derive(Debug, PartialEq, Clone)]
pub struct UntypedTypeParameter {
    inner: Rc<UntypedTypeParameterSymbol>,
}

impl UntypedTypeParameter {
    pub fn new(inner: Rc<UntypedTypeParameterSymbol>) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &UntypedTypeParameterSymbol {
        &self.inner
    }

    pub fn inner_owned(&self) -> Rc<UntypedTypeParameterSymbol> {
        self.inner.clone()
    }
}

impl SemanticEq for UntypedTypeParameter {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.inner().semantic_eq(other.inner())
    }
}

/// A type parameter in a typed AST
///
/// This is part of the composite identifier
#[derive(Debug, PartialEq)]
pub struct TypedTypeParameter {
    name: String,
    data_type: DataType,
}

impl TypedTypeParameter {
    pub fn new(name: String, data_type: DataType) -> Self {
        Self { name, data_type }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn data_type(&self) -> &DataType {
        &self.data_type
    }
}

impl SemanticEq for TypedTypeParameter {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.data_type().semantic_eq(other.data_type())
    }
}
