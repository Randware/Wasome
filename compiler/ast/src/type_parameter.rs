use crate::data_type::DataType;
use crate::symbol::UntypedTypeParameterSymbol;
use std::rc::Rc;

/** A type parameter in an untyped AST
*/
#[derive(Debug, PartialEq)]
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

/** A type parameter in an typed AST
*/
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
