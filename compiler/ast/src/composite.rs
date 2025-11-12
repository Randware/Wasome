use crate::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol,
};
use crate::top_level::Function;
use crate::visibility::{Visibility, Visible};
use crate::{ASTNode, ASTType, SemanticEquality};
use std::rc::Rc;

/** An enum
*/
#[derive(Debug, PartialEq)]
pub struct Enum<Type: ASTType> {
    symbol: Rc<EnumSymbol>,
    variants: Vec<ASTNode<EnumVariant<Type>>>,
}

impl<Type: ASTType> Enum<Type> {
    pub fn new(symbol: Rc<EnumSymbol>, variants: Vec<ASTNode<EnumVariant<Type>>>) -> Self {
        Self { symbol, variants }
    }

    pub fn symbol(&self) -> &EnumSymbol {
        &self.symbol
    }

    pub fn symbol_owned(&self) -> Rc<EnumSymbol> {
        self.symbol.clone()
    }

    pub fn variants(&self) -> &[ASTNode<EnumVariant<Type>>] {
        &self.variants
    }

    /** Gets the struct with the specified name
     */
    pub fn variant_by_name(&self, name: &str) -> Option<&ASTNode<EnumVariant<Type>>> {
        self.variants()
            .iter()
            .find(|var| var.inner().name() == name)
    }

    /** Gets an iterator over all enums
     */
    pub fn variants_iterator(&self) -> impl Iterator<Item = &ASTNode<EnumVariant<Type>>> {
        self.variants().iter()
    }
}

impl<Type: ASTType> SemanticEquality for Enum<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.symbol() == other.symbol()
    }
}

/** A variant of an enum.

In contrast to the symbol, this is supposed to be used for definitions
*/
#[derive(Debug, PartialEq)]
pub struct EnumVariant<Type: ASTType> {
    inner: Rc<EnumVariantSymbol<Type>>,
}

impl<Type: ASTType> EnumVariant<Type> {
    pub fn new(inner: Rc<EnumVariantSymbol<Type>>) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &EnumVariantSymbol<Type> {
        &self.inner
    }

    pub fn inner_owned(&self) -> &Rc<EnumVariantSymbol<Type>> {
        &self.inner
    }
}

impl<Type: ASTType> SemanticEquality for EnumVariant<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        let symbol = self.inner();
        let other_symbol = other.inner();
        symbol.name() == other_symbol.name()
            && symbol.fields().len() == other_symbol.fields().len()
            && symbol
                .fields()
                .iter()
                .zip(other_symbol.fields().iter())
                .all(|(self_fields, other_fields)| self_fields == other_fields)
    }
}

/** A struct
*/
#[derive(Debug, PartialEq)]
pub struct Struct<Type: ASTType> {
    symbol: Rc<StructSymbol>,
    functions: Vec<ASTNode<Function<Type>>>,
    fields: Vec<ASTNode<StructField<Type>>>,
}

impl<Type: ASTType> Struct<Type> {
    pub fn new(
        symbol: Rc<StructSymbol>,
        functions: Vec<ASTNode<Function<Type>>>,
        fields: Vec<ASTNode<StructField<Type>>>,
    ) -> Self {
        Self {
            symbol,
            functions,
            fields,
        }
    }

    pub fn symbol(&self) -> &StructSymbol {
        &self.symbol
    }

    pub fn symbol_owned(&self) -> Rc<StructSymbol> {
        self.symbol.clone()
    }

    pub fn functions(&self) -> &Vec<ASTNode<Function<Type>>> {
        &self.functions
    }

    /** Gets the function with the specified name
     */
    pub fn function_by_name(&self, name: &str) -> Option<&ASTNode<Function<Type>>> {
        self.functions()
            .iter()
            .find(|function| function.declaration().name() == name)
    }

    /** Gets the function with the specified name if it is public or only_public is false
     */
    pub(crate) fn function_symbol(
        &self,
        name: &str,
        only_public: bool,
    ) -> Option<&FunctionSymbol<Type>> {
        self.function_by_name(name)
            .filter(|function| !only_public || function.visibility() == Visibility::Public)
            .map(|function| function.declaration())
    }
}

impl<Type: ASTType> SemanticEquality for Struct<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.symbol == other.symbol
            && self.functions().semantic_equals(other.functions())
            && self.functions.semantic_equals(other.functions())
    }
}

/** A field of a struct
*/
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct StructField<Type: ASTType> {
    inner: Rc<StructFieldSymbol<Type>>,
}

impl<Type: ASTType> StructField<Type> {
    pub fn new(inner: Rc<StructFieldSymbol<Type>>) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &StructFieldSymbol<Type> {
        &self.inner
    }

    pub fn inner_owned(&self) -> Rc<StructFieldSymbol<Type>> {
        self.inner.clone()
    }
}

impl<Type: ASTType> SemanticEquality for StructField<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.inner.name() == other.inner.name()
            && self.inner().data_type() == other.inner().data_type()
    }
}
