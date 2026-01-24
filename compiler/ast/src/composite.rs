use crate::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol,
    TypeParameterSymbol,
};
use crate::top_level::Function;
use crate::visibility::{Visibility, Visible};
use crate::{ASTNode, ASTType, SemanticEq};
use std::rc::Rc;

/// An enum
#[derive(Debug, PartialEq)]
pub struct Enum<Type: ASTType> {
    symbol: Rc<EnumSymbol<Type>>,
    variants: Vec<ASTNode<EnumVariant<Type>>>,
    visibility: Visibility,
}

impl<Type: ASTType> Enum<Type> {
    pub fn new(
        symbol: Rc<EnumSymbol<Type>>,
        variants: Vec<ASTNode<EnumVariant<Type>>>,
        visibility: Visibility,
    ) -> Self {
        Self {
            symbol,
            variants,
            visibility,
        }
    }

    pub fn symbol(&self) -> &EnumSymbol<Type> {
        &self.symbol
    }

    pub fn symbol_owned(&self) -> Rc<EnumSymbol<Type>> {
        self.symbol.clone()
    }

    pub fn variants(&self) -> &[ASTNode<EnumVariant<Type>>] {
        &self.variants
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    /// Gets the enum variant with the specified name
    ///
    /// # Errors
    ///
    /// There is no variant with `name`
    pub fn variant_by_name(&self, name: &str) -> Option<&ASTNode<EnumVariant<Type>>> {
        self.variants()
            .iter()
            .find(|var| var.inner().name() == name)
    }

    /// Gets an iterator over all variants
    pub fn variants_iterator(&self) -> impl Iterator<Item = &ASTNode<EnumVariant<Type>>> {
        self.variants().iter()
    }
}

impl<Type: ASTType> SemanticEq for Enum<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.symbol().semantic_eq(other.symbol()) && self.variants().semantic_eq(other.variants())
    }
}

/// A variant of an enum.
///
/// In contrast to the symbol, this is supposed to be used for definitions
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

impl<Type: ASTType> SemanticEq for EnumVariant<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.inner().semantic_eq(other.inner())
    }
}

/// A struct
#[derive(Debug, PartialEq)]
pub struct Struct<Type: ASTType> {
    symbol: Rc<StructSymbol<Type>>,
    functions: Vec<ASTNode<Function<Type>>>,
    fields: Vec<ASTNode<StructField<Type>>>,
    visibility: Visibility,
}

impl<Type: ASTType> Struct<Type> {
    pub fn new(
        symbol: Rc<StructSymbol<Type>>,
        functions: Vec<ASTNode<Function<Type>>>,
        fields: Vec<ASTNode<StructField<Type>>>,
        visibility: Visibility,
    ) -> Self {
        Self {
            symbol,
            functions,
            fields,
            visibility,
        }
    }

    pub fn symbol(&self) -> &StructSymbol<Type> {
        &self.symbol
    }

    pub fn symbol_owned(&self) -> Rc<StructSymbol<Type>> {
        self.symbol.clone()
    }

    pub fn functions(&self) -> &Vec<ASTNode<Function<Type>>> {
        &self.functions
    }

    pub fn fields(&self) -> &[ASTNode<StructField<Type>>] {
        &self.fields
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    /// Gets the function with the specified name
    pub fn function_by_name(&self, name: &str) -> Option<&ASTNode<Function<Type>>> {
        self.functions()
            .iter()
            .find(|function| function.declaration().name() == name)
    }

    /// Gets the function with the specified name if it is public or only_public is false
    pub fn function_symbol(&self, name: &str, only_public: bool) -> Option<&FunctionSymbol<Type>> {
        self.function_by_name(name)
            .filter(|function| !only_public || function.visibility() == Visibility::Public)
            .map(|function| function.declaration())
    }
}

impl<Type: ASTType> SemanticEq for Struct<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.symbol().semantic_eq(other.symbol())
            && self.functions().semantic_eq(other.functions())
            && self.fields().semantic_eq(other.fields())
    }
}

/// A field of a struct
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct StructField<Type: ASTType> {
    inner: Rc<StructFieldSymbol<Type>>,
    visibility: Visibility,
}

impl<Type: ASTType> StructField<Type> {
    pub fn new(inner: Rc<StructFieldSymbol<Type>>, visibility: Visibility) -> Self {
        Self { inner, visibility }
    }
    pub fn inner(&self) -> &StructFieldSymbol<Type> {
        &self.inner
    }

    pub fn inner_owned(&self) -> Rc<StructFieldSymbol<Type>> {
        self.inner.clone()
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }
}

impl<Type: ASTType> SemanticEq for StructField<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.inner.name() == other.inner.name() && self.inner().semantic_eq(other.inner())
    }
}
