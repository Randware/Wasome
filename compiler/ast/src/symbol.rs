use crate::data_type::{DataType, Typed};
use crate::id::Id;
use crate::{ASTType, SemanticEq, SymbolIdentifier, TypedAST};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
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
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEq`] for the usual
/// PartialEq behavior instead
pub trait SymbolTable<'a, Type: ASTType>:
    Iterator<
    Item = (
        Option<&'a ModuleUsageNameSymbol>,
        DirectlyAvailableSymbol<'a, Type>,
    ),
>
{
}

/// A reference to a directly accessible symbol symbol
///
/// Examples of non-DirectlyAvailableSymbols include EnumVariantSymbolss and StructFieldSymbols
///
/// The data is only owned to allow for efficient creation
/// of instanced of this without giving up type safety
/// when storing concrete symbols (e.g.: VariableSymbols)
#[derive(Debug)]
pub enum DirectlyAvailableSymbol<'a, Type: ASTType> {
    Function(&'a FunctionSymbol<Type>),
    Variable(&'a VariableSymbol<Type>),
    Enum(&'a EnumSymbol<Type>),
    Struct(&'a StructSymbol<Type>),
    ModuleUsageName(&'a ModuleUsageNameSymbol),
    /// Only valid in the untyped AST
    /// In the typed AST, type parameters are part of the composite identifier
    UntypedTypeParameter(&'a UntypedTypeParameterSymbol),
}

impl<'a, Type: ASTType> DirectlyAvailableSymbol<'a, Type> {
    /// Gets the name of a symbol. This name is not directly stored in the enum but in the variant.
    /// # Return
    /// The name
    pub fn name(&self) -> &str {
        match self {
            DirectlyAvailableSymbol::Function(func) => func.name(),
            DirectlyAvailableSymbol::Variable(var) => var.name(),
            DirectlyAvailableSymbol::Enum(en) => en.name(),
            DirectlyAvailableSymbol::Struct(st) => st.name(),
            DirectlyAvailableSymbol::ModuleUsageName(mun) => mun.name(),
            DirectlyAvailableSymbol::UntypedTypeParameter(mun) => mun.name(),
        }
    }

    pub fn matches_identifier(&self, identifier: Type::SymbolIdentifier<'_>) -> bool {
        match self {
            DirectlyAvailableSymbol::Function(func) => {
                Type::symbol_with_type_parameter_matches_identifier(identifier, *func)
            }
            DirectlyAvailableSymbol::Enum(en) => {
                Type::symbol_with_type_parameter_matches_identifier(identifier, *en)
            }
            DirectlyAvailableSymbol::Struct(st) => {
                Type::symbol_with_type_parameter_matches_identifier(identifier, *st)
            }
            _ => self.name() == identifier.name() && identifier.count_type_parameters() == 0,
        }
    }
}

// We want to implement traits without all parts implementing them as well.
// Deriving isn't possible in this case
impl<Type: ASTType> Clone for DirectlyAvailableSymbol<'_, Type> {
    fn clone(&self) -> Self {
        match self {
            DirectlyAvailableSymbol::Function(func) => DirectlyAvailableSymbol::Function(func),
            DirectlyAvailableSymbol::Variable(var) => DirectlyAvailableSymbol::Variable(var),
            DirectlyAvailableSymbol::Enum(en) => DirectlyAvailableSymbol::Enum(en),
            DirectlyAvailableSymbol::Struct(st) => DirectlyAvailableSymbol::Struct(st),
            DirectlyAvailableSymbol::ModuleUsageName(mun) => {
                DirectlyAvailableSymbol::ModuleUsageName(mun)
            }
            DirectlyAvailableSymbol::UntypedTypeParameter(utp) => {
                DirectlyAvailableSymbol::UntypedTypeParameter(utp)
            }
        }
    }
}

impl<Type: ASTType> Hash for DirectlyAvailableSymbol<'_, Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            DirectlyAvailableSymbol::Function(func) => func.hash(state),
            DirectlyAvailableSymbol::Variable(var) => var.hash(state),
            DirectlyAvailableSymbol::Enum(en) => en.hash(state),
            DirectlyAvailableSymbol::Struct(st) => st.hash(state),
            DirectlyAvailableSymbol::ModuleUsageName(mun) => mun.hash(state),
            DirectlyAvailableSymbol::UntypedTypeParameter(utp) => utp.hash(state),
        }
    }
}

impl<Type: ASTType> SemanticEq for DirectlyAvailableSymbol<'_, Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        use DirectlyAvailableSymbol as S;
        match (self, other) {
            (S::Function(lhs), S::Function(rhs)) => lhs.semantic_eq(rhs),
            (S::Variable(lhs), S::Variable(rhs)) => lhs.semantic_eq(rhs),
            (S::Enum(lhs), S::Enum(rhs)) => lhs.semantic_eq(rhs),
            (S::Struct(lhs), S::Struct(rhs)) => lhs.semantic_eq(rhs),
            (S::ModuleUsageName(lhs), S::ModuleUsageName(rhs)) => lhs.semantic_eq(rhs),
            (S::UntypedTypeParameter(lhs), S::UntypedTypeParameter(rhs)) => lhs.semantic_eq(rhs),
            _ => false,
        }
    }
}

impl<Type: ASTType> PartialEq for DirectlyAvailableSymbol<'_, Type> {
    fn eq(&self, other: &Self) -> bool {
        use DirectlyAvailableSymbol as S;
        match (self, other) {
            (S::Function(lhs), S::Function(rhs)) => lhs == rhs,
            (S::Variable(lhs), S::Variable(rhs)) => lhs == rhs,
            (S::Enum(lhs), S::Enum(rhs)) => lhs == rhs,
            (S::Struct(lhs), S::Struct(rhs)) => lhs == rhs,
            (S::ModuleUsageName(lhs), S::ModuleUsageName(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl<Type: ASTType> Eq for DirectlyAvailableSymbol<'_, Type> {}

/// A symbol that has type parameters
///
/// Either a struct, enum or function
///
/// This exists to allow code to work on all three as they are extremely similar
pub trait SymbolWithTypeParameter<Type: ASTType>:
    Debug + PartialEq + SemanticEq + Eq + Hash
{
    fn name(&self) -> &str;
    fn id(&self) -> &Id;
    fn type_parameters(&self) -> &[Type::TypeParameterDeclaration];
}

/// A function symbol
///
/// # Equality
///
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEq`] for the usual
/// PartialEq behavior instead
#[derive(Debug, Clone)]
pub struct FunctionSymbol<Type: ASTType> {
    id: Id,
    name: String,
    // None = no return type/void
    return_type: Option<Type::GeneralDataType>,
    params: Vec<Rc<VariableSymbol<Type>>>,
    type_parameters: Vec<Type::TypeParameterDeclaration>,
}

impl<Type: ASTType> FunctionSymbol<Type> {
    pub fn new(
        name: String,
        return_type: Option<Type::GeneralDataType>,
        params: Vec<Rc<VariableSymbol<Type>>>,
        type_parameters: Vec<Type::TypeParameterDeclaration>,
    ) -> Self {
        Self {
            id: Id::new(),
            name,
            return_type,
            params,
            type_parameters,
        }
    }

    pub fn params(&self) -> &[Rc<VariableSymbol<Type>>] {
        &self.params
    }

    pub fn return_type(&self) -> Option<&Type::GeneralDataType> {
        self.return_type.as_ref()
    }
}

impl<Type: ASTType> SymbolWithTypeParameter<Type> for FunctionSymbol<Type> {
    fn name(&self) -> &str {
        &self.name
    }

    fn id(&self) -> &Id {
        &self.id
    }

    fn type_parameters(&self) -> &[Type::TypeParameterDeclaration] {
        &self.type_parameters
    }
}
impl<Type: ASTType> Hash for FunctionSymbol<Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for FunctionSymbol<Type> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for FunctionSymbol<Type> {}

impl<Type: ASTType> SemanticEq for FunctionSymbol<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name().semantic_eq(other.name())
            && self.return_type().semantic_eq(&other.return_type())
            && self.params().semantic_eq(self.params())
            && self.type_parameters().semantic_eq(other.type_parameters())
    }
}

/// A variable symbol
///
/// # Equality
///
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEq`] for the usual
/// PartialEq behavior instead
#[derive(Debug, Clone)]
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
        self.data_type.clone()
    }
}

impl<Type: ASTType> SemanticEq for VariableSymbol<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name().semantic_eq(other.name()) && self.data_type().semantic_eq(other.data_type())
    }
}

impl<Type: ASTType> Hash for VariableSymbol<Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for VariableSymbol<Type> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for VariableSymbol<Type> {}

/// A module usage name symbol
///
/// It stores the usage name of a module, that is required to access imported symbols from this module.
/// For example, in the following snippet, `trigonometry` is a module usage name
/// `trigonometry.sin_degrees(20.0)`
///
/// # Equality
///
/// Two different [`ModuleUsageNameSymbol`]s are never equal. Use [`SemanticEq`] for the usual
/// PartialEq behavior instead
#[derive(Debug, Eq, PartialEq, Clone)]
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

impl SemanticEq for ModuleUsageNameSymbol {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name().semantic_eq(other.name())
    }
}

impl Hash for ModuleUsageNameSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

/// The symbol of an enum
#[derive(Debug, Clone)]
pub struct EnumSymbol<Type: ASTType> {
    id: Id,
    name: String,
    type_parameters: Vec<Type::TypeParameterDeclaration>,
}

impl<Type: ASTType> EnumSymbol<Type> {
    pub fn new(name: String, type_parameters: Vec<Type::TypeParameterDeclaration>) -> Self {
        Self {
            id: Id::new(),
            name,
            type_parameters,
        }
    }
}
impl<Type: ASTType> SymbolWithTypeParameter<Type> for EnumSymbol<Type> {
    fn name(&self) -> &str {
        &self.name
    }

    fn id(&self) -> &Id {
        &self.id
    }

    fn type_parameters(&self) -> &[Type::TypeParameterDeclaration] {
        &self.type_parameters
    }
}
impl<Type: ASTType> Hash for EnumSymbol<Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for EnumSymbol<Type> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for EnumSymbol<Type> {}

impl<Type: ASTType> SemanticEq for EnumSymbol<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name().semantic_eq(other.name())
            && self.type_parameters().semantic_eq(other.type_parameters())
    }
}

/// A symbol for a struct
#[derive(Debug, Clone)]
pub struct StructSymbol<Type: ASTType> {
    id: Id,
    name: String,
    type_parameters: Vec<Type::TypeParameterDeclaration>,
}

impl<Type: ASTType> StructSymbol<Type> {
    pub fn new(name: String, type_parameters: Vec<Type::TypeParameterDeclaration>) -> Self {
        Self {
            id: Id::new(),
            name,
            type_parameters,
        }
    }
}
impl<Type: ASTType> SymbolWithTypeParameter<Type> for StructSymbol<Type> {
    fn name(&self) -> &str {
        &self.name
    }

    fn id(&self) -> &Id {
        &self.id
    }

    fn type_parameters(&self) -> &[Type::TypeParameterDeclaration] {
        &self.type_parameters
    }
}

impl<Type: ASTType> Hash for StructSymbol<Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for StructSymbol<Type> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for StructSymbol<Type> {}

impl<Type: ASTType> SemanticEq for StructSymbol<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name().semantic_eq(other.name())
            && self.type_parameters().semantic_eq(other.type_parameters())
    }
}

/// The symbol of an enum
#[derive(Debug, Clone)]
pub struct EnumVariantSymbol<Type: ASTType> {
    id: Id,
    name: String,
    fields: Vec<Type::GeneralDataType>,
}

impl<Type: ASTType> EnumVariantSymbol<Type> {
    pub fn new(name: String, fields: Vec<Type::GeneralDataType>) -> Self {
        Self {
            id: Id::new(),
            name,
            fields,
        }
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn fields(&self) -> &[Type::GeneralDataType] {
        &self.fields
    }
}

impl<Type: ASTType> Hash for EnumVariantSymbol<Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for EnumVariantSymbol<Type> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for EnumVariantSymbol<Type> {}

impl<Type: ASTType> SemanticEq for EnumVariantSymbol<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name().semantic_eq(other.name()) && self.fields().semantic_eq(other.fields())
    }
}

/// The symbol of an enum
#[derive(Debug, Clone)]
pub struct StructFieldSymbol<Type: ASTType> {
    id: Id,
    name: String,
    data_type: Type::GeneralDataType,
}

impl<Type: ASTType> StructFieldSymbol<Type> {
    pub fn new(name: String, data_type: Type::GeneralDataType) -> Self {
        Self {
            id: Id::new(),
            name,
            data_type,
        }
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn data_type(&self) -> &Type::GeneralDataType {
        &self.data_type
    }
}

impl<Type: ASTType> Hash for StructFieldSymbol<Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for StructFieldSymbol<Type> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for StructFieldSymbol<Type> {}

impl<Type: ASTType> SemanticEq for StructFieldSymbol<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name().semantic_eq(other.name()) && self.data_type().semantic_eq(other.data_type())
    }
}

/** The symbol of an untyped type parameter
*/
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct UntypedTypeParameterSymbol {
    id: Id,
    name: String,
}

impl UntypedTypeParameterSymbol {
    pub fn new(name: String) -> Self {
        Self {
            id: Id::new(),
            name,
        }
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl SemanticEq for UntypedTypeParameterSymbol {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

#[cfg(test)]
mod tests {
    use crate::data_type::DataType;
    use crate::expression::{Expression, FunctionCall, Literal};
    use crate::symbol::{FunctionSymbol, SymbolWithTypeParameter, VariableSymbol};
    use crate::test_shared::sample_codearea;
    use crate::{ASTNode, SemanticEq, TypedAST, UntypedAST};
    use std::rc::Rc;

    #[test]
    fn create_function_call_untyped() {
        let name = "test".to_string();
        let arg = ASTNode::new(
            Expression::<UntypedAST>::Literal("10".to_string()),
            sample_codearea(),
        );
        let call = FunctionCall::<UntypedAST>::new((name, Vec::new()), vec![arg]);
        assert_eq!("test", call.function().0);
        assert_eq!(1, call.args().len());
    }

    #[test]
    fn create_function_call_typed_wrong_args() {
        let symbol = Rc::new(FunctionSymbol::new(
            "test".to_string(),
            None,
            vec![Rc::new(VariableSymbol::new(
                "test1".to_string(),
                DataType::Bool,
            ))],
            Vec::new(),
        ));
        let arg = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::S32(10)),
            sample_codearea(),
        );
        let call = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg]);
        assert_eq!(None, call);

        let call_empty = FunctionCall::<TypedAST>::new(symbol, Vec::new());
        assert_eq!(None, call_empty)
    }

    #[test]
    fn create_function_call_typed() {
        let symbol = Rc::new(FunctionSymbol::new(
            "test".to_string(),
            None,
            vec![Rc::new(VariableSymbol::new(
                "test1".to_string(),
                DataType::Bool,
            ))],
            Vec::new(),
        ));
        let arg = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::Bool(true)),
            sample_codearea(),
        );
        let call = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg]);
        assert_eq!(None, call.as_ref().unwrap().function().return_type());
        assert_eq!("test", call.as_ref().unwrap().function().name());

        let arg2 = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::Bool(true)),
            sample_codearea(),
        );
        let call2 = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg2]);
        assert!(call.semantic_eq(&call2));
    }
}
