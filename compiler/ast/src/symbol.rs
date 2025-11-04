use std::hash::{Hash, Hasher};
use crate::data_type::Typed;
use crate::expression::Expression;
use crate::id::Id;
use crate::{ASTNode, ASTType, SemanticEquality, TypedAST, UntypedAST};
use std::rc::Rc;
use crate::composite::{EnumVariant, StructField};
use crate::visibility::Visibility;

/**  Any type that has symbols available for use
*/
pub trait SymbolTable<'a, Type: ASTType>: Iterator<Item = Symbol<'a, Type>> {}

#[derive(Debug, PartialEq)]
pub enum Symbol<'a, Type: ASTType> {
    Function(&'a FunctionSymbol<Type>),
    Variable(&'a VariableSymbol<Type>),
}

// We want to implement traits without all parts implementing them as well.
// Deriving isn't possible in this case
impl<Type: ASTType> Clone for Symbol<'_, Type>
{
    fn clone(&self) -> Self {
        match self
        {
            Symbol::Function(func) => Symbol::Function(func),
            Symbol::Variable(var) => Symbol::Variable(var)
        }
    }
}

impl<Type: ASTType> Hash for Symbol<'_, Type>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self
        {
            Symbol::Function(func) => func.hash(state),
            Symbol::Variable(var) => var.hash(state)
        }
    }
}

impl<Type: ASTType> Eq for Symbol<'_, Type>
{
}

/** A function symbol
# Equality
Two different FunctionSymbols are never equal
*/
#[derive(Debug)]
pub struct FunctionSymbol<Type: ASTType> {
    id: Id,
    name: String,
    // None = no return type/void
    return_type: Option<Type::GeneralDataType>,
    params: Vec<Rc<VariableSymbol<Type>>>,
}

impl<Type: ASTType> Hash for FunctionSymbol<Type>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for FunctionSymbol<Type>
{
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for FunctionSymbol<Type>
{
}

/** A variable symbol
# Equality
Two different VariableSymbols are never equal
*/
#[derive(Debug)]
pub struct VariableSymbol<Type: ASTType> {
    id: Id,
    name: String,
    data_type: Type::GeneralDataType,
}

impl<Type: ASTType> Hash for VariableSymbol<Type>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<Type: ASTType> PartialEq<Self> for VariableSymbol<Type>
{
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<Type: ASTType> Eq for VariableSymbol<Type>
{
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

/** The symbol of an enum
*/
#[derive(Debug, PartialEq)]
pub struct EnumSymbol<Type: ASTType>
{
    id: Id,
    visibility: Visibility,
    variants: Vec<ASTNode<EnumVariant<Type>>>
}

impl<Type: ASTType> EnumSymbol<Type>
{
    pub fn new(id: Id, visibility: Visibility, variants: Vec<ASTNode<EnumVariant<Type>>>) -> Self {
        Self { id, visibility, variants }
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    pub fn variants(&self) -> &Vec<ASTNode<EnumVariant<Type>>> {
        &self.variants
    }
}

/** A symbol for a struct
*/
#[derive(Debug, PartialEq)]
pub struct StructSymbol<Type: ASTType>
{
    id: Id,
    visibility: Visibility,
    fields: Vec<ASTNode<StructField<Type>>>,
}

impl<Type: ASTType> StructSymbol<Type>
{
    pub fn new(id: Id, visibility: Visibility, fields: Vec<ASTNode<StructField<Type>>>) -> Self {
        Self { id, visibility, fields }
    }

    pub fn id(&self) -> &Id {
        &self.id
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    pub fn fields(&self) -> &Vec<ASTNode<StructField<Type>>> {
        &self.fields
    }
}

/** A function call with params
*/
#[derive(Debug, PartialEq)]
pub struct FunctionCall<Type: ASTType> {
    function: Type::FunctionCallSymbol,
    args: Vec<ASTNode<Expression<Type>>>,
}

impl<Type: ASTType> FunctionCall<Type> {
    pub fn function(&self) -> &Type::FunctionCallSymbol {
        &self.function
    }

    pub fn args(&self) -> &Vec<ASTNode<Expression<Type>>> {
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
        args: Vec<ASTNode<Expression<TypedAST>>>,
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
    pub fn new(function: String, args: Vec<ASTNode<Expression<UntypedAST>>>) -> Self {
        Self { function, args }
    }
}

#[cfg(test)]
mod tests {
    use crate::data_type::DataType;
    use crate::expression::{Expression, Literal};
    use crate::symbol::{FunctionCall, FunctionSymbol, VariableSymbol};
    use crate::test_shared::sample_codearea;
    use crate::{ASTNode, SemanticEquality, TypedAST, UntypedAST};
    use std::rc::Rc;

    #[test]
    fn create_function_call_untyped() {
        let name = "test".to_string();
        let arg = ASTNode::new(
            Expression::<UntypedAST>::Literal("10".to_string()),
            sample_codearea(),
        );
        let call = FunctionCall::<UntypedAST>::new(name, vec![arg]);
        assert_eq!("test", call.function());
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
        assert!(call.semantic_equals(&call2));
    }
}
