//! This is the Abstract syntax tree, the interface between the parser and the codegen
//! It consists of five "levels", from highest to lowest:
//! 1. Directories
//! 2. Files
//! 3. Composites (Optional)
//! 4. Functions
//! 5. Statements
//! 6. Expressions
//!
//! Each level can contain instances of the level below it and its own level.
//!
//! In addition to these main types, there are also six traversial helpers:
//! DirectoryTraversalHelper, FileTraversalHelper, FunctionTraversalHelper, StatementTraversalHelper, StructTraversalHelper and EnumTraversalHelper
//! They all contain references to an instance of Directory, File, Function, Statement or Struct and allow to list all
//! symbols available to it.
//!
//! For more information on how to use this, refer to the tests in this file.
//! Note that unlike in the tests, ASTs are not supposed to be hardcoded

use crate::data_type::{DataType, UntypedDataType};
use crate::directory::Directory;
use crate::expression::Literal;
use crate::id::Id;
use crate::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol,
    SymbolWithTypeParameter, UntypedTypeParameterSymbol, VariableSymbol,
};
use crate::top_level::{Import, ImportRoot};
use crate::type_parameter::{TypedTypeParameter, UntypedTypeParameter};
use source::types::Span;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::rc::Rc;

pub mod composite;
pub mod data_type;
pub mod directory;
pub mod expression;
pub mod file;
pub mod id;
pub mod statement;
pub mod symbol;
pub mod top_level;
pub mod traversal;
pub mod type_parameter;
pub mod visibility;

///  Comparing semantics only.
///
/// More precisely, this checks if two language constructs have the same meaning
/// while disregarding identifiers such as ids and positional information.
/// The only exception are symbols, where it is required that the same are used.
///
/// Its primary intended use case it for tests, but it may find application elsewhere
pub trait SemanticEq {
    ///  The equality method.
    /// For more information, refer to the trait documentation
    fn semantic_eq(&self, other: &Self) -> bool;
}

// SemanticEquality for common containers of types implementing SemanticEquality

impl<T: SemanticEq> SemanticEq for [T] {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(self_statement, other_statement)| {
                    self_statement.semantic_eq(other_statement)
                })
    }
}

impl<T: SemanticEq> SemanticEq for Option<T> {
    fn semantic_eq(&self, other: &Self) -> bool {
        // Check if both are some and compare then
        // Or both are none
        self.as_ref()
            .zip(other.as_ref())
            .map(|(a, b)| a.semantic_eq(b))
            .unwrap_or(self.is_none() && other.is_none())
    }
}

// Required for ASTType
impl SemanticEq for str {
    fn semantic_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SemanticEq for String {
    fn semantic_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl<T: SemanticEq> SemanticEq for &T {
    fn semantic_eq(&self, other: &Self) -> bool {
        (*self).semantic_eq(*other)
    }
}

impl<T: SemanticEq> SemanticEq for Rc<T> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.deref().semantic_eq(other.deref())
    }
}

/// An AST
///
/// An AST consists of many projects, all of which are linked together by dependencies
///
/// # Type
///
/// The type decides if it will be untyped or typed
///
/// # Root
///
/// The root-level element is supposed to contain the individual projects
/// All imports in this must be valid
#[derive(Debug, PartialEq)]
pub struct AST<Type: ASTType> {
    // The root directory (e.g.: src)
    inner: ASTNode<Directory<Type>, PathBuf>,
}

/// See [`AST::new`]
#[derive(Debug)]
pub struct UnresolvedImports<Type: ASTType> {
    // This includes import errors
    pub(crate) ast: AST<Type>,
}

impl<Type: ASTType> UnresolvedImports<Type> {
    pub fn unresolved_imports(&self) -> Vec<&ASTNode<Import>> {
        self.ast.unresolved_imports()
    }
}

impl<Type: ASTType> AST<Type> {
    /// Creates a new instance of AST
    ///
    /// Returns Err if unresolved imports are contained. The problematic imports will be contained in the error
    // Lifetime issues prevent the imports from being returned directly
    pub fn new(inner: ASTNode<Directory<Type>, PathBuf>) -> Result<Self, UnresolvedImports<Type>> {
        let ast = Self { inner };
        if ast.unresolved_imports().is_empty() {
            return Ok(ast);
        }
        Err(UnresolvedImports { ast })
    }

    fn unresolved_imports(&self) -> Vec<&ASTNode<Import>> {
        self.list_imports()
            .iter()
            .filter(|(import, path)| !self.check_import(import, path))
            .map(|(import, _)| *import)
            .collect()
    }

    fn list_imports(&self) -> Vec<(&ASTNode<Import>, &Directory<Type>)> {
        let mut imports = Vec::new();
        self.deref()
            .traverse_imports(&mut |import, path| imports.push((import, path)));
        imports
    }

    /// Checks a specifiec import for validity. source_dir is where the import is from
    fn check_import(&self, to_check: &Import, source_dir: &Directory<Type>) -> bool {
        let check_origin = match to_check.root() {
            ImportRoot::CurrentModule => source_dir,
            ImportRoot::Root => &self.inner.inner,
        };
        check_origin.get_symbols_for_path(to_check.path()).is_some()
    }
}

impl<Type: ASTType> Deref for AST<Type> {
    type Target = ASTNode<Directory<Type>, PathBuf>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<Type: ASTType> SemanticEq for AST<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.inner.semantic_eq(&other.inner)
    }
}

/// This represents an AST Type and its location. Which type of AST node this is depends on its first
/// generic. The second generic decides what is used to store positional information.
///
/// # Equality
///
/// Two different ASTNodes are never equal.
///
/// Use semantic_equals from [`SemanticEq`] to check semantics only

#[derive(Debug)]
pub struct ASTNode<T: Debug, Position = Span> {
    id: Id,
    inner: T,
    position: Position,
}

impl<T: Debug, Position> ASTNode<T, Position> {
    pub fn new(inner: T, position: Position) -> Self {
        Self {
            inner,
            id: Id::new(),
            position,
        }
    }

    pub fn position(&self) -> &Position {
        &self.position
    }
}

impl<T: Debug, Position> Deref for ASTNode<T, Position> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Debug, Position> DerefMut for ASTNode<T, Position> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: SemanticEq + Debug, Position> SemanticEq for ASTNode<T, Position> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.inner.semantic_eq(&other.inner)
    }
}

impl<T: Debug> Hash for ASTNode<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

// More efficient than deriving
impl<T: Debug + PartialEq, Position> PartialEq for ASTNode<T, Position> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: Debug + PartialEq, Position> Eq for ASTNode<T, Position> {}

/// This compares two values
/// This is useful for returning with the ? operator if values are not equal
///
/// # Params
///
/// - left, right
///     - The values to compare
///
/// # return
/// - None if not equal
/// - Some if equal
fn eq_return_option<T: PartialEq>(left: T, right: T) -> Option<()> {
    if left == right {
        return Some(());
    }
    None
}

///  This decided what type the ast is.
pub trait ASTType: Sized + PartialEq + 'static + Debug {
    type LiteralType: PartialEq + Debug;
    type GeneralDataType: PartialEq + Debug + Clone + SemanticEq;
    type FunctionCallSymbol: Debug + PartialEq + SemanticEq;
    type VariableUse: Debug + PartialEq + Clone + SemanticEq;
    type StructUse: Debug + PartialEq + SemanticEq;
    type EnumUse: Debug + PartialEq + SemanticEq;
    type EnumVariantUse: Debug + PartialEq + SemanticEq;
    type StructFieldUse: Debug + PartialEq + SemanticEq;
    /// The type parameter on declaration, **not** usage
    type TypeParameterDeclaration: Debug + PartialEq + SemanticEq;
    /// The minimal combination of data that identifies a symbol
    ///
    /// This is supposed to be only used for querying and similar and is therefore
    /// in its borrowed form (e.g.: `&str`)
    ///
    /// In the untyped AST, this identifies the symbol itself and not a usage.
    ///     - In the types AST, usage works via symbol
    ///
    /// This always has a name, which can be extracted via the [`SymbolIdentifier`] trait
    ///
    /// Symbols without a type parameter (e.g.: variable) can only match if there are no
    /// type parameters on this.
    ///
    /// This can only be used to distinguish between symbols in a single symbol container
    /// (e.g.: File, Struct)
    type SymbolIdentifier<'a>: Debug + PartialEq + Copy + SymbolIdentifier;

    /// Gets all type parameter symbols of the provided symbol with type parameters
    ///
    /// In practice, this means that for untyped symbols all parameters will be returned and for typed
    /// an empty iterator.
    ///
    /// Keep in mind that for typed symbols, the type parameters are part of the identifier and
    /// not symbols that provide data types inside the composite
    fn type_parameter_symbols_of_symbol_with_type_parameter(
        of: &impl SymbolWithTypeParameter<Self>,
    ) -> impl Iterator<Item = &UntypedTypeParameterSymbol>;

    /// Checks wherever a given symbol with type parameter matches a given identifier
    fn symbol_with_type_parameter_matches_identifier(
        identifier: Self::SymbolIdentifier<'_>,
        to_check: &impl SymbolWithTypeParameter<Self>,
    ) -> bool;
}

/// This is an ast type
/// ASTs with this type include concrete data types
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedAST {}

impl ASTType for TypedAST {
    type LiteralType = Literal;
    type GeneralDataType = DataType;
    type FunctionCallSymbol = Rc<FunctionSymbol<TypedAST>>;
    type VariableUse = Rc<VariableSymbol<TypedAST>>;
    type StructUse = Rc<StructSymbol<TypedAST>>;
    type EnumUse = Rc<EnumSymbol<TypedAST>>;
    type EnumVariantUse = Rc<EnumVariantSymbol<TypedAST>>;
    type StructFieldUse = Rc<StructFieldSymbol<TypedAST>>;
    type TypeParameterDeclaration = TypedTypeParameter;
    type SymbolIdentifier<'a> = (&'a str, &'a [TypedTypeParameter]);
    fn type_parameter_symbols_of_symbol_with_type_parameter(
        _of: &impl SymbolWithTypeParameter<Self>,
    ) -> impl Iterator<Item = &UntypedTypeParameterSymbol> {
        // A typed struct has no type parameter symbols
        [].into_iter()
    }

    fn symbol_with_type_parameter_matches_identifier(
        identifier: Self::SymbolIdentifier<'_>,
        to_check: &impl SymbolWithTypeParameter<Self>,
    ) -> bool {
        to_check.type_parameters() == identifier.1 && to_check.name() == identifier.0
    }
}

/// This is an ast type
/// ASTs with this type carry the data type used in a string and perform no validation on it
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UntypedAST {}

impl ASTType for UntypedAST {
    type LiteralType = String;
    type GeneralDataType = UntypedDataType;
    type FunctionCallSymbol = (String, Vec<UntypedDataType>);
    type VariableUse = String;
    type StructUse = (String, Vec<UntypedDataType>);
    type EnumUse = (String, Vec<UntypedDataType>);
    type EnumVariantUse = String;
    type StructFieldUse = String;
    type TypeParameterDeclaration = UntypedTypeParameter;
    type SymbolIdentifier<'a> = &'a str;
    fn type_parameter_symbols_of_symbol_with_type_parameter(
        of: &impl SymbolWithTypeParameter<Self>,
    ) -> impl Iterator<Item = &UntypedTypeParameterSymbol> {
        of.type_parameters()
            .iter()
            .map(|type_param| type_param.inner())
    }

    fn symbol_with_type_parameter_matches_identifier(
        identifier: Self::SymbolIdentifier<'_>,
        to_check: &impl SymbolWithTypeParameter<Self>,
    ) -> bool {
        to_check.name() == identifier
    }
}

// Required to make the trait bounds of `UntypedAST` work
impl SemanticEq for (String, Vec<UntypedDataType>) {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

/// Used to get information out of a [`ASTType::SymbolIdentifier`]
pub trait SymbolIdentifier {
    fn name(&self) -> &str;
    fn count_type_parameters(&self) -> usize;
    fn has_type_parameters(&self) -> bool {
        self.count_type_parameters() == 0
    }
}

impl SymbolIdentifier for &str {
    fn name(&self) -> &str {
        self
    }

    fn count_type_parameters(&self) -> usize {
        0
    }
}

impl SymbolIdentifier for (&str, &[TypedTypeParameter]) {
    fn name(&self) -> &str {
        self.0
    }

    fn count_type_parameters(&self) -> usize {
        self.1.len()
    }
}

#[cfg(test)]
mod tests {
    use crate::composite::{Enum, EnumVariant, Struct, StructField};
    use crate::data_type::{DataType, UntypedDataType};
    use crate::directory::Directory;
    use crate::expression::{
        BinaryOp, BinaryOpType, Expression, FunctionCall, Literal, NewEnum, NewStruct,
        StructFieldAccess,
    };
    use crate::file::File;
    use crate::statement::{
        CodeBlock, ControlStructure, IfEnumVariant, Loop, LoopType, Return, Statement,
        VariableAssignment, VariableDeclaration,
    };
    use crate::symbol::{
        DirectlyAvailableSymbol, EnumSymbol, EnumVariantSymbol, FunctionSymbol,
        ModuleUsageNameSymbol, StructFieldSymbol, StructSymbol, SymbolWithTypeParameter,
        UntypedTypeParameterSymbol, VariableSymbol,
    };
    use crate::test_shared::{basic_test_variable, functions_into_ast, sample_span};
    use crate::top_level::{Function, Import, ImportRoot};
    use crate::traversal::directory_traversal::DirectoryTraversalHelper;
    use crate::traversal::statement_traversal::StatementTraversalHelper;
    use crate::traversal::{FunctionContainer, HasSymbols};
    use crate::type_parameter::{TypedTypeParameter, UntypedTypeParameter};
    use crate::visibility::Visibility;
    use crate::{AST, ASTNode, SemanticEq, TypedAST, UntypedAST};
    use source::types::FileID;
    use std::path::PathBuf;
    use std::rc::Rc;

    #[test]
    fn prove_identity_vs_semantic_eq() {
        let node_a = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::S32(5)),
            sample_span(),
        );

        let node_b = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::S32(5)),
            sample_span(),
        );

        assert_ne!(
            node_a, node_b,
            "PartialEq (==) must fail because IDs are different"
        );

        assert!(
            node_a.semantic_eq(&node_b),
            "SemanticEquality must succeed because content is identical"
        );
    }

    #[test]
    fn ast() {
        let symbol = Rc::new(VariableSymbol::new("test".to_string(), DataType::F64));
        let statement = ASTNode::new(
            Statement::VariableDeclaration(basic_test_variable(symbol.clone()).unwrap()),
            sample_span(),
        );

        assert_eq!(
            vec![symbol.as_ref()],
            statement.get_direct_variable_symbols()
        );

        assert_eq!(
            vec![symbol.as_ref()],
            statement.get_direct_variable_symbols()
        );

        let function = Function::new(
            Rc::new(FunctionSymbol::new(
                "test".to_string(),
                None,
                Vec::new(),
                Vec::new(),
            )),
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![statement])),
                sample_span(),
            ),
            Visibility::Public,
        );

        let ast = functions_into_ast(vec![ASTNode::new(function, sample_span())]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.file_by_name("main.waso").unwrap();
        let function_ref = file_traversal_helper
            .function_by_identifier(("test", &[]))
            .unwrap();

        let root = StatementTraversalHelper::new_root(&function_ref);
        let statement_ref = root.get_child(0).unwrap();
        assert_eq!(
            vec![DirectlyAvailableSymbol::Function(
                function_ref.inner().declaration()
            )],
            statement_ref
                .symbols()
                .map(|symbol| symbol.1)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn ast_2() {
        let symbol = Rc::new(VariableSymbol::new("test".to_string(), DataType::F64));

        let symbol2 = Rc::new(VariableSymbol::new("test2".to_string(), DataType::Bool));
        let statement = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![
                ASTNode::new(
                    Statement::VariableDeclaration(
                        VariableDeclaration::<TypedAST>::new(
                            symbol.clone(),
                            ASTNode::new(Expression::Literal(Literal::F64(10.0)), sample_span()),
                        )
                        .unwrap(),
                    ),
                    sample_span(),
                ),
                ASTNode::new(
                    Statement::ControlStructure(Box::new(ControlStructure::Loop(Loop::new(
                        ASTNode::new(
                            Statement::VariableDeclaration(
                                VariableDeclaration::<TypedAST>::new(
                                    symbol2.clone(),
                                    ASTNode::new(
                                        Expression::Literal(Literal::Bool(true)),
                                        sample_span(),
                                    ),
                                )
                                .unwrap(),
                            ),
                            sample_span(),
                        ),
                        LoopType::Infinite,
                    )))),
                    sample_span(),
                ),
            ])),
            sample_span(),
        );

        let function = Function::new(
            Rc::new(FunctionSymbol::new(
                "test".to_string(),
                None,
                Vec::new(),
                Vec::new(),
            )),
            statement,
            Visibility::Public,
        );

        let ast = functions_into_ast(vec![ASTNode::new(function, sample_span())]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.file_by_name("main.waso").unwrap();
        let function_ref = file_traversal_helper
            .function_by_identifier(("test", &[]))
            .unwrap();

        let root = StatementTraversalHelper::new_root(&function_ref);
        let loop_statement = root.get_child(1).unwrap();

        assert_eq!(
            vec![DirectlyAvailableSymbol::Variable(&symbol2)],
            loop_statement.symbols_defined_directly_in()
        );
        let statement_ref = loop_statement.get_child(0).unwrap();

        let actual = statement_ref
            .symbols_available_at()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            DirectlyAvailableSymbol::Variable(&symbol),
            DirectlyAvailableSymbol::Function(function_ref.inner().declaration()),
        ];
        assert_eq!(actual.len(), expected.len());
        assert!(expected.iter().all(|val| actual.contains(val)));

        let actual = statement_ref
            .symbols_available_after()
            .unwrap()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            DirectlyAvailableSymbol::Variable(&symbol),
            DirectlyAvailableSymbol::Function(function_ref.inner().declaration()),
            DirectlyAvailableSymbol::Variable(&symbol2),
        ];
        assert_eq!(actual.len(), expected.len());
        assert!(expected.iter().all(|val| actual.contains(val)));
    }

    #[test]
    fn fibonacci_typed() {
        // The how manyth fibonacci number we want

        let (nth, current, previous, temp, fibonacci) = create_fibonacci_typed_symbols();
        let ast = create_fibonacci_typed(&nth, &current, &previous, &temp, &fibonacci);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.file_by_name("main.waso").unwrap();
        let function_ref = file_traversal_helper
            .function_by_identifier(("fibonacci", &[]))
            .unwrap();

        let root = function_ref.ref_to_implementation();
        let return_statement = root.get_child(3).unwrap();

        let actual = return_statement
            .symbols_available_at()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            DirectlyAvailableSymbol::Variable(&nth),
            DirectlyAvailableSymbol::Variable(&current),
            DirectlyAvailableSymbol::Variable(&previous),
            DirectlyAvailableSymbol::Function(&fibonacci),
        ];
        assert_eq!(actual.len(), expected.len());
        assert!(expected.iter().all(|val| actual.contains(val)));
    }

    fn create_fibonacci_typed_symbols() -> (
        Rc<VariableSymbol<TypedAST>>,
        Rc<VariableSymbol<TypedAST>>,
        Rc<VariableSymbol<TypedAST>>,
        Rc<VariableSymbol<TypedAST>>,
        Rc<FunctionSymbol<TypedAST>>,
    ) {
        let nth = Rc::new(VariableSymbol::new("nth".to_string(), DataType::S32));
        let current = Rc::new(VariableSymbol::new("current".to_string(), DataType::S32));
        let previous = Rc::new(VariableSymbol::new("previous".to_string(), DataType::S32));
        let temp = Rc::new(VariableSymbol::new("temp".to_string(), DataType::S32));

        let fibonacci = Rc::new(FunctionSymbol::new(
            "fibonacci".to_string(),
            Some(DataType::S32),
            vec![nth.clone()],
            Vec::new(),
        ));
        (nth, current, previous, temp, fibonacci)
    }

    #[test]
    fn fibonacci_typed_semantic_equals() {
        let (nth, current, previous, temp, fibonacci) = create_fibonacci_typed_symbols();
        let ast1 = create_fibonacci_typed(&nth, &current, &previous, &temp, &fibonacci);
        let ast2 = create_fibonacci_typed(&nth, &current, &previous, &temp, &fibonacci);
        assert!(ast1.semantic_eq(&ast2));
        assert!(ast2.semantic_eq(&ast1));
        // Sanity check: An AST should be semantically equal to itself
        assert!(ast1.semantic_eq(&ast1));
        assert!(ast2.semantic_eq(&ast2));

        let empty = functions_into_ast(Vec::new());
        assert!(!ast1.semantic_eq(&empty));
        assert!(!empty.semantic_eq(&ast1));
    }

    fn create_fibonacci_typed(
        nth: &Rc<VariableSymbol<TypedAST>>,
        current: &Rc<VariableSymbol<TypedAST>>,
        previous: &Rc<VariableSymbol<TypedAST>>,
        temp: &Rc<VariableSymbol<TypedAST>>,
        fibonacci: &Rc<FunctionSymbol<TypedAST>>,
    ) -> AST<TypedAST> {
        functions_into_ast(vec![ASTNode::new(
            Function::new(
                fibonacci.clone(),
                ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(vec![
                        ASTNode::new(
                            Statement::VariableDeclaration(
                                VariableDeclaration::<TypedAST>::new(
                                    current.clone(),
                                    ASTNode::new(
                                        Expression::Literal(Literal::S32(1)),
                                        sample_span(),
                                    ),
                                )
                                .unwrap(),
                            ),
                            sample_span(),
                        ),
                        ASTNode::new(
                            Statement::VariableDeclaration(
                                VariableDeclaration::<TypedAST>::new(
                                    previous.clone(),
                                    ASTNode::new(
                                        Expression::Literal(Literal::S32(0)),
                                        sample_span(),
                                    ),
                                )
                                .unwrap(),
                            ),
                            sample_span(),
                        ),
                        ASTNode::new(
                            Statement::ControlStructure(Box::new(ControlStructure::Loop(
                                Loop::new(
                                    ASTNode::new(
                                        Statement::Codeblock(CodeBlock::new(vec![
                                            ASTNode::new(
                                                Statement::VariableDeclaration(
                                                    VariableDeclaration::<TypedAST>::new(
                                                        temp.clone(),
                                                        ASTNode::new(
                                                            Expression::Variable(current.clone()),
                                                            sample_span(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_span(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<TypedAST>::new(
                                                        current.clone(),
                                                        ASTNode::new(
                                                            Expression::BinaryOp(Box::new(
                                                                BinaryOp::<TypedAST>::new(
                                                                    BinaryOpType::Addition,
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            current.clone(),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            previous.clone(),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                )
                                                                .unwrap(),
                                                            )),
                                                            sample_span(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_span(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<TypedAST>::new(
                                                        previous.clone(),
                                                        ASTNode::new(
                                                            Expression::Variable(temp.clone()),
                                                            sample_span(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_span(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<TypedAST>::new(
                                                        nth.clone(),
                                                        ASTNode::new(
                                                            Expression::BinaryOp(Box::new(
                                                                BinaryOp::<TypedAST>::new(
                                                                    BinaryOpType::Subtraction,
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            nth.clone(),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Literal(
                                                                            Literal::S32(1),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                )
                                                                .unwrap(),
                                                            )),
                                                            sample_span(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_span(),
                                            ),
                                        ])),
                                        sample_span(),
                                    ),
                                    LoopType::While(ASTNode::new(
                                        Expression::BinaryOp(Box::new(
                                            BinaryOp::<TypedAST>::new(
                                                BinaryOpType::Greater,
                                                ASTNode::new(
                                                    Expression::Variable(nth.clone()),
                                                    sample_span(),
                                                ),
                                                ASTNode::new(
                                                    Expression::Literal(Literal::S32(
                                                        1, //The fibonacci number of 1 is 1
                                                    )),
                                                    sample_span(),
                                                ),
                                            )
                                            .unwrap(),
                                        )),
                                        sample_span(),
                                    )),
                                ),
                            ))),
                            sample_span(),
                        ),
                        ASTNode::new(
                            Statement::Return(Return::new(Some(ASTNode::new(
                                Expression::Variable(current.clone()),
                                sample_span(),
                            )))),
                            sample_span(),
                        ),
                    ])),
                    sample_span(),
                ),
                Visibility::Public,
            ),
            sample_span(),
        )])
    }

    #[test]
    fn fibonacci_untyped() {
        // The how manyth fibonacci number we want
        let nth = Rc::new(VariableSymbol::<UntypedAST>::new(
            "nth".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        ));
        let current = Rc::new(VariableSymbol::new(
            "current".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        ));
        let previous = Rc::new(VariableSymbol::new(
            "previous".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        ));
        let temp = Rc::new(VariableSymbol::new(
            "temp".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        ));

        let fibonacci = Rc::new(FunctionSymbol::new(
            "fibonacci".to_string(),
            Some(UntypedDataType::new("s32".to_string(), Vec::new())),
            vec![nth.clone()],
            Vec::new(),
        ));
        let ast = functions_into_ast(vec![ASTNode::new(
            Function::new(
                fibonacci.clone(),
                ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(vec![
                        ASTNode::new(
                            Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                                current.clone(),
                                ASTNode::new(Expression::Literal("1".to_string()), sample_span()),
                            )),
                            sample_span(),
                        ),
                        ASTNode::new(
                            Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                                previous.clone(),
                                ASTNode::new(Expression::Literal("0".to_string()), sample_span()),
                            )),
                            sample_span(),
                        ),
                        ASTNode::new(
                            Statement::ControlStructure(Box::new(ControlStructure::Loop(
                                Loop::new(
                                    ASTNode::new(
                                        Statement::Codeblock(CodeBlock::new(vec![
                                            ASTNode::new(
                                                Statement::VariableDeclaration(
                                                    VariableDeclaration::<UntypedAST>::new(
                                                        temp.clone(),
                                                        ASTNode::new(
                                                            Expression::Variable(
                                                                "current".to_string(),
                                                            ),
                                                            sample_span(),
                                                        ),
                                                    ),
                                                ),
                                                sample_span(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<UntypedAST>::new(
                                                        current.name().into(),
                                                        ASTNode::new(
                                                            Expression::BinaryOp(Box::new(
                                                                BinaryOp::<UntypedAST>::new(
                                                                    BinaryOpType::Addition,
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            "current".to_string(),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            "previous".to_string(),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                ),
                                                            )),
                                                            sample_span(),
                                                        ),
                                                    ),
                                                ),
                                                sample_span(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<UntypedAST>::new(
                                                        previous.name().into(),
                                                        ASTNode::new(
                                                            Expression::Variable(
                                                                "temp".to_string(),
                                                            ),
                                                            sample_span(),
                                                        ),
                                                    ),
                                                ),
                                                sample_span(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<UntypedAST>::new(
                                                        nth.name().into(),
                                                        ASTNode::new(
                                                            Expression::BinaryOp(Box::new(
                                                                BinaryOp::<UntypedAST>::new(
                                                                    BinaryOpType::Subtraction,
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            "nth".to_string(),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Literal(
                                                                            "1".to_string(),
                                                                        ),
                                                                        sample_span(),
                                                                    ),
                                                                ),
                                                            )),
                                                            sample_span(),
                                                        ),
                                                    ),
                                                ),
                                                sample_span(),
                                            ),
                                        ])),
                                        sample_span(),
                                    ),
                                    LoopType::While(ASTNode::new(
                                        Expression::BinaryOp(Box::new(
                                            BinaryOp::<UntypedAST>::new(
                                                BinaryOpType::Greater,
                                                ASTNode::new(
                                                    Expression::Variable("nth".to_string()),
                                                    sample_span(),
                                                ),
                                                ASTNode::new(
                                                    Expression::Literal(
                                                        "1".to_string(), //The fibonacci number of 1 is 1
                                                    ),
                                                    sample_span(),
                                                ),
                                            ),
                                        )),
                                        sample_span(),
                                    )),
                                ),
                            ))),
                            sample_span(),
                        ),
                        ASTNode::new(
                            Statement::Return(Return::new(Some(ASTNode::new(
                                Expression::Variable("current".to_string()),
                                sample_span(),
                            )))),
                            sample_span(),
                        ),
                    ])),
                    sample_span(),
                ),
                Visibility::Public,
            ),
            sample_span(),
        )]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.file_by_name("main.waso").unwrap();
        let function_ref = file_traversal_helper
            .function_by_identifier("fibonacci")
            .unwrap();

        let root = function_ref.ref_to_implementation();
        let return_statement = root.get_child(3).unwrap();

        let actual = return_statement
            .symbols_available_at()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            DirectlyAvailableSymbol::Variable(&nth),
            DirectlyAvailableSymbol::Variable(&current),
            DirectlyAvailableSymbol::Variable(&previous),
            DirectlyAvailableSymbol::Function(&fibonacci),
        ];
        assert_eq!(actual.len(), expected.len());
        assert!(expected.iter().all(|val| actual.contains(val)));
    }

    #[test]
    fn multifile_ast_imports_should_work() {
        let main_fn_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            "main".to_string(),
            None,
            Vec::new(),
            Vec::new(),
        ));
        let lhs_var = Rc::new(VariableSymbol::new("lhs".to_string(), DataType::S32));
        let rhs_var = Rc::new(VariableSymbol::new("rhs".to_string(), DataType::S32));
        let add_fn_symbol = Rc::new(FunctionSymbol::new(
            "add".to_string(),
            Some(DataType::S32),
            vec![lhs_var.clone(), rhs_var.clone()],
            Vec::new(),
        ));

        let testproject_symbol = Rc::new(ModuleUsageNameSymbol::new("testproject".to_string()));
        let add_function = ASTNode::new(
            Function::new(
                add_fn_symbol.clone(),
                ASTNode::new(
                    Statement::Return(Return::new(Some(ASTNode::new(
                        Expression::BinaryOp(Box::new(
                            BinaryOp::<TypedAST>::new(
                                BinaryOpType::Addition,
                                ASTNode::new(Expression::Variable(lhs_var.clone()), sample_span()),
                                ASTNode::new(Expression::Variable(rhs_var.clone()), sample_span()),
                            )
                            .unwrap(),
                        )),
                        sample_span(),
                    )))),
                    sample_span(),
                ),
                Visibility::Public,
            ),
            sample_span(),
        );

        let add_file = File::new(
            "add".to_string(),
            Vec::new(),
            vec![add_function],
            Vec::new(),
            Vec::new(),
        );

        let main_function = ASTNode::new(
            Function::new(
                main_fn_symbol.clone(),
                ASTNode::new(
                    Statement::Expression(ASTNode::new(
                        Expression::FunctionCall(
                            FunctionCall::<TypedAST>::new(
                                add_fn_symbol.clone(),
                                vec![
                                    ASTNode::new(
                                        Expression::Literal(Literal::S32(1)),
                                        sample_span(),
                                    ),
                                    ASTNode::new(
                                        Expression::Literal(Literal::S32(1)),
                                        sample_span(),
                                    ),
                                ],
                            )
                            .unwrap(),
                        ),
                        sample_span(),
                    )),
                    sample_span(),
                ),
                Visibility::Public,
            ),
            sample_span(),
        );
        let main_file = File::new(
            "main".to_string(),
            vec![ASTNode::new(
                Import::new(ImportRoot::Root, vec![], testproject_symbol.clone()),
                sample_span(),
            )],
            vec![main_function],
            Vec::new(),
            Vec::new(),
        );

        let ast = AST::new(ASTNode::new(
            Directory::new(
                "src".to_string(),
                Vec::new(),
                vec![
                    ASTNode::new(main_file, FileID::from(0)),
                    ASTNode::new(add_file, FileID::from(0)),
                ],
            ),
            PathBuf::new(),
        ))
        .unwrap();

        let dth = DirectoryTraversalHelper::new_from_ast(&ast);
        let fth = dth.file_by_name("main").unwrap();
        assert_eq!(
            vec![
                DirectlyAvailableSymbol::Function(&main_fn_symbol),
                DirectlyAvailableSymbol::Function(&add_fn_symbol),
                DirectlyAvailableSymbol::ModuleUsageName(&testproject_symbol),
                // Main is supposed to come twice.
                // First because it is in the same file (without ModuleUsageName)
                // And then from the import (with ModuleUsageName)
                DirectlyAvailableSymbol::Function(&main_fn_symbol),
            ],
            fth.symbols().map(|symbol| symbol.1).collect::<Vec<_>>()
        );
        assert_eq!(0, dth.len_subdirectories());
        assert_eq!(2, dth.len_files());
        assert_eq!(0, dth.subdirectories_iterator().count());
        assert_eq!(2, dth.files_iterator().count());
        assert_ne!(
            dth.index_file(0).unwrap().inner(),
            dth.index_file(1).unwrap().inner()
        );
        assert_eq!(2, dth.inner().files().len())
    }

    #[test]
    fn unresolved_import_should_fail() {
        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                vec![ASTNode::new(
                    Import::new(
                        ImportRoot::Root,
                        vec!["nonexistent".to_string()],
                        Rc::new(ModuleUsageNameSymbol::new("testproject".to_string())),
                    ),
                    sample_span(),
                )],
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        );
        let directory = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        let ast = AST::new(directory);
        let unresolved = ast.unwrap_err();
        let imports = unresolved.unresolved_imports();
        assert_eq!(1, imports.len())
    }

    #[test]
    pub fn composite_multifile() {
        let warning_msg_inner_symbol =
            Rc::new(StructFieldSymbol::new("inner".to_string(), DataType::Char));
        let warning_msg_symbol = Rc::new(StructSymbol::new("Warning".to_string(), Vec::new()));

        let warning_msg_new_inner_param =
            Rc::new(VariableSymbol::new("inner".to_string(), DataType::Char));
        let warning_msg_new_symbol = Rc::new(FunctionSymbol::new(
            "new".to_string(),
            Some(DataType::Struct(warning_msg_symbol.clone())),
            vec![warning_msg_new_inner_param.clone()],
            Vec::new(),
        ));

        let warning_msg_get_inner_self_param = Rc::new(VariableSymbol::new(
            "self".to_string(),
            DataType::Struct(warning_msg_symbol.clone()),
        ));
        let warning_msg_get_inner_symbol = Rc::new(FunctionSymbol::new(
            "get_inner".to_string(),
            Some(DataType::Char),
            vec![warning_msg_get_inner_self_param.clone()],
            Vec::new(),
        ));

        let error_msg_inner_symbol =
            Rc::new(StructFieldSymbol::new("inner".to_string(), DataType::Char));
        let error_msg_symbol = Rc::new(StructSymbol::new("Error".to_string(), Vec::new()));

        let error_msg_new_inner_param =
            Rc::new(VariableSymbol::new("inner".to_string(), DataType::Char));
        let error_msg_new_symbol = Rc::new(FunctionSymbol::new(
            "new".to_string(),
            Some(DataType::Struct(error_msg_symbol.clone())),
            vec![error_msg_new_inner_param.clone()],
            Vec::new(),
        ));

        let error_msg_get_inner_self_param = Rc::new(VariableSymbol::new(
            "self".to_string(),
            DataType::Struct(error_msg_symbol.clone()),
        ));
        let error_msg_get_inner_symbol = Rc::new(FunctionSymbol::new(
            "get_inner".to_string(),
            Some(DataType::Char),
            vec![error_msg_get_inner_self_param.clone()],
            Vec::new(),
        ));

        let msg_warning_msg_symbol = Rc::new(EnumVariantSymbol::new(
            "Warning".to_string(),
            vec![DataType::Struct(warning_msg_symbol.clone())],
        ));
        let msg_error_msg_symbol = Rc::new(EnumVariantSymbol::new(
            "Error".to_string(),
            vec![DataType::Struct(error_msg_symbol.clone())],
        ));
        let msg_symbol = Rc::new(EnumSymbol::new("Message".to_string(), Vec::new()));

        let main_fn_symbol = Rc::new(FunctionSymbol::new(
            "main".to_string(),
            None,
            vec![],
            Vec::new(),
        ));
        let main_fn_warning_symbol = Rc::new(VariableSymbol::new(
            "warn".to_string(),
            DataType::Struct(warning_msg_symbol.clone()),
        ));

        let ast = AST::new(
            ASTNode::new(
                Directory::<TypedAST>::new(
                    "src".to_string(),
                    vec![
                        ASTNode::new(
                            Directory::new(
                                "message".to_string(),
                                vec![],
                                vec![
                                    ASTNode::new(
                                        File::new(
                                            "message".to_string(),
                                            vec![ASTNode::new(
                                                Import::new(ImportRoot::Root, vec!["warning".to_string()],
                                                            Rc::new(ModuleUsageNameSymbol::new("warning".to_string()))),
                                                sample_span()
                                            )],
                                            vec![],
                                            vec![
                                                ASTNode::new(
                                                    Enum::new(
                                                        msg_symbol.clone(),
                                                        vec![
                                                            ASTNode::new(
                                                                EnumVariant::new(
                                                                    msg_error_msg_symbol.clone()
                                                                ),
                                                                sample_span()
                                                            ),
                                                            ASTNode::new(
                                                                EnumVariant::new(
                                                                    msg_warning_msg_symbol.clone()
                                                                ),
                                                                sample_span()
                                                            )
                                                        ],
                                                        Visibility::Public
                                                    ),
                                                    sample_span()
                                                )
                                            ],
                                            vec![
                                                ASTNode::new(
                                                    Struct::new(
                                                        error_msg_symbol.clone(),
                                                        vec![
                                                            ASTNode::new(
                                                                Function::new(
                                                                    error_msg_new_symbol.clone(),
                                                                    ASTNode::new(
                                                                        Statement::Return(Return::new(
                                                                            Some(ASTNode::new(
                                                                                Expression::NewStruct(
                                                                                    Box::new(NewStruct::new(
                                                                                        error_msg_symbol.clone(),
                                                                                        vec![
                                                                                            (ASTNode::new(error_msg_inner_symbol.clone(),
                                                                                                          sample_span()
                                                                                                          ),
                                                                                            ASTNode::new(
                                                                                                Expression::Variable(
                                                                                                    error_msg_new_inner_param.clone()
                                                                                                ),
                                                                                                sample_span()))
                                                                                        ],
                                                                                    ))
                                                                                ),
                                                                                sample_span()
                                                                            ))
                                                                        )),
                                                                        sample_span()
                                                                    ),
                                                                    Visibility::Public
                                                                ),
                                                                sample_span()
                                                            ),
                                                            ASTNode::new(
                                                                Function::new(
                                                                    error_msg_get_inner_symbol.clone(),
                                                                    ASTNode::new(
                                                                        Statement::Return(Return::new(
                                                                            Some(ASTNode::new(
                                                                                Expression::StructFieldAccess(
                                                                                    Box::new(StructFieldAccess::<TypedAST>::new(
                                                                                        ASTNode::new(
                                                                                            Expression::Variable(error_msg_get_inner_self_param.clone()),
                                                                                            sample_span()
                                                                                        ),
                                                                                        error_msg_inner_symbol.clone()
                                                                                    ).unwrap())
                                                                                ),
                                                                                sample_span()
                                                                            ))
                                                                        )),
                                                                        sample_span()
                                                                    ),
                                                                    Visibility::Public
                                                                ),
                                                                sample_span()
                                                            )
                                                        ],
                                                        vec![
                                                            ASTNode::new(
                                                                StructField::new(
                                                                    error_msg_inner_symbol.clone(),
                                                                    Visibility::Public
                                                                ),
                                                                sample_span()
                                                            )
                                                        ],
                                                        Visibility::Private
                                                    ),
                                                    sample_span()
                                                )
                                            ]
                                        ),
                                        FileID::from(0)
                                    )
                                ],
                            ),
                            PathBuf::from("message")
                        ),
                        ASTNode::new(
                            Directory::new(
                                "warning".to_string(),
                                vec![],
                                vec![
                                    ASTNode::new(
                                        File::new(
                                            "warning".to_string(),
                                            vec![],
                                            vec![],
                                            vec![],
                                            vec![
                                                ASTNode::new(
                                                    Struct::new(
                                                        warning_msg_symbol.clone(),
                                                        vec![
                                                            ASTNode::new(
                                                                Function::new(
                                                                    warning_msg_new_symbol.clone(),
                                                                    ASTNode::new(
                                                                        Statement::Return(Return::new(
                                                                            Some(ASTNode::new(
                                                                                Expression::NewStruct(
                                                                                    Box::new(NewStruct::new(
                                                                                        warning_msg_symbol.clone(),
                                                                                        vec![
                                                                                            (ASTNode::new(warning_msg_inner_symbol.clone(),
                                                                                                          sample_span()
                                                                                                          ),
                                                                                            ASTNode::new(
                                                                                                Expression::Variable(
                                                                                                    warning_msg_new_inner_param.clone()
                                                                                                ),
                                                                                                sample_span()))
                                                                                        ],
                                                                                    ))
                                                                                ),
                                                                                sample_span()
                                                                            ))
                                                                        )),
                                                                        sample_span()
                                                                    ),
                                                                    Visibility::Public
                                                                ),
                                                                sample_span()
                                                            ),
                                                            ASTNode::new(
                                                                Function::new(
                                                                    warning_msg_get_inner_symbol.clone(),
                                                                    ASTNode::new(
                                                                        Statement::Return(Return::new(
                                                                            Some(ASTNode::new(
                                                                                Expression::StructFieldAccess(
                                                                                    Box::new(StructFieldAccess::<TypedAST>::new(
                                                                                        ASTNode::new(
                                                                                            Expression::Variable(warning_msg_get_inner_self_param.clone()),
                                                                                            sample_span()
                                                                                        ),
                                                                                        warning_msg_inner_symbol.clone()
                                                                                    ).unwrap())
                                                                                ),
                                                                                sample_span()
                                                                            ))
                                                                        )),
                                                                        sample_span()
                                                                    ),
                                                                    Visibility::Public
                                                                ),
                                                                sample_span()
                                                            )
                                                        ],
                                                        vec![
                                                            ASTNode::new(
                                                                StructField::new(
                                                                    warning_msg_inner_symbol.clone(),
                                                                    Visibility::Public
                                                                ),
                                                                sample_span()
                                                            )
                                                        ],
                                                        Visibility::Public
                                                    ),
                                                    sample_span()
                                                )
                                            ]
                                        ),
                                        FileID::from(0)
                                    )
                                ],
                            ),
                            PathBuf::from("warning")
                        )
                    ],
                    vec![
                        ASTNode::new(File::new(
                            "main".to_string(),
                            vec![
                                ASTNode::new(
                                    Import::new(ImportRoot::Root, vec!["warning".to_string()],
                                    Rc::new(ModuleUsageNameSymbol::new("warning".to_string()))),
                                    sample_span()),
                                ASTNode::new(
                                    Import::new(ImportRoot::Root, vec!["message".to_string()],
                                                Rc::new(ModuleUsageNameSymbol::new("warning".to_string()))),
                                    sample_span())
                            ],
                            vec![ASTNode::new(
                                Function::new(
                                    main_fn_symbol.clone(),
                                    ASTNode::new(
                                        Statement::Codeblock(
                                            CodeBlock::new(
                                                vec![
                                                    ASTNode::new(
                                                        Statement::ControlStructure(Box::new(
                                                            ControlStructure::IfEnumVariant(
                                                                IfEnumVariant::<TypedAST>::new(
                                                                    msg_symbol.clone(),
                                                                    msg_warning_msg_symbol.clone(),
                                                                    ASTNode::new(
                                                                        Expression::NewEnum(
                                                                            Box::new(NewEnum::<TypedAST>::new(
                                                                                msg_symbol.clone(),
                                                                                msg_warning_msg_symbol.clone(),
                                                                                vec![
                                                                                    ASTNode::new(
                                                                                        Expression::FunctionCall(
                                                                                            FunctionCall::<TypedAST>::new(
                                                                                                warning_msg_new_symbol.clone(),
                                                                                                vec![
                                                                                                    ASTNode::new(
                                                                                                        Expression::Literal(
                                                                                                            Literal::Char('e' as u32)
                                                                                                        ),
                                                                                                        sample_span()
                                                                                                    )
                                                                                                ]
                                                                                            ).unwrap()
                                                                                        ),
                                                                                        sample_span()
                                                                                    )
                                                                                ]
                                                                            ).unwrap())
                                                                        ),
                                                                        sample_span()),
                                                                    vec![
                                                                        main_fn_warning_symbol.clone()
                                                                    ],
                                                                    ASTNode::new(
                                                                        Statement::Expression(
                                                                            ASTNode::new(
                                                                                Expression::FunctionCall(
                                                                                    FunctionCall::<TypedAST>::new(
                                                                                        warning_msg_get_inner_symbol.clone(),
                                                                                        vec![
                                                                                            ASTNode::new(
                                                                                                Expression::Variable(main_fn_warning_symbol.clone()),
                                                                                                sample_span()
                                                                                            )
                                                                                        ]
                                                                                    ).unwrap()
                                                                                ),
                                                                                sample_span()
                                                                            )
                                                                        ),
                                                                        sample_span()
                                                                    ),
                                                                ).unwrap()
                                                            )
                                                        )),
                                                        sample_span()
                                                    )
                                                ]
                                            )
                                        ),
                                        sample_span()
                                    ),
                                    Visibility::Public
                                ),
                                sample_span()
                            )],
                            vec![],
                            vec![]),
                                     FileID::from(0))]),
                PathBuf::new()
            )
        ).unwrap();

        assert!(ast.semantic_eq(&ast));

        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let main = root.file_by_name("main").unwrap();
        let main_func = main.function_by_identifier(("main", &[])).unwrap();
        let root_statement = main_func.ref_to_implementation();
        let match_statement = root_statement.get_child(0).unwrap();
        let inner_function_call = match_statement.get_child(0).unwrap();

        let symbols = inner_function_call
            .symbols()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        assert_eq!(symbols.len(), 6);
        assert!(symbols.contains(&DirectlyAvailableSymbol::Function(&main_fn_symbol)));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Variable(&main_fn_warning_symbol)));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Enum(&msg_symbol)));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Struct(&warning_msg_symbol)));

        let msg_dir = root.subdirectory_by_name("message").unwrap();
        let msg_file = msg_dir.file_by_name("message").unwrap();
        let error_msg_struct = msg_file
            .struct_by_identifier(("Error", &Vec::new()))
            .unwrap();
        let new_error_function = error_msg_struct
            .function_by_identifier(("new", &[]))
            .unwrap();
        let root_statement = new_error_function.ref_to_implementation();
        let symbols = root_statement
            .symbols()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();

        assert_eq!(symbols.len(), 7);
        assert!(symbols.contains(&DirectlyAvailableSymbol::Function(&error_msg_new_symbol)));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Function(
            &error_msg_get_inner_symbol
        )));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Variable(
            &error_msg_new_inner_param
        )));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Struct(&error_msg_symbol)));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Enum(&msg_symbol)));
        assert!(symbols.contains(&DirectlyAvailableSymbol::Struct(&warning_msg_symbol)));
    }

    #[test]
    fn create_function_call_untyped() {
        let name = "test".to_string();
        let arg = ASTNode::new(
            Expression::<UntypedAST>::Literal("10".to_string()),
            sample_span(),
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
            sample_span(),
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
            sample_span(),
        );
        let call = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg]);
        assert_eq!(None, call.as_ref().unwrap().function().return_type());
        assert_eq!("test", call.as_ref().unwrap().function().name());

        let arg2 = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::Bool(true)),
            sample_span(),
        );
        let call2 = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg2]);
        assert!(call.semantic_eq(&call2));
    }

    #[test]
    pub fn generic_untyped() {
        let generic_test = Rc::new(StructSymbol::new(
            "GenericTest".to_string(),
            vec![UntypedTypeParameter::new(Rc::new(
                UntypedTypeParameterSymbol::new("T".to_string()),
            ))],
        ));
        let ast = AST::<UntypedAST>::new(ASTNode::new(
            Directory::new(
                "src".to_string(),
                Vec::new(),
                vec![ASTNode::new(
                    File::new(
                        "main".to_string(),
                        Vec::new(),
                        Vec::new(),
                        Vec::new(),
                        vec![ASTNode::new(
                            Struct::new(
                                generic_test.clone(),
                                Vec::new(),
                                Vec::new(),
                                Visibility::Public,
                            ),
                            sample_span(),
                        )],
                    ),
                    FileID::from(0),
                )],
            ),
            PathBuf::from("src"),
        ))
        .unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let main = root.file_by_name("main").unwrap();
        let generic_test = main.struct_by_identifier("GenericTest").unwrap();
        assert_eq!(generic_test.symbols().count(), 2);
    }

    #[test]
    pub fn generic_typed() {
        let generic_test = Rc::new(StructSymbol::new(
            "GenericTest".to_string(),
            vec![TypedTypeParameter::new("T".to_string(), DataType::S16)],
        ));
        let ast = AST::<TypedAST>::new(ASTNode::new(
            Directory::new(
                "src".to_string(),
                Vec::new(),
                vec![ASTNode::new(
                    File::new(
                        "main".to_string(),
                        Vec::new(),
                        Vec::new(),
                        Vec::new(),
                        vec![ASTNode::new(
                            Struct::new(
                                generic_test.clone(),
                                Vec::new(),
                                Vec::new(),
                                Visibility::Public,
                            ),
                            sample_span(),
                        )],
                    ),
                    FileID::from(0),
                )],
            ),
            PathBuf::from("src"),
        ))
        .unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let main = root.file_by_name("main").unwrap();
        let generic_test = main
            .struct_by_identifier((
                "GenericTest",
                &[TypedTypeParameter::new("T".to_string(), DataType::S16)],
            ))
            .unwrap();
        assert_eq!(generic_test.symbols().count(), 1);
    }
}

#[cfg(test)]
// Stuff that is needed in tests in the entire crate
pub(crate) mod test_shared {
    use crate::directory::Directory;
    use crate::expression::{Expression, Literal};
    use crate::file::File;
    use crate::statement::VariableDeclaration;
    use crate::symbol::VariableSymbol;
    use crate::top_level::Function;
    use crate::{AST, ASTNode, ASTType, TypedAST};
    use source::types::{FileID, Span};
    use std::path::PathBuf;
    use std::rc::Rc;

    pub(crate) fn sample_span() -> Span {
        FileID::from(0).span(0, 10)
    }

    pub(crate) fn basic_test_variable(
        symbol: Rc<VariableSymbol<TypedAST>>,
    ) -> Option<VariableDeclaration<TypedAST>> {
        VariableDeclaration::<TypedAST>::new(
            symbol,
            ASTNode::new(Expression::Literal(Literal::F64(14.0)), sample_span()),
        )
    }

    pub(crate) fn functions_into_ast<Type: ASTType>(
        functions: Vec<ASTNode<Function<Type>>>,
    ) -> AST<Type> {
        let mut src_path = PathBuf::new();
        src_path.push("src");
        AST::new(ASTNode::new(
            Directory::new(
                "src".to_string(),
                Vec::new(),
                vec![ASTNode::new(
                    File::new(
                        "main.waso".to_string(),
                        Vec::new(),
                        functions,
                        Vec::new(),
                        Vec::new(),
                    ),
                    FileID::from(0),
                )],
            ),
            PathBuf::new(),
        ))
        .unwrap()
    }
}
