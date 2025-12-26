//! This is the Abstract syntax tree, the interface between the parser and the codegen
//! It consists of five "levels", from highest to lowest:
//! 1. Directories
//! 2. Files
//! 3. Functions
//! 4. Statements
//! 5. Expressions
//!
//! Each level can contain instances of the level below it and its own level.
//!
//! In addition to these main types, there are also four traversial helpers:
//! DirectoryTraversalHelper, FileTraversalHelper, FunctionTraversalHelper, StatementTraversalHelper
//! They both contain references to an instance of Directory, File, Function or Statement and allow to list all
//! symbols available to it.
//!
//! For more information on how to use this, refer to the tests in this file.
//! Note that unlike in the tests, ASTs are not supposed to be hardcoded

use crate::data_type::DataType;
use crate::directory::Directory;
use crate::expression::Literal;
use crate::id::Id;
use crate::symbol::{FunctionSymbol, VariableSymbol};
use crate::top_level::{Import, ImportRoot};
use shared::code_reference::CodeArea;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::rc::Rc;

pub mod data_type;
pub mod directory;
pub mod expression;
pub mod file;
pub mod id;
pub mod statement;
pub mod symbol;
pub mod top_level;
pub mod traversal;
pub mod visibility;

///  Comparing semantics only.
///
/// More precisely, this checks if two language constructs have the same meaning
/// while disregarding identifiers such as ids and positional information.
/// The only exception are symbols, where it is required that the same are used.
///
/// Its primary intended use case it for tests, but it may find application elsewhere
pub trait SemanticEquality {
    ///  The equality method.
    /// For more information, refer to the trait documentation
    fn semantic_equals(&self, other: &Self) -> bool;
}

// SemanticEquality for common containers of types implementing SemanticEquality

impl<T: SemanticEquality> SemanticEquality for [T] {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(self_statement, other_statement)| {
                    self_statement.semantic_equals(other_statement)
                })
    }
}

impl<T: SemanticEquality> SemanticEquality for Option<T> {
    fn semantic_equals(&self, other: &Self) -> bool {
        // Check if both are some and compare then
        // Or both are none
        self.as_ref()
            .zip(other.as_ref())
            .map(|(a, b)| a.semantic_equals(b))
            .unwrap_or(self.is_none() && other.is_none())
    }
}

impl SemanticEquality for str {
    fn semantic_equals(&self, other: &Self) -> bool {
        self == other
    }
}

impl SemanticEquality for String {
    fn semantic_equals(&self, other: &Self) -> bool {
        self == other
    }
}

impl<T: SemanticEquality> SemanticEquality for &T {
    fn semantic_equals(&self, other: &Self) -> bool {
        (*self).semantic_equals(*other)
    }
}

impl<T: SemanticEquality> SemanticEquality for Rc<T> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.deref().semantic_equals(other.deref())
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

impl<Type: ASTType> SemanticEquality for AST<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.inner.semantic_equals(&other.inner)
    }
}

/// This represents an AST Type and its location. Which type of AST node this is depends on its first
/// generic. The second generic decides what is used to store positional information.
/// # Equality
/// Two different ASTNodes are never equal.
/// Use semantic_equals from [`SemanticEquality`] to check semantics only

#[derive(Debug)]
pub struct ASTNode<T: Debug, Position = CodeArea> {
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

impl<T: SemanticEquality + Debug, Position> SemanticEquality for ASTNode<T, Position> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.inner.semantic_equals(&other.inner)
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
    type LiteralType: PartialEq + Debug + SemanticEquality;
    type GeneralDataType: Eq + PartialEq + Debug + Clone + SemanticEquality;
    type FunctionCallSymbol: Debug + PartialEq + SemanticEquality;
    type VariableUse: Debug + PartialEq + Clone + SemanticEquality;
}

///  This is an ast type
/// ASTs with this type include concrete data types
#[derive(Clone, PartialEq, Debug)]
pub struct TypedAST {}

impl ASTType for TypedAST {
    type LiteralType = Literal;
    type GeneralDataType = DataType;
    type FunctionCallSymbol = Rc<FunctionSymbol<TypedAST>>;
    type VariableUse = Rc<VariableSymbol<TypedAST>>;
}

///  This is an ast type
/// ASTs with this type carry the data type used in a string and perform no validation on it
#[derive(Clone, PartialEq, Debug)]
pub struct UntypedAST {}

impl ASTType for UntypedAST {
    type LiteralType = String;
    type GeneralDataType = String;
    type FunctionCallSymbol = String;
    type VariableUse = String;
}

#[cfg(test)]
mod tests {
    use crate::data_type::DataType;
    use crate::directory::Directory;
    use crate::expression::{BinaryOp, BinaryOpType, Expression, FunctionCall, Literal};
    use crate::file::File;
    use crate::statement::{
        CodeBlock, ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
    };
    use crate::symbol::{FunctionSymbol, ModuleUsageNameSymbol, Symbol, VariableSymbol};
    use crate::test_shared::{basic_test_variable, functions_into_ast, sample_codearea};
    use crate::top_level::{Function, Import, ImportRoot};
    use crate::traversal::directory_traversal::DirectoryTraversalHelper;
    use crate::traversal::statement_traversal::StatementTraversalHelper;
    use crate::visibility::Visibility;
    use crate::{AST, ASTNode, SemanticEquality, TypedAST, UntypedAST};
    use shared::code_file::CodeFile;
    use shared::code_reference::{CodeArea, CodeLocation};
    use std::path::PathBuf;
    use std::rc::Rc;

    #[test]
    fn ast() {
        let symbol = Rc::new(VariableSymbol::new("test".to_string(), DataType::F32));
        let statement = ASTNode::new(
            Statement::VariableDeclaration(basic_test_variable(symbol.clone()).unwrap()),
            sample_codearea(),
        );

        assert_eq!(Some(symbol.as_ref()), statement.get_direct_symbol());

        let function = Function::new(
            Rc::new(FunctionSymbol::new("test".to_string(), None, Vec::new())),
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![statement])),
                sample_codearea(),
            ),
            Visibility::Public,
        );

        let ast = functions_into_ast(vec![ASTNode::new(function, sample_codearea())]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.file_by_name("main.waso").unwrap();
        let function_ref = file_traversal_helper.function_by_name("test").unwrap();

        let root = StatementTraversalHelper::new_root(&function_ref);
        let statement_ref = root.get_child(0).unwrap();
        assert_eq!(
            vec![Symbol::Function(function_ref.inner().declaration())],
            statement_ref
                .symbols_available_at()
                .map(|symbol| symbol.1)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn ast_2() {
        let symbol = Rc::new(VariableSymbol::new("test".to_string(), DataType::F32));

        let symbol2 = Rc::new(VariableSymbol::new("test2".to_string(), DataType::Bool));
        let statement = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![
                ASTNode::new(
                    Statement::VariableDeclaration(
                        VariableAssignment::<TypedAST>::new(
                            symbol.clone(),
                            ASTNode::new(
                                Expression::Literal(Literal::F32(10.0)),
                                sample_codearea(),
                            ),
                        )
                        .unwrap(),
                    ),
                    sample_codearea(),
                ),
                ASTNode::new(
                    Statement::ControlStructure(Box::new(ControlStructure::Loop(Loop::new(
                        ASTNode::new(
                            Statement::VariableDeclaration(
                                VariableAssignment::<TypedAST>::new(
                                    symbol2.clone(),
                                    ASTNode::new(
                                        Expression::Literal(Literal::Bool(true)),
                                        sample_codearea(),
                                    ),
                                )
                                .unwrap(),
                            ),
                            sample_codearea(),
                        ),
                        LoopType::Infinite,
                    )))),
                    sample_codearea(),
                ),
            ])),
            sample_codearea(),
        );

        let function = Function::new(
            Rc::new(FunctionSymbol::new("test".to_string(), None, Vec::new())),
            statement,
            Visibility::Public,
        );

        let ast = functions_into_ast(vec![ASTNode::new(function, sample_codearea())]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.file_by_name("main.waso").unwrap();
        let function_ref = file_traversal_helper.function_by_name("test").unwrap();

        let root = StatementTraversalHelper::new_root(&function_ref);
        let loop_statement = root.get_child(1).unwrap();

        assert_eq!(
            vec![Symbol::Variable(&symbol2)],
            loop_statement.symbols_defined_directly_in().unwrap()
        );
        let statement_ref = loop_statement.get_child(0).unwrap();

        let actual = statement_ref
            .symbols_available_at()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            Symbol::Variable(&symbol),
            Symbol::Function(function_ref.inner().declaration()),
        ];
        assert_eq!(actual.len(), expected.len());
        assert!(expected.iter().all(|val| actual.contains(val)));

        let actual = statement_ref
            .symbols_available_after()
            .unwrap()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            Symbol::Variable(&symbol),
            Symbol::Function(function_ref.inner().declaration()),
            Symbol::Variable(&symbol2),
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
        let function_ref = file_traversal_helper.function_by_name("fibonacci").unwrap();

        let root = function_ref.ref_to_implementation();
        let return_statement = root.get_child(3).unwrap();

        let actual = return_statement
            .symbols_available_at()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            Symbol::Variable(&nth),
            Symbol::Variable(&current),
            Symbol::Variable(&previous),
            Symbol::Function(&fibonacci),
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
        ));
        (nth, current, previous, temp, fibonacci)
    }

    #[test]
    fn fibonacci_typed_semantic_equals() {
        let (nth, current, previous, temp, fibonacci) = create_fibonacci_typed_symbols();
        let ast1 = create_fibonacci_typed(&nth, &current, &previous, &temp, &fibonacci);
        let ast2 = create_fibonacci_typed(&nth, &current, &previous, &temp, &fibonacci);
        assert!(ast1.semantic_equals(&ast2));
        assert!(ast2.semantic_equals(&ast1));
        // Sanity check: An AST should be semantically equal to itself
        assert!(ast1.semantic_equals(&ast1));
        assert!(ast2.semantic_equals(&ast2));

        let empty = functions_into_ast(Vec::new());
        assert!(!ast1.semantic_equals(&empty));
        assert!(!empty.semantic_equals(&ast1));
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
                                VariableAssignment::<TypedAST>::new(
                                    current.clone(),
                                    ASTNode::new(
                                        Expression::Literal(Literal::S32(1)),
                                        sample_codearea(),
                                    ),
                                )
                                .unwrap(),
                            ),
                            sample_codearea(),
                        ),
                        ASTNode::new(
                            Statement::VariableDeclaration(
                                VariableAssignment::<TypedAST>::new(
                                    previous.clone(),
                                    ASTNode::new(
                                        Expression::Literal(Literal::S32(0)),
                                        sample_codearea(),
                                    ),
                                )
                                .unwrap(),
                            ),
                            sample_codearea(),
                        ),
                        ASTNode::new(
                            Statement::ControlStructure(Box::new(ControlStructure::Loop(
                                Loop::new(
                                    ASTNode::new(
                                        Statement::Codeblock(CodeBlock::new(vec![
                                            ASTNode::new(
                                                Statement::VariableDeclaration(
                                                    VariableAssignment::<TypedAST>::new(
                                                        temp.clone(),
                                                        ASTNode::new(
                                                            Expression::Variable(current.clone()),
                                                            sample_codearea(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_codearea(),
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
                                                                        sample_codearea(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            previous.clone(),
                                                                        ),
                                                                        sample_codearea(),
                                                                    ),
                                                                )
                                                                .unwrap(),
                                                            )),
                                                            sample_codearea(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_codearea(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<TypedAST>::new(
                                                        previous.clone(),
                                                        ASTNode::new(
                                                            Expression::Variable(temp.clone()),
                                                            sample_codearea(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_codearea(),
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
                                                                        sample_codearea(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Literal(
                                                                            Literal::S32(1),
                                                                        ),
                                                                        sample_codearea(),
                                                                    ),
                                                                )
                                                                .unwrap(),
                                                            )),
                                                            sample_codearea(),
                                                        ),
                                                    )
                                                    .unwrap(),
                                                ),
                                                sample_codearea(),
                                            ),
                                        ])),
                                        sample_codearea(),
                                    ),
                                    LoopType::While(ASTNode::new(
                                        Expression::BinaryOp(Box::new(
                                            BinaryOp::<TypedAST>::new(
                                                BinaryOpType::Greater,
                                                ASTNode::new(
                                                    Expression::Variable(nth.clone()),
                                                    sample_codearea(),
                                                ),
                                                ASTNode::new(
                                                    Expression::Literal(Literal::S32(
                                                        1, //The fibonacci number of 1 is 1
                                                    )),
                                                    sample_codearea(),
                                                ),
                                            )
                                            .unwrap(),
                                        )),
                                        sample_codearea(),
                                    )),
                                ),
                            ))),
                            sample_codearea(),
                        ),
                        ASTNode::new(
                            Statement::Return(Return::new(Some(ASTNode::new(
                                Expression::Variable(current.clone()),
                                sample_codearea(),
                            )))),
                            sample_codearea(),
                        ),
                    ])),
                    sample_codearea(),
                ),
                Visibility::Public,
            ),
            sample_codearea(),
        )])
    }

    #[test]
    fn fibonacci_untyped() {
        // The how manyth fibonacci number we want
        let nth = Rc::new(VariableSymbol::<UntypedAST>::new(
            "nth".to_string(),
            "s32".to_string(),
        ));
        let current = Rc::new(VariableSymbol::new(
            "current".to_string(),
            "s32".to_string(),
        ));
        let previous = Rc::new(VariableSymbol::new(
            "previous".to_string(),
            "s32".to_string(),
        ));
        let temp = Rc::new(VariableSymbol::new("temp".to_string(), "s32".to_string()));

        let fibonacci = Rc::new(FunctionSymbol::new(
            "fibonacci".to_string(),
            Some("s32".to_string()),
            vec![nth.clone()],
        ));
        let ast = functions_into_ast(vec![ASTNode::new(
            Function::new(
                fibonacci.clone(),
                ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(vec![
                        ASTNode::new(
                            Statement::VariableDeclaration(VariableAssignment::<UntypedAST>::new(
                                current.clone(),
                                ASTNode::new(
                                    Expression::Literal("1".to_string()),
                                    sample_codearea(),
                                ),
                            )),
                            sample_codearea(),
                        ),
                        ASTNode::new(
                            Statement::VariableDeclaration(VariableAssignment::<UntypedAST>::new(
                                previous.clone(),
                                ASTNode::new(
                                    Expression::Literal("0".to_string()),
                                    sample_codearea(),
                                ),
                            )),
                            sample_codearea(),
                        ),
                        ASTNode::new(
                            Statement::ControlStructure(Box::new(ControlStructure::Loop(
                                Loop::new(
                                    ASTNode::new(
                                        Statement::Codeblock(CodeBlock::new(vec![
                                            ASTNode::new(
                                                Statement::VariableDeclaration(
                                                    VariableAssignment::<UntypedAST>::new(
                                                        temp.clone(),
                                                        ASTNode::new(
                                                            Expression::Variable(
                                                                "current".to_string(),
                                                            ),
                                                            sample_codearea(),
                                                        ),
                                                    ),
                                                ),
                                                sample_codearea(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<UntypedAST>::new(
                                                        current.clone(),
                                                        ASTNode::new(
                                                            Expression::BinaryOp(Box::new(
                                                                BinaryOp::<UntypedAST>::new(
                                                                    BinaryOpType::Addition,
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            "current".to_string(),
                                                                        ),
                                                                        sample_codearea(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            "previous".to_string(),
                                                                        ),
                                                                        sample_codearea(),
                                                                    ),
                                                                ),
                                                            )),
                                                            sample_codearea(),
                                                        ),
                                                    ),
                                                ),
                                                sample_codearea(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<UntypedAST>::new(
                                                        previous.clone(),
                                                        ASTNode::new(
                                                            Expression::Variable(
                                                                "temp".to_string(),
                                                            ),
                                                            sample_codearea(),
                                                        ),
                                                    ),
                                                ),
                                                sample_codearea(),
                                            ),
                                            ASTNode::new(
                                                Statement::VariableAssignment(
                                                    VariableAssignment::<UntypedAST>::new(
                                                        nth.clone(),
                                                        ASTNode::new(
                                                            Expression::BinaryOp(Box::new(
                                                                BinaryOp::<UntypedAST>::new(
                                                                    BinaryOpType::Subtraction,
                                                                    ASTNode::new(
                                                                        Expression::Variable(
                                                                            "nth".to_string(),
                                                                        ),
                                                                        sample_codearea(),
                                                                    ),
                                                                    ASTNode::new(
                                                                        Expression::Literal(
                                                                            "1".to_string(),
                                                                        ),
                                                                        sample_codearea(),
                                                                    ),
                                                                ),
                                                            )),
                                                            sample_codearea(),
                                                        ),
                                                    ),
                                                ),
                                                sample_codearea(),
                                            ),
                                        ])),
                                        sample_codearea(),
                                    ),
                                    LoopType::While(ASTNode::new(
                                        Expression::BinaryOp(Box::new(
                                            BinaryOp::<UntypedAST>::new(
                                                BinaryOpType::Greater,
                                                ASTNode::new(
                                                    Expression::Variable("nth".to_string()),
                                                    sample_codearea(),
                                                ),
                                                ASTNode::new(
                                                    Expression::Literal(
                                                        "1".to_string(), //The fibonacci number of 1 is 1
                                                    ),
                                                    sample_codearea(),
                                                ),
                                            ),
                                        )),
                                        sample_codearea(),
                                    )),
                                ),
                            ))),
                            sample_codearea(),
                        ),
                        ASTNode::new(
                            Statement::Return(Return::new(Some(ASTNode::new(
                                Expression::Variable("current".to_string()),
                                sample_codearea(),
                            )))),
                            sample_codearea(),
                        ),
                    ])),
                    sample_codearea(),
                ),
                Visibility::Public,
            ),
            sample_codearea(),
        )]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.file_by_name("main.waso").unwrap();
        let function_ref = file_traversal_helper.function_by_name("fibonacci").unwrap();

        let root = function_ref.ref_to_implementation();
        let return_statement = root.get_child(3).unwrap();

        let actual = return_statement
            .symbols_available_at()
            .map(|symbol| symbol.1)
            .collect::<Vec<_>>();
        let expected = vec![
            Symbol::Variable(&nth),
            Symbol::Variable(&current),
            Symbol::Variable(&previous),
            Symbol::Function(&fibonacci),
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
        ));
        let lhs_var = Rc::new(VariableSymbol::new("lhs".to_string(), DataType::S32));
        let rhs_var = Rc::new(VariableSymbol::new("rhs".to_string(), DataType::S32));
        let add_fn_symbol = Rc::new(FunctionSymbol::new(
            "add".to_string(),
            Some(DataType::S32),
            vec![lhs_var.clone(), rhs_var.clone()],
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
                                ASTNode::new(
                                    Expression::Variable(lhs_var.clone()),
                                    CodeArea::new(
                                        CodeLocation::new(2, 1),
                                        CodeLocation::new(2, 4),
                                        CodeFile::new(PathBuf::from("add.waso")),
                                    )
                                    .unwrap(),
                                ),
                                ASTNode::new(
                                    Expression::Variable(rhs_var.clone()),
                                    CodeArea::new(
                                        CodeLocation::new(2, 5),
                                        CodeLocation::new(2, 8),
                                        CodeFile::new(PathBuf::from("add.waso")),
                                    )
                                    .unwrap(),
                                ),
                            )
                            .unwrap(),
                        )),
                        CodeArea::new(
                            CodeLocation::new(2, 1),
                            CodeLocation::new(2, 8),
                            CodeFile::new(PathBuf::from("add.waso")),
                        )
                        .unwrap(),
                    )))),
                    CodeArea::new(
                        CodeLocation::new(1, 0),
                        CodeLocation::new(3, 1),
                        CodeFile::new(PathBuf::from("add.waso")),
                    )
                    .unwrap(),
                ),
                Visibility::Public,
            ),
            CodeArea::new(
                CodeLocation::new(0, 0),
                CodeLocation::new(3, 1),
                CodeFile::new(PathBuf::from("add.waso")),
            )
            .unwrap(),
        );
        let add_file = File::new("add".to_string(), Vec::new(), vec![add_function]);

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
                                        CodeArea::new(
                                            CodeLocation::new(3, 5),
                                            CodeLocation::new(3, 6),
                                            CodeFile::new(PathBuf::from("main.waso")),
                                        )
                                        .unwrap(),
                                    ),
                                    ASTNode::new(
                                        Expression::Literal(Literal::S32(1)),
                                        CodeArea::new(
                                            CodeLocation::new(3, 8),
                                            CodeLocation::new(3, 9),
                                            CodeFile::new(PathBuf::from("main.waso")),
                                        )
                                        .unwrap(),
                                    ),
                                ],
                            )
                            .unwrap(),
                        ),
                        CodeArea::new(
                            CodeLocation::new(3, 1),
                            CodeLocation::new(3, 10),
                            CodeFile::new(PathBuf::from("main.waso")),
                        )
                        .unwrap(),
                    )),
                    CodeArea::new(
                        CodeLocation::new(2, 0),
                        CodeLocation::new(4, 1),
                        CodeFile::new(PathBuf::from("main.waso")),
                    )
                    .unwrap(),
                ),
                Visibility::Public,
            ),
            CodeArea::new(
                CodeLocation::new(1, 0),
                CodeLocation::new(4, 1),
                CodeFile::new(PathBuf::from("main.waso")),
            )
            .unwrap(),
        );
        let main_file = File::new(
            "main".to_string(),
            vec![ASTNode::new(
                Import::new(ImportRoot::Root, vec![], testproject_symbol.clone()),
                CodeArea::new(
                    CodeLocation::new(0, 0),
                    CodeLocation::new(0, 15),
                    CodeFile::new(PathBuf::from("main.waso")),
                )
                .unwrap(),
            )],
            vec![main_function],
        );

        let ast = AST::new(ASTNode::new(
            Directory::new(
                "src".to_string(),
                Vec::new(),
                vec![
                    ASTNode::new(main_file, PathBuf::from("main")),
                    ASTNode::new(add_file, PathBuf::from("add")),
                ],
            ),
            PathBuf::new(),
        ))
        .unwrap();

        let dth = DirectoryTraversalHelper::new_from_ast(&ast);
        let fth = dth.file_by_name("main").unwrap();
        assert_eq!(
            vec![
                Symbol::Function(&main_fn_symbol),
                Symbol::Function(&add_fn_symbol),
                Symbol::ModuleUsageName(&testproject_symbol)
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
                    CodeArea::new(
                        CodeLocation::new(0, 0),
                        CodeLocation::new(0, 10),
                        CodeFile::new(PathBuf::from("main.waso")),
                    )
                    .unwrap(),
                )],
                Vec::new(),
            ),
            PathBuf::from("main.waso"),
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

#[cfg(test)]
// Stuff that is needed in tests in the entire crate
pub(crate) mod test_shared {
    use crate::directory::Directory;
    use crate::expression::{Expression, Literal};
    use crate::file::File;
    use crate::statement::VariableAssignment;
    use crate::symbol::VariableSymbol;
    use crate::top_level::Function;
    use crate::{AST, ASTNode, ASTType, TypedAST};
    use shared::code_file::CodeFile;
    use shared::code_reference::{CodeArea, CodeLocation};
    use std::path::PathBuf;
    use std::rc::Rc;

    pub(crate) fn sample_codearea() -> CodeArea {
        CodeArea::new(
            CodeLocation::new(0, 0),
            CodeLocation::new(0, 10),
            CodeFile::new(PathBuf::from("test/test")),
        )
        .unwrap()
    }

    pub(crate) fn basic_test_variable(
        symbol: Rc<VariableSymbol<TypedAST>>,
    ) -> Option<VariableAssignment<TypedAST>> {
        VariableAssignment::<TypedAST>::new(
            symbol,
            ASTNode::new(Expression::Literal(Literal::F32(14.0)), sample_codearea()),
        )
    }

    pub(crate) fn functions_into_ast<Type: ASTType>(
        functions: Vec<ASTNode<Function<Type>>>,
    ) -> AST<Type> {
        let mut src_path = PathBuf::new();
        src_path.push("src");
        let mut main_path = src_path.clone();
        main_path.push("main.waso");
        AST::new(ASTNode::new(
            Directory::new(
                "src".to_string(),
                Vec::new(),
                vec![ASTNode::new(
                    File::new("main.waso".to_string(), Vec::new(), functions),
                    main_path,
                )],
            ),
            PathBuf::new(),
        ))
        .unwrap()
    }
}
