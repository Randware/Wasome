//! This is the Abstract syntax tree, the interface between the parser and the codegen
//! It consists of three "levels", from highest to lowest:
//! 1. Functions
//! 2. Statements
//! 3. Expressions
//!
//! Each level can contain instances of the level below it and its own level.
//!
//! In addition to these main types, there are also two traversial helpers:
//! FunctionRef and StatementRef
//! They both contain references to an instance of Function or Statement and allow to list all
//! symbols available to a function/statement.
//!
//! For more information on how to use this, refer to the tests in this file.
//! Note that unlike in the tests, ASTs are not supposed to be hardcoded

use crate::data_type::DataType;
use crate::directory::Directory;
use crate::expression::Literal;
use crate::id::Id;
use crate::symbol::{FunctionSymbol, VariableSymbol};
use shared::code_reference::CodeArea;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::rc::Rc;

pub mod block;
pub mod data_type;
pub mod directory;
pub mod expression;
pub mod file;
pub mod id;
pub mod statement;
pub mod symbol;
pub mod top_level;
pub mod traversal;

/** Comparing semantics only.

More precisely, this checks if two language constructs have the same meaning
while disregarding identifiers such as ids and positional information.
The only exception are symbols, where it is required that the same are used.
*/
// Its primary intended use case it for tests, but it may find application elsewhere
pub trait SemanticEquality {
    /** The equality method. <br>
    For more information, refer to the trait documentation
    */
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

#[derive(Debug)]
pub struct AST<Type: ASTType> {
    // The root directory (e.g.: src)
    inner: ASTNode<Directory<Type>, PathBuf>,
}

impl<Type: ASTType> AST<Type> {
    pub fn new(inner: ASTNode<Directory<Type>, PathBuf>) -> Self {
        Self { inner }
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

/** This represents an AST Type and its location. Which type of AST node this is depends on its first
generic. The second generic decides what is used to store positional information.
# Equality
Two different ASTNodes are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/

#[derive(Debug, PartialEq)]
pub struct ASTNode<T: Debug + PartialEq, Position = CodeArea> {
    id: Id,
    inner: T,
    position: Position,
}

impl<T: Debug + PartialEq, Position> ASTNode<T, Position> {
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

impl<T: Debug + PartialEq, Position> Deref for ASTNode<T, Position> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Debug + PartialEq, Position> DerefMut for ASTNode<T, Position> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: SemanticEquality + Debug + PartialEq, Position> SemanticEquality for ASTNode<T, Position> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.inner.semantic_equals(&other.inner)
    }
}

impl<T: Debug + PartialEq> Hash for ASTNode<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

/** This compares two values
This is useful for returning with the ? operator if values are not equal
@params
left, right: The values to compare
@return
None if not equal
Some if equal
*/
fn eq_return_option<T: PartialEq>(left: T, right: T) -> Option<()> {
    if left == right {
        return Some(());
    }
    None
}

/** This decided what type the ast is.
*/
pub trait ASTType: Sized + PartialEq + 'static + Debug {
    type LiteralType: PartialEq + Debug;

    type GeneralDataType: Eq + PartialEq + Debug + Clone;
    type FunctionCallSymbol: Debug + PartialEq;
    type VariableUse: Debug + PartialEq;
}

/** This is an ast type
ASTs with this type include concrete data types
*/
#[derive(Clone, PartialEq, Debug)]
pub struct TypedAST {}

impl ASTType for TypedAST {
    type LiteralType = Literal;
    type GeneralDataType = DataType;
    type FunctionCallSymbol = Rc<FunctionSymbol<TypedAST>>;
    type VariableUse = Rc<VariableSymbol<TypedAST>>;
}

/** This is an ast type
ASTs with this type carry the data type used in a string and perform no validation on it
*/
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
    use crate::block::{CodeBlock, FunctionBlock};
    use crate::data_type::DataType;
    use crate::directory::Directory;
    use crate::expression::{BinaryOp, BinaryOpType, Expression, Literal};
    use crate::file::File;
    use crate::statement::{
        ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
    };
    use crate::symbol::{FunctionCall, FunctionSymbol, Symbol, VariableSymbol};
    use crate::test_shared::{basic_test_variable, functions_into_ast, sample_codearea};
    use crate::top_level::{Function, Import, ImportRoot};
    use crate::traversal::directory_traversal::DirectoryTraversalHelper;
    use crate::traversal::function_traversal::FunctionTraversalHelper;
    use crate::traversal::statement_traversal::StatementTraversalHelper;
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

        assert_eq!(
            Some(Symbol::Variable(&symbol)),
            statement.get_direct_symbol()
        );

        let function = Function::new(
            Rc::new(FunctionSymbol::new("test".to_string(), None, Vec::new())),
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![statement])),
                sample_codearea(),
            ),
        );

        let ast = functions_into_ast(vec![ASTNode::new(function, sample_codearea())]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.specific_file("main.waso").unwrap();
        let function_ref = file_traversal_helper.specific_function("test").unwrap();

        let root = StatementTraversalHelper::new_root(&function_ref);
        let statement_ref = root.index(0);
        assert_eq!(
            vec![Symbol::Function(function_ref.inner().declaration())],
            statement_ref
                .symbols_available_at()
                .unwrap()
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
        );

        let ast = functions_into_ast(vec![ASTNode::new(function, sample_codearea())]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.specific_file("main.waso").unwrap();
        let function_ref = file_traversal_helper.specific_function("test").unwrap();

        let root = StatementTraversalHelper::new_root(&function_ref);
        let loop_statement = root.index(1);

        assert_eq!(
            vec![Symbol::Variable(&symbol2)],
            loop_statement.symbols_defined_directly_in()
        );
        let statement_ref = loop_statement.index(0);

        let actual = statement_ref
            .symbols_available_at()
            .unwrap()
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
        let file_traversal_helper = root_traversal_helper.specific_file("main.waso").unwrap();
        let function_ref = file_traversal_helper
            .specific_function("fibonacci")
            .unwrap();

        let root = function_ref.ref_to_implementation();
        let return_statement = root.index(3);

        let actual = return_statement
            .symbols_available_at()
            .unwrap()
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
            ),
            sample_codearea(),
        )]);

        let root_traversal_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_traversal_helper = root_traversal_helper.specific_file("main.waso").unwrap();
        let function_ref = file_traversal_helper
            .specific_function("fibonacci")
            .unwrap();

        let root = function_ref.ref_to_implementation();
        let return_statement = root.index(3);

        let actual = return_statement
            .symbols_available_at()
            .unwrap()
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
            ),
            CodeArea::new(
                CodeLocation::new(0, 0),
                CodeLocation::new(3, 1),
                CodeFile::new(PathBuf::from("add.waso")),
            )
            .unwrap(),
        );
        let add_file = File::new(
            "add".to_string(),
            Vec::new(),
            FunctionBlock::new(vec![add_function]),
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
                Import::new(
                    ImportRoot::ProjectRoot,
                    vec!["add".to_string(), "add".to_string()],
                ),
                CodeArea::new(
                    CodeLocation::new(0, 0),
                    CodeLocation::new(0, 15),
                    CodeFile::new(PathBuf::from("main.waso")),
                )
                .unwrap(),
            )],
            FunctionBlock::new(vec![main_function]),
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
            PathBuf::from("src"),
        ));

        let dth = DirectoryTraversalHelper::new_from_ast(&ast);
        let fth = dth.specific_file("main").unwrap();
        assert_eq!(
            vec![Symbol::Function(&add_fn_symbol)],
            fth.symbols().collect::<Vec<_>>()
        );
        assert_eq!(0, dth.len_subdirectories());
        assert_eq!(2, dth.len_files());
        assert_eq!(0, dth.subdirectories_iterator().count());
        assert_eq!(2, dth.files_iterator().count());
        assert_ne!(dth.index_file(0).inner(), dth.index_file(1).inner());
        assert_eq!(2, dth.inner().files().len())
    }
}

#[cfg(test)]
// Stuff that is needed in tests in the entire crate
pub(crate) mod test_shared {
    use crate::block::FunctionBlock;
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
                    File::new(
                        "main.waso".to_string(),
                        Vec::new(),
                        FunctionBlock::new(functions),
                    ),
                    main_path,
                )],
            ),
            src_path,
        ))
    }
}
