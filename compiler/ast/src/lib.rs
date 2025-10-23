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
use crate::expression::{Expression, Literal};
use crate::symbol::{FunctionSymbol, VariableSymbol};
use crate::top_level::{Function, TopLevelElement, TopLevelElementNode};
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use crate::id::Id;

pub mod block;
pub mod data_type;
pub mod expression;
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
    inner: Vec<TopLevelElementNode<Type>>,
}

impl<Type: ASTType> AST<Type> {
    pub fn new(inner: Vec<TopLevelElementNode<Type>>) -> Self {
        Self { inner }
    }

    pub fn functions(&self) -> impl Iterator<Item = &Function<Type>> {
        #[allow(irrefutable_let_patterns)]
        // This only temporarily matches for everything, more TopLevelElements to come
        self.inner.iter().filter_map(|element| {
            if let TopLevelElement::Function(func) = element.deref() {
                Some(func)
            } else {
                None
            }
        })
    }
}

impl<Type: ASTType> Deref for AST<Type> {
    type Target = [TopLevelElementNode<Type>];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<Type: ASTType> SemanticEquality for AST<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.inner.semantic_equals(&other.inner)
    }
}

/** This represents an AST Type and its location. Which type of AST node this is depends on its
generic
# Equality
Two different ExpressionNodes are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/

#[derive(Debug, PartialEq)]
pub struct ASTNode<T: Debug+PartialEq>
{
    id: Id,
    inner: T
}

impl<T: Debug+PartialEq> ASTNode<T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            id: Id::new(),
        }
    }
}

impl<T: Debug+PartialEq> Deref for ASTNode<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Debug+PartialEq> DerefMut for ASTNode<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: SemanticEquality+Debug+PartialEq> SemanticEquality for ASTNode<T> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.inner.semantic_equals(&other.inner)
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
    use crate::block::CodeBlock;
    use crate::data_type::DataType;
    use crate::expression::{BinaryOp, BinaryOpType, Expression, Literal};
    use crate::statement::{
        ControlStructure, Loop, LoopType, Return, Statement, StatementNode, VariableAssignment,
    };
    use crate::symbol::{FunctionSymbol, Symbol, VariableSymbol};
    use crate::top_level::{Function, TopLevelElement, TopLevelElementNode};
    use crate::traversal::function_traversal::FunctionTraversalHelper;
    use crate::traversal::statement_traversal::StatementTraversalHelper;
    use crate::{AST, TypedAST, UntypedAST, ASTNode};
    use std::rc::Rc;

    #[test]
    fn ast() {
        let symbol = Rc::new(VariableSymbol::new("test".to_string(), DataType::F32));
        let statement = StatementNode::new(Statement::VariableDeclaration(
            basic_test_variable(symbol.clone()).unwrap(),
        ));

        assert_eq!(
            Some(Symbol::Variable(&symbol)),
            statement.get_direct_symbol()
        );

        let function = Function::new(
            Rc::new(FunctionSymbol::new("test".to_string(), None, Vec::new())),
            StatementNode::new(Statement::Codeblock(CodeBlock::new(vec![statement]))),
        );

        let ast = AST::new(vec![TopLevelElementNode::new(TopLevelElement::Function(
            function,
        ))]);

        let function_ref = FunctionTraversalHelper::new(ast.functions().next().unwrap(), &ast);

        let root = StatementTraversalHelper::new_root(&function_ref);
        let statement_ref = root.index(0);
        assert_eq!(
            vec![Symbol::Function(function_ref.declaration())],
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
        let statement = StatementNode::new(Statement::Codeblock(CodeBlock::new(vec![
            StatementNode::new(Statement::VariableDeclaration(
                VariableAssignment::<TypedAST>::new(
                    symbol.clone(),
                    ASTNode::new(Expression::Literal(Literal::F32(10.0))),
                )
                .unwrap(),
            )),
            StatementNode::new(Statement::ControlStructure(Box::new(
                ControlStructure::Loop(Loop::new(
                    StatementNode::new(Statement::VariableDeclaration(
                        VariableAssignment::<TypedAST>::new(
                            symbol2.clone(),
                            ASTNode::new(Expression::Literal(Literal::Bool(true))),
                        )
                        .unwrap(),
                    )),
                    LoopType::Infinite,
                )),
            ))),
        ])));

        let function = Function::new(
            Rc::new(FunctionSymbol::new("test".to_string(), None, Vec::new())),
            statement,
        );

        let ast = AST::new(vec![TopLevelElementNode::new(TopLevelElement::Function(
            function,
        ))]);

        let function_ref = FunctionTraversalHelper::new(ast.functions().next().unwrap(), &ast);

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
            Symbol::Function(function_ref.declaration()),
        ];
        assert_eq!(actual.len(), expected.len());
        assert!(expected.iter().all(|val| actual.contains(val)));

        let actual = statement_ref
            .symbols_available_after()
            .unwrap()
            .collect::<Vec<_>>();
        let expected = vec![
            Symbol::Variable(&symbol),
            Symbol::Function(function_ref.declaration()),
            Symbol::Variable(&symbol2),
        ];
        assert_eq!(actual.len(), expected.len());
        assert!(expected.iter().all(|val| actual.contains(val)));
    }

    #[test]
    fn fibonacci_typed() {
        // The how manyth fibonacci number we want
        let nth = Rc::new(VariableSymbol::new("nth".to_string(), DataType::S32));
        let current = Rc::new(VariableSymbol::new("current".to_string(), DataType::S32));
        let previous = Rc::new(VariableSymbol::new("previous".to_string(), DataType::S32));
        let temp = Rc::new(VariableSymbol::new("temp".to_string(), DataType::S32));

        let fibonacci = Rc::new(FunctionSymbol::new(
            "fibonacci".to_string(),
            Some(DataType::S32),
            vec![nth.clone()],
        ));
        let ast = AST::new(vec![TopLevelElementNode::new(TopLevelElement::Function(
            Function::new(
                fibonacci.clone(),
                StatementNode::new(Statement::Codeblock(CodeBlock::new(vec![
                    StatementNode::new(Statement::VariableDeclaration(
                        VariableAssignment::<TypedAST>::new(
                            current.clone(),
                            ASTNode::new(Expression::Literal(Literal::S32(1))),
                        )
                        .unwrap(),
                    )),
                    StatementNode::new(Statement::VariableDeclaration(
                        VariableAssignment::<TypedAST>::new(
                            previous.clone(),
                            ASTNode::new(Expression::Literal(Literal::S32(0))),
                        )
                        .unwrap(),
                    )),
                    StatementNode::new(Statement::ControlStructure(Box::new(
                        ControlStructure::Loop(Loop::new(
                            StatementNode::new(Statement::Codeblock(CodeBlock::new(vec![
                                StatementNode::new(Statement::VariableDeclaration(
                                    VariableAssignment::<TypedAST>::new(
                                        temp.clone(),
                                        ASTNode::new(Expression::Variable(current.clone())),
                                    )
                                    .unwrap(),
                                )),
                                StatementNode::new(Statement::VariableAssignment(
                                    VariableAssignment::<TypedAST>::new(
                                        current.clone(),
                                        ASTNode::new(Expression::BinaryOp(Box::new(
                                            BinaryOp::<TypedAST>::new(
                                                BinaryOpType::Addition,
                                                ASTNode::new(Expression::Variable(
                                                    current.clone(),
                                                )),
                                                ASTNode::new(Expression::Variable(
                                                    previous.clone(),
                                                )),
                                            )
                                            .unwrap(),
                                        ))),
                                    )
                                    .unwrap(),
                                )),
                                StatementNode::new(Statement::VariableAssignment(
                                    VariableAssignment::<TypedAST>::new(
                                        previous.clone(),
                                        ASTNode::new(Expression::Variable(temp.clone())),
                                    )
                                    .unwrap(),
                                )),
                                StatementNode::new(Statement::VariableAssignment(
                                    VariableAssignment::<TypedAST>::new(
                                        nth.clone(),
                                        ASTNode::new(Expression::BinaryOp(Box::new(
                                            BinaryOp::<TypedAST>::new(
                                                BinaryOpType::Subtraction,
                                                ASTNode::new(Expression::Variable(
                                                    nth.clone(),
                                                )),
                                                ASTNode::new(Expression::Literal(
                                                    Literal::S32(1),
                                                )),
                                            )
                                            .unwrap(),
                                        ))),
                                    )
                                    .unwrap(),
                                )),
                            ]))),
                            LoopType::While(ASTNode::new(Expression::BinaryOp(Box::new(
                                BinaryOp::<TypedAST>::new(
                                    BinaryOpType::Greater,
                                    ASTNode::new(Expression::Variable(nth.clone())),
                                    ASTNode::new(Expression::Literal(Literal::S32(
                                        1, //The fibonacci number of 1 is 1
                                    ))),
                                )
                                .unwrap(),
                            )))),
                        )),
                    ))),
                    StatementNode::new(Statement::Return(Return::new(Some(ASTNode::new(
                        Expression::Variable(current.clone()),
                    ))))),
                ]))),
            ),
        ))]);

        let function_ref = FunctionTraversalHelper::new(ast.functions().next().unwrap(), &ast);

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
        let ast = AST::new(vec![TopLevelElementNode::new(TopLevelElement::Function(
            Function::new(
                fibonacci.clone(),
                StatementNode::new(Statement::Codeblock(CodeBlock::new(vec![
                    StatementNode::new(Statement::VariableDeclaration(VariableAssignment::<
                        UntypedAST,
                    >::new(
                        current.clone(),
                        ASTNode::new(Expression::Literal("1".to_string())),
                    ))),
                    StatementNode::new(Statement::VariableDeclaration(VariableAssignment::<
                        UntypedAST,
                    >::new(
                        previous.clone(),
                        ASTNode::new(Expression::Literal("0".to_string())),
                    ))),
                    StatementNode::new(Statement::ControlStructure(Box::new(
                        ControlStructure::Loop(Loop::new(
                            StatementNode::new(Statement::Codeblock(CodeBlock::new(vec![
                                StatementNode::new(Statement::VariableDeclaration(
                                    VariableAssignment::<UntypedAST>::new(
                                        temp.clone(),
                                        ASTNode::new(Expression::Variable(
                                            "current".to_string(),
                                        )),
                                    ),
                                )),
                                StatementNode::new(Statement::VariableAssignment(
                                    VariableAssignment::<UntypedAST>::new(
                                        current.clone(),
                                        ASTNode::new(Expression::BinaryOp(Box::new(
                                            BinaryOp::<UntypedAST>::new(
                                                BinaryOpType::Addition,
                                                ASTNode::new(Expression::Variable(
                                                    "current".to_string(),
                                                )),
                                                ASTNode::new(Expression::Variable(
                                                    "previous".to_string(),
                                                )),
                                            ),
                                        ))),
                                    ),
                                )),
                                StatementNode::new(Statement::VariableAssignment(
                                    VariableAssignment::<UntypedAST>::new(
                                        previous.clone(),
                                        ASTNode::new(Expression::Variable(
                                            "temp".to_string(),
                                        )),
                                    ),
                                )),
                                StatementNode::new(Statement::VariableAssignment(
                                    VariableAssignment::<UntypedAST>::new(
                                        nth.clone(),
                                        ASTNode::new(Expression::BinaryOp(Box::new(
                                            BinaryOp::<UntypedAST>::new(
                                                BinaryOpType::Subtraction,
                                                ASTNode::new(Expression::Variable(
                                                    "nth".to_string(),
                                                )),
                                                ASTNode::new(Expression::Literal(
                                                    "1".to_string(),
                                                )),
                                            ),
                                        ))),
                                    ),
                                )),
                            ]))),
                            LoopType::While(ASTNode::new(Expression::BinaryOp(Box::new(
                                BinaryOp::<UntypedAST>::new(
                                    BinaryOpType::Greater,
                                    ASTNode::new(Expression::Variable("nth".to_string())),
                                    ASTNode::new(Expression::Literal(
                                        "1".to_string(), //The fibonacci number of 1 is 1
                                    )),
                                ),
                            )))),
                        )),
                    ))),
                    StatementNode::new(Statement::Return(Return::new(Some(ASTNode::new(
                        Expression::Variable("current".to_string()),
                    ))))),
                ]))),
            ),
        ))]);

        let function_ref = FunctionTraversalHelper::new(ast.functions().next().unwrap(), &ast);

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

    fn basic_test_variable(
        symbol: Rc<VariableSymbol<TypedAST>>,
    ) -> Option<VariableAssignment<TypedAST>> {
        VariableAssignment::<TypedAST>::new(
            symbol,
            ASTNode::new(Expression::Literal(Literal::F32(14.0))),
        )
    }
}
