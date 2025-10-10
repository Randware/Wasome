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
use crate::expression::Literal;
use crate::symbol::{FunctionSymbol, VariableSymbol};
use crate::top_level::{Function, TopLevelElement};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

pub mod block;
pub mod data_type;
pub mod expression;
pub mod statement;
pub mod symbol;
pub mod top_level;
pub mod traversal;

#[derive(Debug, PartialEq)]
pub struct AST<Type: ASTType> {
    inner: Vec<TopLevelElement<Type>>,
}

impl<Type: ASTType> AST<Type> {
    pub fn new(inner: Vec<TopLevelElement<Type>>) -> Self {
        Self { inner }
    }

    pub fn functions(&self) -> impl Iterator<Item = &Function<Type>> {
        #[allow(irrefutable_let_patterns)]
        // This only temporarily matches for everything, more TopLevelElements to come
        self.inner.iter().filter_map(|element| {
            if let TopLevelElement::Function(func) = element {
                Some(func)
            } else {
                None
            }
        })
    }
}

impl<Type: ASTType> Deref for AST<Type> {
    type Target = [TopLevelElement<Type>];

    fn deref(&self) -> &Self::Target {
        &self.inner
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
        ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
        VariableDeclaration,
    };
    use crate::symbol::{FunctionSymbol, Symbol, VariableSymbol};
    use crate::top_level::{Function, TopLevelElement};
    use crate::traversal::function_traversal::FunctionTraversalHelper;
    use crate::traversal::statement_traversal::StatementTraversalHelper;
    use crate::{AST, TypedAST, UntypedAST};
    use std::rc::Rc;

    #[test]
    fn ast() {
        let symbol = Rc::new(VariableSymbol::new("test".to_string(), DataType::F32));
        let statement =
            Statement::VariableDeclaration(basic_test_variable(symbol.clone()).unwrap());

        assert_eq!(
            Some(Symbol::Variable(&symbol)),
            statement.get_direct_symbol()
        );

        let function = Function::new(
            Rc::new(FunctionSymbol::new("test".to_string(), None, Vec::new())),
            Statement::Codeblock(CodeBlock::new(vec![statement])),
        );

        let ast = AST::new(vec![TopLevelElement::Function(function)]);

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
        let statement = Statement::Codeblock(CodeBlock::new(vec![
            Statement::VariableDeclaration(
                VariableDeclaration::<TypedAST>::new(
                    symbol.clone(),
                    Expression::Literal(Literal::F32(10.0)),
                )
                .unwrap(),
            ),
            Statement::ControlStructure(Box::new(ControlStructure::Loop(Loop::new(
                Statement::VariableDeclaration(
                    VariableDeclaration::<TypedAST>::new(
                        symbol2.clone(),
                        Expression::Literal(Literal::Bool(true)),
                    )
                    .unwrap(),
                ),
                LoopType::Infinite,
            )))),
        ]));

        let function = Function::new(
            Rc::new(FunctionSymbol::new("test".to_string(), None, Vec::new())),
            statement,
        );

        let ast = AST::new(vec![TopLevelElement::Function(function)]);

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
        let ast = AST::new(vec![TopLevelElement::Function(Function::new(
            fibonacci.clone(),
            Statement::Codeblock(CodeBlock::new(vec![
                Statement::VariableDeclaration(
                    VariableDeclaration::<TypedAST>::new(
                        current.clone(),
                        Expression::Literal(Literal::S32(1)),
                    )
                    .unwrap(),
                ),
                Statement::VariableDeclaration(
                    VariableDeclaration::<TypedAST>::new(
                        previous.clone(),
                        Expression::Literal(Literal::S32(0)),
                    )
                    .unwrap(),
                ),
                Statement::ControlStructure(Box::new(ControlStructure::Loop(Loop::new(
                    Statement::Codeblock(CodeBlock::new(vec![
                        Statement::VariableDeclaration(
                            VariableDeclaration::<TypedAST>::new(
                                temp.clone(),
                                Expression::Variable(current.clone()),
                            )
                            .unwrap(),
                        ),
                        Statement::VariableAssignment(
                            VariableAssignment::<TypedAST>::new(
                                current.clone(),
                                Expression::BinaryOp(Box::new(
                                    BinaryOp::<TypedAST>::new(
                                        BinaryOpType::Addition,
                                        Expression::Variable(current.clone()),
                                        Expression::Variable(previous.clone()),
                                    )
                                    .unwrap(),
                                )),
                            )
                            .unwrap(),
                        ),
                        Statement::VariableAssignment(
                            VariableAssignment::<TypedAST>::new(
                                previous.clone(),
                                Expression::Variable(temp.clone()),
                            )
                            .unwrap(),
                        ),
                        Statement::VariableAssignment(
                            VariableAssignment::<TypedAST>::new(
                                nth.clone(),
                                Expression::BinaryOp(Box::new(
                                    BinaryOp::<TypedAST>::new(
                                        BinaryOpType::Subtraction,
                                        Expression::Variable(nth.clone()),
                                        Expression::Literal(Literal::S32(1)),
                                    )
                                    .unwrap(),
                                )),
                            )
                            .unwrap(),
                        ),
                    ])),
                    LoopType::While(Expression::BinaryOp(Box::new(
                        BinaryOp::<TypedAST>::new(
                            BinaryOpType::Greater,
                            Expression::Variable(nth.clone()),
                            Expression::Literal(Literal::S32(
                                1, //The fibonacci number of 1 is 1
                            )),
                        )
                        .unwrap(),
                    ))),
                )))),
                Statement::Return(Return::new(Some(Expression::Variable(current.clone())))),
            ])),
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
        let ast = AST::new(vec![TopLevelElement::Function(Function::new(
            fibonacci.clone(),
            Statement::Codeblock(CodeBlock::new(vec![
                Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                    current.clone(),
                    Expression::Literal("1".to_string()),
                )),
                Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                    previous.clone(),
                    Expression::Literal("0".to_string()),
                )),
                Statement::ControlStructure(Box::new(ControlStructure::Loop(Loop::new(
                    Statement::Codeblock(CodeBlock::new(vec![
                        Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                            temp.clone(),
                            Expression::Variable("current".to_string()),
                        )),
                        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
                            "current".to_string(),
                            Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                                BinaryOpType::Addition,
                                Expression::Variable("current".to_string()),
                                Expression::Variable("previous".to_string()),
                            ))),
                        )),
                        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
                            "previous".to_string(),
                            Expression::Variable("temp".to_string()),
                        )),
                        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
                            "nth".to_string(),
                            Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                                BinaryOpType::Subtraction,
                                Expression::Variable("nth".to_string()),
                                Expression::Literal("1".to_string()),
                            ))),
                        )),
                    ])),
                    LoopType::While(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Greater,
                        Expression::Variable("nth".to_string()),
                        Expression::Literal(
                            "1".to_string(), //The fibonacci number of 1 is 1
                        ),
                    )))),
                )))),
                Statement::Return(Return::new(Some(Expression::Variable(
                    "current".to_string(),
                )))),
            ])),
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
    ) -> Option<VariableDeclaration<TypedAST>> {
        VariableDeclaration::<TypedAST>::new(symbol, Expression::Literal(Literal::F32(14.0)))
    }
}
