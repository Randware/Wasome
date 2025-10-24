use crate::block::CodeBlock;
use crate::data_type::{DataType, Typed};
use crate::expression::Expression;
use crate::symbol::{FunctionCall, Symbol, VariableSymbol};
use crate::{ASTNode, ASTType, SemanticEquality, TypedAST, UntypedAST, eq_return_option};
use std::cmp::PartialEq;
use std::ops::Index;
use std::rc::Rc;

/** This represents a Statement as per section 4 of the lang spec
# Equality
Two different Statements are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/
#[derive(Debug, PartialEq)]
pub enum Statement<Type: ASTType> {
    // Assignment to existing variable
    VariableAssignment(VariableAssignment<Type>),
    // Creation of new variable
    VariableDeclaration(VariableAssignment<Type>),
    Expression(ASTNode<Expression<Type>>),
    Return(Return<Type>),
    ControlStructure(Box<ControlStructure<Type>>),
    Codeblock(CodeBlock<Type>),
    // A call of a void function
    // It can't be an expression as it does not have a return type
    VoidFunctionCall(FunctionCall<Type>),
    // Terminates a loop
    Break,
}

impl<Type: ASTType> SemanticEquality for Statement<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        use Statement as St;
        match (self, other) {
            (St::VariableAssignment(inner), St::VariableAssignment(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            (St::VariableDeclaration(inner), St::VariableDeclaration(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            (St::Expression(inner), St::Expression(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            (St::Return(inner), St::Return(other_inner)) => inner.semantic_equals(other_inner),
            (St::ControlStructure(inner), St::ControlStructure(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            (St::Codeblock(inner), St::Codeblock(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            // All cases where equality == semantic equality
            _ => self == other,
        }
    }
}

impl<Type: ASTType> Statement<Type> {
    /** Gets the symbol defined in this expression
    Only this is considered, while subexpressions are ignored
    @return
    Some(symbol) if symbol is defined here
    None if no symbols are defined here
    */
    pub fn get_direct_symbol(&self) -> Option<Symbol<'_, Type>> {
        match self {
            Statement::VariableDeclaration(inner) => Some(Symbol::Variable(inner.variable())),
            _ => None,
        }
    }

    /** Gets the length of the child statements
     */
    pub fn len_children(&self) -> usize {
        match self {
            Statement::ControlStructure(structure) => structure.child_len(),
            Statement::Codeblock(codeblock) => codeblock.len(),
            _ => 0,
        }
    }
}

impl<Type: ASTType> Index<usize> for Statement<Type> {
    type Output = ASTNode<Statement<Type>>;

    /** Gets the indexth child statement
    panics if self has no children or index is out of bounds
    */
    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Statement::Codeblock(block) => &block[index],
            Statement::ControlStructure(structure) => structure.child_statement_at(index),
            _ => panic!("This has no child members!"),
        }
    }
}

/** This represents an assignement to a variable. If this variable doesn't exist previously, it is created
# Equality
Two different VariableAssignement are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/
#[derive(Debug, PartialEq)]
pub struct VariableAssignment<Type: ASTType> {
    variable: Rc<VariableSymbol<Type>>,
    value: ASTNode<Expression<Type>>,
}

impl<Type: ASTType> SemanticEquality for VariableAssignment<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.variable == other.variable && self.value.semantic_equals(&other.value)
    }
}

impl VariableAssignment<TypedAST> {
    /** Tries to create a new instance
       returns None if the type of the variable symbol and the return type of the expression doesn't
       match
    */
    pub fn new(
        variable: Rc<VariableSymbol<TypedAST>>,
        value: ASTNode<Expression<TypedAST>>,
    ) -> Option<Self> {
        eq_return_option(*variable.data_type(), value.data_type())?;
        Some(Self { variable, value })
    }
}

impl VariableAssignment<UntypedAST> {
    /** Creates a new instance
     */
    pub fn new(
        variable: Rc<VariableSymbol<UntypedAST>>,
        value: ASTNode<Expression<UntypedAST>>,
    ) -> Self {
        Self { variable, value }
    }
}

impl<Type: ASTType> VariableAssignment<Type> {
    pub fn variable(&self) -> &VariableSymbol<Type> {
        &self.variable
    }

    /** Gets the variable symbol by cloning the underlying RC
     */
    pub fn variable_owned(&self) -> Rc<VariableSymbol<Type>> {
        self.variable.clone()
    }

    pub fn value(&self) -> &ASTNode<Expression<Type>> {
        &self.value
    }
}

/** This represents a control structure as defined in chapters 8 and 13 of the lang spec
# Equality
Two different ControlStructures are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/
#[derive(Debug, PartialEq)]
pub enum ControlStructure<Type: ASTType> {
    Conditional(Conditional<Type>),
    Loop(Loop<Type>),
}

impl<Type: ASTType> SemanticEquality for ControlStructure<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        match (self, other) {
            (ControlStructure::Conditional(inner), ControlStructure::Conditional(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            (ControlStructure::Loop(inner), ControlStructure::Loop(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            _ => false,
        }
    }
}

impl<Type: ASTType> ControlStructure<Type> {
    /** Returns the number of child statements
     */
    pub fn child_len(&self) -> usize {
        match self {
            ControlStructure::Conditional(inner) => inner.len(),
            ControlStructure::Loop(inner) => inner.len(),
        }
    }

    /** Returns the child statement at index
     */
    pub(crate) fn child_statement_at(&self, index: usize) -> &ASTNode<Statement<Type>> {
        match self {
            ControlStructure::Conditional(cond) => cond.child_statement_at(index),
            ControlStructure::Loop(inner) => inner.child_statement_at(index),
        }
    }
}

/** This represents a conditional as defined in chapter 8 of the lang spec
# Equality
Two different Conditionals are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/
#[derive(Debug, PartialEq)]
pub struct Conditional<Type: ASTType> {
    condition: ASTNode<Expression<Type>>,
    then_statement: ASTNode<Statement<Type>>,
    else_statement: Option<ASTNode<Statement<Type>>>,
}

impl<Type: ASTType> Conditional<Type> {
    pub fn new(
        condition: ASTNode<Expression<Type>>,
        then_statement: ASTNode<Statement<Type>>,
        else_statement: Option<ASTNode<Statement<Type>>>,
    ) -> Self {
        Self {
            condition,
            then_statement,
            else_statement,
        }
    }

    /** Returns the number of child statements
     */
    pub(crate) fn len(&self) -> usize {
        1 + self.else_statement.is_some() as usize
    }

    pub fn condition(&self) -> &ASTNode<Expression<Type>> {
        &self.condition
    }

    pub fn then_statement(&self) -> &ASTNode<Statement<Type>> {
        &self.then_statement
    }

    pub fn else_statement(&self) -> Option<&ASTNode<Statement<Type>>> {
        self.else_statement.as_ref()
    }

    /** Returns the child statement at index
    0 is the then-statement
    1 is the else-statement
    */
    fn child_statement_at(&self, index: usize) -> &ASTNode<Statement<Type>> {
        match index {
            0 => &self.then_statement,
            1 => self.else_statement.as_ref().unwrap(),
            _ => panic!("Index is out of bounds"),
        }
    }
}

impl<Type: ASTType> SemanticEquality for Conditional<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.condition.semantic_equals(&other.condition)
            && self.then_statement.semantic_equals(&other.then_statement)
            // Check if both are some and compare then
            // Or both are none
            && self.else_statement.as_ref().zip(other.else_statement.as_ref())
            .map(|(a,b)| a.semantic_equals(b))
            .unwrap_or(self.else_statement.is_none() && other.else_statement.is_none())
    }
}

/** This represents a conditional as defined in chapter 13 of the lang spec
# Equality
Two different Loops are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/
#[derive(Debug, PartialEq)]
pub struct Loop<Type: ASTType> {
    to_loop_on: ASTNode<Statement<Type>>,
    loop_type: LoopType<Type>,
}

impl<Type: ASTType> Loop<Type> {
    pub fn new(to_loop_on: ASTNode<Statement<Type>>, loop_type: LoopType<Type>) -> Self {
        Self {
            to_loop_on,
            loop_type,
        }
    }

    /** Returns the number of child statements
     */
    pub(crate) fn len(&self) -> usize {
        self.loop_type.len() + 1
    }

    pub fn to_loop_on(&self) -> &Statement<Type> {
        &self.to_loop_on
    }

    pub fn loop_type(&self) -> &LoopType<Type> {
        &self.loop_type
    }

    /** Returns the child statement at index
     */
    fn child_statement_at(&self, index: usize) -> &ASTNode<Statement<Type>> {
        // The after each statement comes after the looped on code
        // So it needs special handling
        if let LoopType::For {
            start,
            cond: _cond,
            after_each,
        } = &self.loop_type
        {
            return match index {
                0 => start,
                1 => &self.to_loop_on,
                2 => after_each,
                _ => panic!("Index out of bounds!"),
            };
        }
        if index == self.loop_type.len() {
            &self.to_loop_on
        } else {
            self.loop_type.child_statement_at(index)
        }
    }
}

impl<Type: ASTType> SemanticEquality for Loop<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.to_loop_on.semantic_equals(&other.to_loop_on)
            && self.loop_type.semantic_equals(&other.loop_type)
    }
}

/** This is the type of a loop
*/
#[derive(Debug, PartialEq)]
pub enum LoopType<Type: ASTType> {
    Infinite,
    While(ASTNode<Expression<Type>>),
    For {
        start: ASTNode<Statement<Type>>,
        cond: ASTNode<Expression<Type>>,
        after_each: ASTNode<Statement<Type>>,
    },
}

impl<Type: ASTType> LoopType<Type> {
    /** Returns the number of child statements
     */
    pub(crate) fn len(&self) -> usize {
        match self {
            LoopType::Infinite => 0,
            LoopType::While(_) => 0,
            LoopType::For { .. } => 2,
        }
    }

    /** Returns the child statement at index
     */
    fn child_statement_at(&self, index: usize) -> &ASTNode<Statement<Type>> {
        if let LoopType::For {
            start,
            cond: _cond,
            after_each,
        } = self
        {
            match index {
                0 => start,
                1 => after_each,
                _ => panic!("Index is out of bounds"),
            }
        } else {
            panic!("This loop type has no children!");
        }
    }
}

impl<Type: ASTType> SemanticEquality for LoopType<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        match (self, other) {
            (LoopType::Infinite, LoopType::Infinite) => true,
            (LoopType::While(inner), LoopType::While(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            (
                LoopType::For {
                    start,
                    cond,
                    after_each,
                },
                LoopType::For {
                    start: other_start,
                    cond: other_cond,
                    after_each: other_after_each,
                },
            ) => {
                start.semantic_equals(other_start)
                    && cond.semantic_equals(other_cond)
                    && after_each.semantic_equals(other_after_each)
            }
            _ => false,
        }
    }
}

/** A return in wasome code
This is a wrapper around Expression with the wrapped one being the one's result that will be returned
# Equality
Two different Returns are never equal.
Use semantic_equals from [`SemanticEquality`] to check semantics only
*/
#[derive(Debug, PartialEq)]
pub struct Return<Type: ASTType> {
    to_return: Option<ASTNode<Expression<Type>>>,
}

impl<Type: ASTType> Return<Type> {
    pub fn new(to_return: Option<ASTNode<Expression<Type>>>) -> Self {
        Self { to_return }
    }

    pub fn to_return(&self) -> Option<&ASTNode<Expression<Type>>> {
        self.to_return.as_ref()
    }
}

impl<Type: ASTType> SemanticEquality for Return<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.to_return.semantic_equals(&other.to_return)
    }
}

impl Return<TypedAST> {
    /** Gets the type being returned
       Returns none if nothing
       And Some(type) if an expression with type is being returned
    */
    pub fn return_type(&self) -> Option<DataType> {
        // Gets the type from the expression
        self.to_return().map(|val| val.data_type())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TypedAST;
    use crate::data_type::DataType;
    use crate::expression::{Expression, Literal};
    #[test]
    fn variable_assignement() {
        basic_test_variable(Rc::new(VariableSymbol::new(
            "test".to_string(),
            DataType::F32,
        )))
        .unwrap();
    }


    #[test]
    fn for_loop_inner_order() {
        let cond = create_literal_expr(Literal::Bool(true));
        let before = create_literal_statement(Literal::S32(1));
        let after = create_literal_statement(Literal::S32(2));
        let inner = create_literal_statement(Literal::S32(3));
        let for_loop = Loop::new(inner, LoopType::For {start: before, cond, after_each: after});
        assert!(for_loop.child_statement_at(0).semantic_equals(
            &create_literal_statement(Literal::S32(1))
        ));
        assert!(for_loop.child_statement_at(1).semantic_equals(
            &create_literal_statement(Literal::S32(3))
        ));

        assert!(for_loop.child_statement_at(2).semantic_equals(
            &create_literal_statement(Literal::S32(2))
        ));
    }

    #[test]
    fn break_statement_semantic_equality_should_be_true()
    {
        let break_statement: Statement<TypedAST> = Statement::Break;
        assert!(break_statement.semantic_equals(&break_statement))
    }

    fn create_literal_expr(literal: Literal) -> ASTNode<Expression<TypedAST>>
    {
        ASTNode::new(Expression::Literal(literal))
    }

    fn create_literal_statement(literal: Literal) -> ASTNode<Statement<TypedAST>>
    {
        ASTNode::new(Statement::Expression(create_literal_expr(literal)))
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
