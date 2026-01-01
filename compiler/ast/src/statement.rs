use crate::data_type::{DataType, Typed};
use crate::expression::{Expression, FunctionCall};
use crate::symbol::{Symbol, VariableSymbol};
use crate::{ASTNode, ASTType, SemanticEq, TypedAST, UntypedAST, eq_return_option};
use std::cmp::PartialEq;
use std::ops::{Deref, Index};
use std::rc::Rc;

/// A Statement as per section 4 of the lang spec
///
/// # Equality
///
/// Two different Statements are never equal.
/// Use semantic_equals from [`SemanticEq`] to check semantics only
#[derive(Debug, PartialEq)]
pub enum Statement<Type: ASTType> {
    // Assignment to existing variable
    VariableAssignment(VariableAssignment<Type>),
    // Creation of new variable
    VariableDeclaration(VariableDeclaration<Type>),
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

impl<Type: ASTType> SemanticEq for Statement<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        use Statement as St;
        match (self, other) {
            (St::VariableAssignment(inner), St::VariableAssignment(other_inner)) => {
                inner.semantic_eq(other_inner)
            }
            (St::VariableDeclaration(inner), St::VariableDeclaration(other_inner)) => {
                inner.semantic_eq(other_inner)
            }
            (St::Expression(inner), St::Expression(other_inner)) => {
                inner.semantic_eq(other_inner)
            }
            (St::Return(inner), St::Return(other_inner)) => inner.semantic_eq(other_inner),
            (St::ControlStructure(inner), St::ControlStructure(other_inner)) => {
                inner.semantic_eq(other_inner)
            }
            (St::Codeblock(inner), St::Codeblock(other_inner)) => {
                inner.semantic_eq(other_inner)
            }
            // All cases where equality == semantic equality
            _ => self == other,
        }
    }
}

impl<Type: ASTType> Statement<Type> {
    /// Gets the symbol defined in this statement
    /// Only symbols that can be accessed by following statements in the
    /// following scope are considered. For example, variables
    ///
    /// # Return
    ///
    /// - Some(symbol) if symbol is defined here
    /// - None if no symbols are defined here
    pub fn get_direct_symbol(&self) -> Option<&VariableSymbol<Type>> {
        match self {
            Statement::VariableDeclaration(inner) => Some(inner.variable()),
            _ => None,
        }
    }

    /// Same as `get_direct_symbol`, except that the [`Symbol`] struct is used
    pub fn get_direct_symbol_reference_struct(&self) -> Option<Symbol<'_, Type>> {
        self.get_direct_symbol().map(Symbol::Variable)
    }

    /// Gets the length of the child statements
    pub fn amount_children(&self) -> usize {
        match self {
            Statement::ControlStructure(structure) => structure.child_len(),
            Statement::Codeblock(codeblock) => codeblock.len(),
            _ => 0,
        }
    }
}

impl<Type: ASTType> Index<usize> for Statement<Type> {
    type Output = ASTNode<Statement<Type>>;

    /// Gets the indexth child statement
    // panics if self has no children or index is out of bounds
    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Statement::Codeblock(block) => &block[index],
            Statement::ControlStructure(structure) => structure.child_statement_at(index),
            _ => panic!("This has no child members!"),
        }
    }
}

/// This represents an assignement to a variable. If this variable doesn't exist previously, it is created
///
/// # Equality
///
/// Two different VariableAssignement are never equal.
/// Use semantic_equals from [`SemanticEq`] to check semantics only
#[derive(Debug, PartialEq)]
pub struct VariableDeclaration<Type: ASTType> {
    variable: Rc<VariableSymbol<Type>>,
    value: ASTNode<Expression<Type>>,
}

impl<Type: ASTType> SemanticEq for VariableDeclaration<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.variable().semantic_eq(other.variable()) && self.value.semantic_eq(&other.value)
    }
}

impl VariableDeclaration<TypedAST> {
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

impl VariableDeclaration<UntypedAST> {
    /** Creates a new instance
     */
    pub fn new(
        variable: Rc<VariableSymbol<UntypedAST>>,
        value: ASTNode<Expression<UntypedAST>>,
    ) -> Self {
        Self { variable, value }
    }
}

impl<Type: ASTType> VariableDeclaration<Type> {
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

/** This represents an assignment to a variable.
*/
#[derive(Debug, PartialEq)]
pub struct VariableAssignment<Type: ASTType> {
    variable: Type::VariableUse,
    value: ASTNode<Expression<Type>>,
}

impl VariableAssignment<TypedAST> {
    /// Tries to create a new instance
    /// returns None if the type of the variable symbol and the return type of the expression doesn't
    /// match
    pub fn new(
        variable: Rc<VariableSymbol<TypedAST>>,
        value: ASTNode<Expression<TypedAST>>,
    ) -> Option<Self> {
        eq_return_option(*variable.data_type(), value.data_type())?;
        Some(Self { variable, value })
    }
}

impl VariableAssignment<UntypedAST> {
    /// Creates a new instance
    pub fn new(
        variable: String,
        value: ASTNode<Expression<UntypedAST>>,
    ) -> Self {
        Self { variable, value }
    }
}

impl<Type: ASTType> VariableAssignment<Type> {
    pub fn variable(&self) -> &Type::VariableUse {
        &self.variable
    }
    
    pub fn value(&self) -> &ASTNode<Expression<Type>> {
        &self.value
    }
}

impl<Type: ASTType> SemanticEq for VariableAssignment<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.variable().semantic_eq(other.variable()) && self.value.semantic_eq(&other.value)
    }
}

/// This represents a control structure as defined in chapters 8 and 13 of the lang spec
///
/// # Equality
///
/// Two different ControlStructures are never equal.
/// Use semantic_equals from [`SemanticEq`] to check semantics only
#[derive(Debug, PartialEq)]
pub enum ControlStructure<Type: ASTType> {
    Conditional(Conditional<Type>),
    Loop(Loop<Type>),
}

impl<Type: ASTType> SemanticEq for ControlStructure<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ControlStructure::Conditional(inner), ControlStructure::Conditional(other_inner)) => {
                inner.semantic_eq(other_inner)
            }
            (ControlStructure::Loop(inner), ControlStructure::Loop(other_inner)) => {
                inner.semantic_eq(other_inner)
            }
            _ => false,
        }
    }
}

impl<Type: ASTType> ControlStructure<Type> {
    /// Returns the number of direct child statements
    ///
    /// This includes the before and after statements of the for loop
    pub fn child_len(&self) -> usize {
        match self {
            ControlStructure::Conditional(inner) => inner.len(),
            ControlStructure::Loop(inner) => inner.len(),
        }
    }

    /// Returns the child statement at index
    ///
    /// The available statements are the same as in [`child_len`](ControlStructure::child_len)
    pub(crate) fn child_statement_at(&self, index: usize) -> &ASTNode<Statement<Type>> {
        match self {
            ControlStructure::Conditional(cond) => cond.child_statement_at(index),
            ControlStructure::Loop(inner) => inner.child_statement_at(index),
        }
    }
}

/// This represents a conditional as defined in chapter 8 of the lang spec
///
/// # Equality
///
/// Two different Conditionals are never equal.
/// Use semantic_equals from [`SemanticEq`] to check semantics only
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

    /// Returns the number of direct child statements
    ///
    /// This includes the if and the else statement, if it exists
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

    /// Returns the child statement at index
    /// 0 is the then-statement
    /// 1 is the else-statement
    ///
    /// # Panics
    ///
    /// Panics if index is out of bounds.
    /// - This is if:
    ///     - There is an else statement and index >= 2
    ///     - There is no else statement and index >= 1
    fn child_statement_at(&self, index: usize) -> &ASTNode<Statement<Type>> {
        match index {
            0 => &self.then_statement,
            1 => self.else_statement.as_ref().unwrap(),
            _ => panic!("Index is out of bounds"),
        }
    }
}

impl<Type: ASTType> SemanticEq for Conditional<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.condition.semantic_eq(&other.condition)
            && self.then_statement.semantic_eq(&other.then_statement)
            // Check if both are some and compare then
            // Or both are none
            && self.else_statement.as_ref().zip(other.else_statement.as_ref())
            .map(|(a,b)| a.semantic_eq(b))
            .unwrap_or(self.else_statement.is_none() && other.else_statement.is_none())
    }
}

/// This represents a loop as defined in chapter 13 of the lang spec
///
/// # Equality
/// Two different Loops are never equal.
/// Use semantic_equals from [`SemanticEq`] to check semantics only
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

    /// Returns the number of direct child statements
    ///
    /// This includes the before and after statements of the for loop
    pub(crate) fn len(&self) -> usize {
        self.loop_type.len() + 1
    }

    pub fn to_loop_on(&self) -> &Statement<Type> {
        &self.to_loop_on
    }

    pub fn loop_type(&self) -> &LoopType<Type> {
        &self.loop_type
    }

    /// Returns the child statement at index
    ///
    /// The available statements are the same as in [`child_len`](Loop::len)
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

impl<Type: ASTType> SemanticEq for Loop<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.to_loop_on.semantic_eq(&other.to_loop_on)
            && self.loop_type.semantic_eq(&other.loop_type)
    }
}

/// This is the type of a loop
///
/// It includes all type-specific elements (e.g.: the before Statement of a for loop
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
    /// Returns the number of child statements
    ///
    /// This is two for a for loop and zero for everything else
    pub fn len(&self) -> usize {
        match self {
            LoopType::Infinite => 0,
            LoopType::While(_) => 0,
            LoopType::For { .. } => 2,
        }
    }

    /// Returns the child statement at index
    ///
    /// The available statement are the same as in [`child_len`](LoopType::len)
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

impl<Type: ASTType> SemanticEq for LoopType<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoopType::Infinite, LoopType::Infinite) => true,
            (LoopType::While(inner), LoopType::While(other_inner)) => {
                inner.semantic_eq(other_inner)
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
                start.semantic_eq(other_start)
                    && cond.semantic_eq(other_cond)
                    && after_each.semantic_eq(other_after_each)
            }
            _ => false,
        }
    }
}

/// A return in wasome code
/// This is a wrapper around Expression with the wrapped one being the one's result that will be returned
/// # Equality
/// Two different Returns are never equal.
/// Use semantic_equals from [`SemanticEq`] to check semantics only
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

impl<Type: ASTType> SemanticEq for Return<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.to_return.semantic_eq(&other.to_return)
    }
}

impl Return<TypedAST> {
    /// Gets the type being returned
    ///
    /// Returns none if nothing
    /// And Some(type) if an expression with type is being returned
    pub fn return_type(&self) -> Option<DataType> {
        // Gets the type from the expression
        self.to_return().map(|val| val.data_type())
    }
}

#[derive(Debug, PartialEq)]
pub struct CodeBlock<Type: ASTType> {
    contents: Vec<ASTNode<Statement<Type>>>,
}

impl<Type: ASTType> CodeBlock<Type> {
    pub fn new(contents: Vec<ASTNode<Statement<Type>>>) -> Self {
        Self { contents }
    }
}

impl<Type: ASTType> Deref for CodeBlock<Type> {
    type Target = [ASTNode<Statement<Type>>];

    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl<Type: ASTType> SemanticEq for CodeBlock<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.contents.semantic_eq(&other.contents)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TypedAST;
    use crate::data_type::DataType;
    use crate::expression::{Expression, Literal};
    use crate::test_shared::{basic_test_variable, sample_codearea};

    #[test]
    fn variable_assignement() {
        basic_test_variable(Rc::new(VariableSymbol::new(
            "test".to_string(),
            DataType::F64,
        )))
        .unwrap();
    }

    #[test]
    fn for_loop_inner_order() {
        let cond = create_literal_expr(Literal::Bool(true));
        let before = create_literal_statement(Literal::S32(1));
        let after = create_literal_statement(Literal::S32(2));
        let inner = create_literal_statement(Literal::S32(3));
        let for_loop = Loop::new(
            inner,
            LoopType::For {
                start: before,
                cond,
                after_each: after,
            },
        );
        assert!(
            for_loop
                .child_statement_at(0)
                .semantic_eq(&create_literal_statement(Literal::S32(1)))
        );
        assert!(
            for_loop
                .child_statement_at(1)
                .semantic_eq(&create_literal_statement(Literal::S32(3)))
        );

        assert!(
            for_loop
                .child_statement_at(2)
                .semantic_eq(&create_literal_statement(Literal::S32(2)))
        );
    }

    #[test]
    fn break_statement_semantic_equality_should_be_true() {
        let break_statement: Statement<TypedAST> = Statement::Break;
        assert!(break_statement.semantic_eq(&break_statement))
    }

    fn create_literal_expr(literal: Literal) -> ASTNode<Expression<TypedAST>> {
        ASTNode::new(Expression::Literal(literal), sample_codearea())
    }

    fn create_literal_statement(literal: Literal) -> ASTNode<Statement<TypedAST>> {
        ASTNode::new(
            Statement::Expression(create_literal_expr(literal)),
            sample_codearea(),
        )
    }
}
