use crate::block::CodeBlock;
use crate::data_type::{DataType, Typed};
use crate::expression::Expression;
use crate::symbol::{FunctionCall, Symbol, VariableSymbol};
use crate::{ASTType, TypedAST, UntypedAST, eq_return_option};
use std::cmp::PartialEq;
use std::ops::Index;
use std::rc::Rc;

/** This represents a Statement as per section 4 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum Statement<Type: ASTType> {
    // Assignment to existing variable
    VariableAssignment(VariableAssignment<Type>),
    // Creation of new variable
    VariableDeclaration(VariableDeclaration<Type>),
    Expression(Expression<Type>),
    Return(Return<Type>),
    ControlStructure(Box<ControlStructure<Type>>),
    Codeblock(CodeBlock<Type>),
    // A call of a void function
    // It can't be an expression as it does not have a return type
    VoidFunctionCall(FunctionCall<Type>),
}

impl<Type: ASTType> Statement<Type> {
    /** Gets the symbol defined in this expression
    Only this is considered, while subexpressions are ignored
    @return
    Some(symbol) if symbol is defined here
    None if no symbols are defined here
    */
    pub fn get_direct_symbol(&self) -> Option<Symbol<Type>> {
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
    type Output = Statement<Type>;

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

/** This represents a declaration of a variable.
*/
#[derive(Debug, PartialEq)]
pub struct VariableDeclaration<Type: ASTType> {
    variable: Rc<VariableSymbol<Type>>,
    value: Expression<Type>,
}

impl VariableDeclaration<TypedAST> {
    /** Tries to create a new instance
          returns None if the type of the variable symbol and the return type of the expression doesn't
          match
    */
    pub fn new(
        variable: Rc<VariableSymbol<TypedAST>>,
        value: Expression<TypedAST>,
    ) -> Option<Self> {
        eq_return_option(*variable.data_type(), value.data_type())?;
        Some(Self { variable, value })
    }
}

impl VariableDeclaration<UntypedAST> {
    /** Creates a new instance
     */
    pub fn new(variable: Rc<VariableSymbol<UntypedAST>>, value: Expression<UntypedAST>) -> Self {
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

    pub fn value(&self) -> &Expression<Type> {
        &self.value
    }
}

/** This represents an assignment to a variable.
*/
#[derive(Debug, PartialEq)]
pub struct VariableAssignment<Type: ASTType> {
    variable: Type::VariableUse,
    value: Expression<Type>,
}

impl VariableAssignment<TypedAST> {
    /** Tries to create a new assignment to a variable
             returns None if the type of the variable symbol and the return type of the expression doesn't
             match
    */
    pub fn new(
        variable: Rc<VariableSymbol<TypedAST>>,
        value: Expression<TypedAST>,
    ) -> Option<Self> {
        eq_return_option(*variable.data_type(), value.data_type())?;
        Some(Self { variable, value })
    }
}

impl VariableAssignment<UntypedAST> {
    /** Creates a new instance
     */
    pub fn new(variable: String, value: Expression<UntypedAST>) -> Self {
        Self { variable, value }
    }
}

impl<Type: ASTType> VariableAssignment<Type> {
    pub fn variable(&self) -> &Type::VariableUse {
        &self.variable
    }

    pub fn value(&self) -> &Expression<Type> {
        &self.value
    }
}

/** This represents a control structure as defined in chapters 8 and 13 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum ControlStructure<Type: ASTType> {
    Conditional(Conditional<Type>),
    Loop(Loop<Type>),
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
    pub(crate) fn child_statement_at(&self, index: usize) -> &Statement<Type> {
        match self {
            ControlStructure::Conditional(cond) => cond.child_statement_at(index),
            ControlStructure::Loop(inner) => inner.child_statement_at(index),
        }
    }
}

/** This represents a conditional as defined in chapter 8 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct Conditional<Type: ASTType> {
    condition: Expression<Type>,
    then_statement: Statement<Type>,
    else_statement: Option<Statement<Type>>,
}

impl<Type: ASTType> Conditional<Type> {
    pub fn new(
        condition: Expression<Type>,
        then_statement: Statement<Type>,
        else_statement: Option<Statement<Type>>,
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

    pub fn condition(&self) -> &Expression<Type> {
        &self.condition
    }

    pub fn then_statement(&self) -> &Statement<Type> {
        &self.then_statement
    }

    pub fn else_statement(&self) -> Option<&Statement<Type>> {
        self.else_statement.as_ref()
    }

    /** Returns the child statement at index
    0 is the then-statement
    1 is the else-statement
    */
    fn child_statement_at(&self, index: usize) -> &Statement<Type> {
        match index {
            0 => &self.then_statement,
            1 => self.else_statement.as_ref().unwrap(),
            _ => panic!("Index is out of bounds"),
        }
    }
}

/** This represents a conditional as defined in chapter 13 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct Loop<Type: ASTType> {
    to_loop_on: Statement<Type>,
    loop_type: LoopType<Type>,
}

impl<Type: ASTType> Loop<Type> {
    pub fn new(to_loop_on: Statement<Type>, loop_type: LoopType<Type>) -> Self {
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
    fn child_statement_at(&self, index: usize) -> &Statement<Type> {
        if index == self.loop_type.len() {
            &self.to_loop_on
        } else {
            self.loop_type.child_statement_at(index)
        }
    }
}

/** This is the type of a loop
*/
#[derive(Debug, PartialEq)]
pub enum LoopType<Type: ASTType> {
    Infinite,
    While(Expression<Type>),
    For {
        start: Statement<Type>,
        cond: Expression<Type>,
        after_each: Statement<Type>,
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
    fn child_statement_at(&self, index: usize) -> &Statement<Type> {
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

/** A return in wasome code
This is a wrapper around Expression with the wrapped one being the one's result that will be returned
*/
#[derive(Debug, PartialEq)]
pub struct Return<Type: ASTType> {
    to_return: Option<Expression<Type>>,
}

impl<Type: ASTType> Return<Type> {
    pub fn new(to_return: Option<Expression<Type>>) -> Self {
        Self { to_return }
    }

    pub fn to_return(&self) -> Option<&Expression<Type>> {
        self.to_return.as_ref()
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
    use crate::expression::Literal;
    #[test]
    fn variable_assignement() {
        basic_test_variable(Rc::new(VariableSymbol::new(
            "test".to_string(),
            DataType::F32,
        )))
        .unwrap();
    }

    fn basic_test_variable(
        symbol: Rc<VariableSymbol<TypedAST>>,
    ) -> Option<VariableDeclaration<TypedAST>> {
        VariableDeclaration::<TypedAST>::new(symbol, Expression::Literal(Literal::F64(14.0)))
    }
}
