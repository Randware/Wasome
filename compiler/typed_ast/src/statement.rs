use std::cmp::PartialEq;
use std::ops::Index;
use std::rc::Rc;
use crate::block::CodeBlock;
use crate::data_type::{DataType, Type};
use crate::eq_return_option;
use crate::expression::Expression;
use crate::symbol::{Symbol, VariableSymbol};

/** This represents a Statement as per section 4 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum Statement
{
    // Assignment to existing variable
    VariableAssignment(VariableAssignment),
    // Creation of new variable
    VariableDeclaration(VariableAssignment),
    Expression(Expression),
    Return(Return),
    ControlStructure(Box<ControlStructure>),
    Codeblock(CodeBlock)
}

impl Statement
{
    /** Gets the symbol defined in this expression
    Only this is considered, while subexpressions are ignored
    @return
    Some(symbol) if symbol is defined here
    None if no symbols are defined here
    */
    pub fn get_direct_symbol(&self) -> Option<Symbol>
    {
        match self {
            Statement::VariableDeclaration(inner) => Some(Symbol::Variable(inner.variable())),
            _ => None
        }
    }

    /** Gets the length of the child statements
    */
    pub fn len_children(&self) -> usize
    {
        match self {
            Statement::ControlStructure(structure) => structure.child_len(),
            Statement::Codeblock(codeblock) => codeblock.len(),
            _ => 0
        }
    }
}

impl Index<usize> for Statement
{
    type Output = Statement;

    /** Gets the indexth child statement
    panics if self has no children or index is out of bounds
    */
    fn index(&self, index: usize) -> &Self::Output
    {
        match self {
            Statement::Codeblock(block) => &block[index],
            Statement::ControlStructure(structure) => &structure[index],
            _ => panic!("This has no child members!")
        }
    }
}

/** This represents an assignement to a variable. If this variable doesn't exist previously, it is created
*/
#[derive(Debug, PartialEq)]
pub struct VariableAssignment
{
    variable: Rc<VariableSymbol>,
    value: Expression
}

impl VariableAssignment
{
    /** Tries to create a new instance
    returns None if the type of the variable symbol and the return type of the expression doesn't
    match
    */
    pub fn new(variable: Rc<VariableSymbol>, value: Expression) -> Option<Self>
    {
        eq_return_option(variable.data_type(), value.data_type())?;
        Some(Self {
            variable,
            value
        })
    }

    pub fn variable(&self) -> &VariableSymbol
    {
        &self.variable
    }

    /** Gets the variable symbol by cloning the underlying RC
    */
    pub fn variable_owned(&self) -> Rc<VariableSymbol>
    {
        self.variable.clone()
    }

    pub fn value(&self) -> &Expression
    {
        &self.value
    }
}

/** This represents a control structure as defined in chapters 8 and 13 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum ControlStructure
{
    Conditional(Conditional),
    Loop(Loop)
}

impl ControlStructure
{
    /** Returns the number of child statements
    */
    pub fn child_len(&self) -> usize
    {
        match self {
            ControlStructure::Conditional(inner) => inner.len(),
            ControlStructure::Loop(inner) => inner.len()
        }
    }
}
impl Index<usize> for ControlStructure
{
    type Output = Statement;

    /** Returns the child statement at index
    */
    fn index(&self, index: usize) -> &Self::Output
    {
        match self {
            ControlStructure::Conditional(cond) => &cond[index],
            ControlStructure::Loop(inner) => &inner[index]
        }
    }
}

/** This represents a conditional as defined in chapter 8 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct Conditional
{
    condition: Expression,
    then_statement: Statement,
    else_statement: Option<Statement>
}

impl Conditional
{
    pub fn new(condition: Expression, then_statement: Statement, else_statement: Option<Statement>) -> Self
    {
        Self {
            condition,
            then_statement,
            else_statement
        }
    }

    /** Returns the number of child statements
    */
    pub fn len(&self) -> usize
    {
        1+self.else_statement.is_some() as usize
    }

    pub fn condition(&self) -> &Expression
    {
        &self.condition
    }

    pub fn then_statement(&self) -> &Statement
    {
        &self.then_statement
    }

    pub fn else_statement(&self) -> Option<&Statement>
    {
        self.else_statement.as_ref()
    }
}

impl Index<usize> for Conditional
{
    type Output = Statement;

    /** Returns the child statement at index
    */
    fn index(&self, index: usize) -> &Self::Output
    {
        match index {
            0 => &self.then_statement,
            1 => &self.else_statement.as_ref().unwrap(),
            _ => panic!("Index is out of bounds")
        }
    }
}

/** This represents a conditional as defined in chapter 13 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub struct Loop
{
    to_loop_on: Statement,
    loop_type: LoopType
}

impl Loop
{
    pub fn new(to_loop_on: Statement, loop_type: LoopType) -> Self
    {
        Self {
            to_loop_on,
            loop_type
        }
    }

    /** Returns the number of child statements
    */
    pub fn len(&self) -> usize
    {
        self.loop_type.len()+1
    }

    pub fn to_loop_on(&self) -> &Statement
    {
        &self.to_loop_on
    }

    pub fn loop_type(&self) -> &LoopType
    {
        &self.loop_type
    }
}

impl Index<usize> for Loop
{
    type Output = Statement;

    /** Returns the child statement at index
    */
    fn index(&self, index: usize) -> &Self::Output
    {
        if index == self.loop_type.len() {
            &self.to_loop_on
        }
        else {
            &self.loop_type[index]
        }
    }
}

/** This is the type of a loop
*/
#[derive(Debug, PartialEq)]
pub enum LoopType
{
    Infinite,
    While(Expression),
    For{
        start: Statement,
        cond: Expression,
        after_each: Statement
    }
}

impl LoopType
{
    /** Returns the number of child statements
    */
    pub fn len(&self) -> usize
    {
        match self {
            LoopType::Infinite => 0,
            LoopType::While(_) => 0,
            LoopType::For { .. } => 2
        }
    }
}

impl Index<usize> for LoopType
{
    type Output = Statement;

    /** Returns the child statement at index
    */
    fn index(&self, index: usize) -> &Self::Output
    {
        if let LoopType::For {start, cond: _cond, after_each} = self {
            match index {
                0 => start,
                1 => after_each,
                _ => panic!("Index is out of bounds")
            }
        }
        else {
            panic!("This loop type has no children!");
        }
    }
}

/** A return in wasome code
This is a wrapper around Expression with the wrapped one being the one's result that will be returned
*/
#[derive(Debug, PartialEq)]
pub struct Return
{
    to_return: Option<Expression>
}

impl Return
{
    pub fn new(to_return: Option<Expression>) -> Self
    {
        Self {
            to_return
        }
    }

    pub fn to_return(&self) -> Option<&Expression>
    {
        self.to_return.as_ref()
    }

    /** Gets the type being returned
    Returns none if nothing
    And Some(type) if an expression with type is being returned
    */
    pub fn return_type(&self) -> Option<DataType>
    {
        // Gets the type from the expression
        self.to_return().map(|val| val.data_type())
    }
}

#[cfg(test)]
mod tests
{
    use crate::data_type::DataType;
    use crate::expression::Literal;
    use super::*;
    #[test]
    fn variable_assignement()
    {
        basic_test_variable(Rc::new(
            VariableSymbol::new("test".to_string(), DataType::F32)
        )).unwrap();
    }

    fn basic_test_variable(symbol: Rc<VariableSymbol>) -> Option<VariableAssignment> {
        VariableAssignment::new(
            symbol,
            Expression::Literal(
                Literal::F32(
                    14.0
                )
            )
        )
    }
}