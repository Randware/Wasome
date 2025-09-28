use std::cmp::PartialEq;
use std::ops::{Deref, Index};
use std::rc::Rc;
use crate::block::CodeBlock;
use crate::data_type::Type;
use crate::eq_return_option;
use crate::expression::Expression;
use crate::symbol::{Symbol, SymbolTable, VariableSymbol};
use crate::top_level::Function;

/** This represents a Statement as per section 4 of the lang spec
*/
#[derive(Debug)]
pub enum Statement
{
    VariableAssignment(VariableAssignment),
    VariableDeclaration(VariableAssignment),
    Expression(Expression),
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
        match self
        {
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

/** This is a wrapper around Statement to make traversial easier
*/
#[derive(Debug)]
pub struct StatementRef<'a>
{
    // The referenced statement
    inner: &'a Statement,
    // The location of this statement, relative to the root
    location: Option<StatementLocation<'a>>,
    // The root of the statement tree (should eventually become the function)
    root: &'a Function
}

impl<'a> StatementRef<'a>
{
    /** Creates a new StatementRef where inner is the root
    */
    pub fn new_root(inner: &'a Function) -> Self
    {
        Self {
            inner: inner.implementation(),
            location: None,
            root: inner
        }
    }

    /** Creates a new StatementRef that is the child of the specified statementRef at the specified index
    */
    fn new_child(inner: &'a Statement, parent: &'a StatementRef<'a>, location: usize) -> Self
    {
        Self {
            inner,
            location: Some(StatementLocation::new(location, parent.location.as_ref())),
            root: parent.root
        }
    }

    /** Indexes the root with index
    */
    fn index_relative_to_root<'b>(&'b self, index: &StatementLocation) -> &'b Statement
    {
        let mut current_statement = self.root.implementation();
        let starting_index_size = index.len();
        let mut current_index_size = starting_index_size;
        let mut current_index;
        while current_index_size > 0 {
            current_index = &index[starting_index_size - current_index_size];
            let path = current_index.index();
            current_statement = &current_statement[path];
            current_index_size -= 1;
        }
        current_statement
    }

    /** Indexes this with index
    */
    pub fn index(&'a self, index: usize) -> StatementRef<'a>
    {
        let indexed_statement = self.inner.index(index);
        Self::new_child(indexed_statement, self, index)
    }

    /** Creates a symbol table that iterates over all symbols defined at the current location
    */
    pub fn symbols(&self) -> impl SymbolTable
    {
        DefaultSymbolTable::new(self)
    }
}

impl<'a> Deref for StatementRef<'a>
{
    type Target = Statement;

    fn deref(&self) -> &Self::Target
    {
        self.inner
    }
}

// Helper struct for getting a symbols defined at a current location
struct DefaultSymbolTable<'a>
{
    source: &'a StatementRef<'a>,
    current: &'a Statement,
    current_location: Option<&'a StatementLocation<'a>>,
    prev_index: usize
}

impl<'a> DefaultSymbolTable<'a>
{
    pub fn new(source: &'a StatementRef) -> Self
    {
        Self {
            source,
            current: source.inner,
            current_location: source.location.as_ref(),
            prev_index: 0
        }
    }
}

impl<'a> Iterator for DefaultSymbolTable<'a>
{
    type Item = Symbol<'a>;

    fn next(&mut self) -> Option<Self::Item>
    {
        while self.prev_index > 0 {
            self.prev_index -= 1;
            if let Some(symbol) = self.current.index(self.prev_index).get_direct_symbol() {
                return Some(symbol);
            }
        }
        let new_current_location = self.current_location?.prev();
        self.prev_index = self.current_location?.index();
        self.current_location = new_current_location;
        if let Some(inner_current_location) = self.current_location
        {
            self.current = self.source.index_relative_to_root(inner_current_location);
        }
        else
        {
            self.current = self.source.root.implementation();
        }
        self.next()
    }
}

impl<'a> SymbolTable<'a> for DefaultSymbolTable<'a> {}

// Linked list representing the path from the root to the current statement
// The first step is at the end of the list
#[derive(Debug, Eq, PartialEq)]
struct StatementLocation<'a>
{
    index: usize,
    prev: Box<Option<&'a StatementLocation<'a>>>
}

impl<'a> StatementLocation<'a>
{
    pub fn new(index: usize, prev: Option<&'a StatementLocation<'a>>) -> Self
    {
        Self {
            index,
            prev: Box::new(prev)
        }
    }

    pub fn index(&self) -> usize
    {
        self.index
    }

    pub fn prev(&self) -> Option<&'a StatementLocation<'a>>
    {
        self.prev.as_ref().as_ref().map(|val| *val)
    }

    pub fn len(&self) -> usize
    {
        let mut len = 0;
        let mut current: Option<&StatementLocation> = Some(self);
        while current != None {
            // Unwrap safety:
            // current can't be none as it was checked by the loop condition
            current = current.unwrap().prev();
            len += 1;
        }
        len
    }
}

impl<'a> Index<usize> for StatementLocation<'a>
{
    type Output = StatementLocation<'a>;

    /** Indexes self with the specified index
    0 results in self
    len()-1 results in a StatementLocation that just indexes one level deeper than root
    */
    fn index(&self, mut index: usize) -> &Self::Output
    {
        let mut current = self;
        while index > 0 {
            // Expect safety:
            // If the index is out of bounds, we want to panic
            current = current.prev().expect("Index out of bounds");
            index -= 1;
        }
        current
    }
}

/** This represents an assignement to a variable
*/
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
pub struct Loop
{
    to_loop_on: Statement,
    loop_type: LoopType
}

impl Loop
{
    pub fn new(to_loop_on: Statement, loop_type: LoopType) -> Self
    {
        Self
        {
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
        else
        {
            &self.loop_type[index]
        }
    }
}

/** This is the type of a loop
*/
#[derive(Debug)]
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
        match self
        {
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
        if let LoopType::For {start, cond: _cond, after_each} = self
        {
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

#[cfg(test)]
mod tests
{
    use crate::data_type::DataType;
    use crate::expression::Literal;
    use crate::symbol::FunctionSymbol;
    use super::*;
    #[test]
    fn variable_assignement()
    {
        basic_test_variable(Rc::new(
            VariableSymbol::new("test".to_string(), DataType::F32)
        )).unwrap();
    }

    #[test]
    fn statement()
    {
        let symbol = Rc::new(
            VariableSymbol::new("test".to_string(), DataType::F32)
        );
        let statement = Statement::VariableDeclaration(
            basic_test_variable(symbol.clone()).unwrap()
        );


        assert_eq!(Some(Symbol::Variable(&symbol)), statement.get_direct_symbol());

        let function = Function::new(
            Rc::new(
                FunctionSymbol::new(
                    "test".to_string(),
                    None,
                    Vec::new()
                )
            ),
            statement
        );

        let statement_ref = StatementRef::new_root(&function);
        assert_eq!(Vec::<Symbol>::new(), statement_ref.symbols().collect::<Vec<_>>());
    }

    #[test]
    fn statement_2()
    {
        let symbol = Rc::new(
            VariableSymbol::new("test".to_string(), DataType::F32)
        );
        let statement = Statement::Codeblock(
            CodeBlock::new(
                vec![
                    Statement::VariableDeclaration(
                        VariableAssignment::new(
                            symbol.clone(),
                            Expression::Literal(
                                Literal::F32(
                                    10.0
                                )
                            )
                        ).unwrap()
                    ),
                    Statement::ControlStructure(
                        Box::new(
                            ControlStructure::Loop(
                                Loop::new(
                                    Statement::Expression(
                                        Expression::Literal(
                                            Literal::Bool(
                                                false
                                            )
                                        )
                                    ),
                                    LoopType::Infinite
                                )
                            )
                        )
                    )
                ]
            )
        );

        let function = Function::new(
            Rc::new(
                FunctionSymbol::new(
                    "test".to_string(),
                    None,
                    Vec::new()
                )
            ),
            statement
        );

        let root = StatementRef::new_root(&function);
        let loop_statement = root.index(1);
        let statement_ref = loop_statement.index(0);
        assert_eq!(vec![Symbol::Variable(&symbol)], statement_ref.symbols().collect::<Vec<_>>());
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