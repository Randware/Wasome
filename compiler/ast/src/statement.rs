use std::ops::{Deref, Index};
use std::rc::Rc;
use crate::data_type::Type;
use crate::eq_return_option;
use crate::expression::Expression;
use crate::symbol::{Symbol, SymbolTable, VariableSymbol};

/** This represents a Statement as per section 4 of the lang spec
*/
pub enum Statement
{
    VariableAssignment(VariableAssignment),
    VariableDeclaration(VariableAssignment),
    Expression(Expression),
    ControlStructure //TODO
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
}

impl Index<usize> for Statement
{
    type Output = Statement;

    /** Gets the indexth child statement
    panics if self has no children or index is out of bounds
    */
    fn index(&self, index: usize) -> &Self::Output
    {
        todo!()
    }
}

/** This is a wrapper around Statement to make traversial easier
*/
pub struct StatementRef<'a>
{
    inner: &'a Statement,
    location: Option<StatementLocation<'a>>,
    root: &'a Statement //TODO: Change this to a proper root
}

impl<'a> StatementRef<'a>
{
    pub fn new_root(inner: &'a Statement) -> Self
    {
        Self {
            inner,
            location: None,
            root: inner
        }
    }

    fn new_child(inner: &'a Statement, parent: &'a StatementRef<'a>, location: usize) -> Self
    {
        Self {
            inner,
            location: Some(StatementLocation::new(location, parent.location.as_ref())),
            root: parent.root
        }
    }

    fn index_relative_to_root<'b>(&'b self, index: &StatementLocation) -> &'b Statement
    {
        let mut current = self.root;
        let starting_index_size = index.len();
        let mut current_index_size = starting_index_size;
        let mut current_index = index;
        while current_index_size > 0 {
            let path = current_index.index();
            current = &current[path];
            current_index_size -= 1;
            current_index = &index[starting_index_size - current_index_size];
        }
        current
    }

    /** Gets an iterator that iterates over all child statements
    Returns an empty iterator if there are no child expressions
    */
    pub fn child_iter(&self) -> impl Iterator<Item=StatementRef<'a>>
    {
        [].into_iter() //TODO: Add child statements
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
impl<'a> StatementRef<'a>
{
    pub fn symbols(&self) -> impl SymbolTable
    {
        DefaultSymbolTable::new(self)
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
        let new_current_location = self.current_location?.prev()?;
        self.prev_index = new_current_location.index();
        self.current_location = Some(new_current_location);
        self.current = self.source.index_relative_to_root(self.current_location?);
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
        let mut len = 1;
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

#[cfg(test)]
mod tests
{
    use crate::data_type::DataType;
    use crate::expression::Literal;
    use super::*;
    #[test]
    fn variable_assignement()
    {
        let assign = basic_test_variable(Rc::new(
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

        let statement_ref = StatementRef::new_root(&statement);
        assert_eq!(Vec::<Symbol>::new(), statement_ref.symbols().collect::<Vec<_>>());
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