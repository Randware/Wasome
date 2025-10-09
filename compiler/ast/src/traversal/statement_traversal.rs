use std::ops::{Deref, Index};
use crate::ASTType;
use crate::statement::Statement;
use crate::symbol::{Symbol, SymbolTable};
use crate::traversal::function_traversal::FunctionTraversalHelper;

/** This struct helps with traversing statements
It keeps a reference to the root (function) and a statement.
This allows it to be used to keep track of all symbols available to a statement
It is supposed to be created by either a FunctionTraversalHelper or the StatementTraversalHelper of the parent statement
*/
#[derive(Debug)]
pub struct StatementTraversalHelper<'a, Type: ASTType>
{
    // The referenced statement
    inner: &'a Statement<Type>,
    // The location of this statement, relative to the root
    location: Option<StatementLocation<'a>>,
    // The root of the statement tree (should eventually become the function)
    root: &'a FunctionTraversalHelper<'a, Type>
}

impl<'a, Type: ASTType> StatementTraversalHelper<'a, Type>
{
    /** Creates a new StatementRef where inner is the root
    */
    pub fn new_root(inner: &'a FunctionTraversalHelper<'a, Type>) -> Self
    {
        Self {
            inner: inner.implementation(),
            location: None,
            root: inner
        }
    }

    /** Creates a new StatementRef that is the child of the specified statementRef at the specified index
    */
    fn new_child(inner: &'a Statement<Type>, parent: &'a StatementTraversalHelper<'a, Type>, location: usize) -> Self
    {
        Self {
            inner,
            location: Some(StatementLocation::new(location, parent.location.as_ref())),
            root: parent.root
        }
    }

    /** Indexes this with index
    */
    pub fn index(&'a self, index: usize) -> StatementTraversalHelper<'a, Type>
    {
        let indexed_statement = self.inner.index(index);
        Self::new_child(indexed_statement, self, index)
    }

    /** Creates a symbol table that iterates over all symbols available to the current statement
       self may not be the root statement in its function
       @return:
       Some(table) The requested table
       None if self is the root statement
    */
    pub fn symbols_available_at(&self) -> Option<impl SymbolTable<Type>>
    {
        DefaultSymbolTable::new_available_to_statement(self)
    }

    /** Creates a symbol table that iterates over all symbols available after the current statement
       self may not be the root statement in its function
       @return:
       Some(table) The requested table
       None if self is the root statement
    */
    pub fn symbols_available_after(&self) -> Option<impl SymbolTable<Type>>
    {
        DefaultSymbolTable::new_available_after_statement(self)
    }

    /** Gets a vec of all symbols declared in a direct child of self
    Symbols declared by self directly are not included
    */
    pub fn symbols_defined_directly_in(&self) -> Vec<Symbol<'a, Type>> // Returning an iterator would require a trait object
    {
        let statement_to_symbol  = Statement::get_direct_symbol;
        match self.inner
        {
            Statement::ControlStructure(control) =>
                Self::indexable_into_vec(|index| control.child_statement_at(index), control.child_len(), statement_to_symbol),
            Statement::Codeblock(codeblock) =>
                Self::indexable_into_vec(|index| &codeblock[index], codeblock.len(), statement_to_symbol),
            _ => Vec::new()
        }
    }

    // I was unable to find a better solution
    /** This takes an indexable to_convert, reads the first len elements from it, converts them with
    map_with and returns the inner values of the somes in a vec
    */
    fn indexable_into_vec<'b, T, U, F>(to_convert: impl Fn(usize) -> &'b T + 'b,
                                 len: usize,
                                 mut map_with: F) -> Vec<U>
    where
        F: FnMut(&'b T) -> Option<U>,
        T: 'b
    {
        let mut result = Vec::with_capacity(len);
        for index in 0..len
        {
            if let Some(converted) = map_with(to_convert(index))
            {
                result.push(converted)
            }
        }
        result
    }
}

impl<Type: ASTType> Deref for StatementTraversalHelper<'_, Type>
{
    type Target = Statement<Type>;

    fn deref(&self) -> &Self::Target
    {
        self.inner
    }
}

// Helper struct for getting a symbols defined at a current location
struct DefaultSymbolTable<'a, Type: ASTType>
{
    source: &'a StatementTraversalHelper<'a, Type>,
    current: &'a Statement<Type>,
    current_location: Option<&'a StatementLocation<'a>>,
    prev_index: usize,
    // The symbols from the root, for example functions
    root_symbols: Box<dyn SymbolTable<'a, Type> + 'a>
}

impl<'a, Type: ASTType> DefaultSymbolTable<'a, Type>
{
    pub(crate) fn new_available_to_statement(source: &'a StatementTraversalHelper<Type>) -> Option<Self>
    {
        if source.root.implementation() == source.inner {
            return None
        }
        Some(Self {
            source,
            current: source.inner,
            current_location: source.location.as_ref(),
            prev_index: 0,
            root_symbols: Box::new(source.root.symbols())
        })
    }

    pub(crate) fn new_available_after_statement(source: &'a StatementTraversalHelper<Type>) -> Option<Self>
    {
        let mut to_ret = Self::new_available_to_statement(source)?;
        to_ret.go_up_statement_tree()?;
        to_ret.prev_index += 1;
        Some(to_ret)
    }
}

impl<'a, Type: ASTType> DefaultSymbolTable<'a, Type>
{
    fn next_variable_symbol(&mut self) -> Option<Symbol<'a, Type>>
    {
        while self.prev_index > 0 {
            self.prev_index -= 1;
            if let Some(symbol) = self.current.index(self.prev_index).get_direct_symbol() {
                return Some(symbol);
            }
        }
        self.go_up_statement_tree()?;
        self.next()
    }

    fn go_up_statement_tree(&mut self) -> Option<()> {
        let new_current_location = self.current_location?.prev();
        self.prev_index = self.current_location?.index();
        self.current_location = new_current_location;
        if let Some(inner_current_location) = self.current_location {
            self.current = self.source.root.index_implementation(inner_current_location);
        } else {
            self.current = self.source.root.implementation();
        }
        Some(())
    }
}

impl<'a, Type: ASTType> Iterator for DefaultSymbolTable<'a, Type>
{
    type Item = Symbol<'a, Type>;

    fn next(&mut self) -> Option<Self::Item>
    {
        self.next_variable_symbol()
            .or_else(|| self.root_symbols.next())
    }
}

impl<'a, Type: ASTType> SymbolTable<'a, Type> for DefaultSymbolTable<'a, Type> {}

// Linked list representing the path from the root to the current statement
// The first step is at the end of the list
#[derive(Debug, Eq, PartialEq)]
pub(crate) struct StatementLocation<'a>
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
        while current.is_some() {
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