use crate::statement::Statement;
use crate::symbol::{Symbol, SymbolTable};
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{ASTNode, ASTType};
use std::ops::Index;

/** This struct helps with traversing statements
It keeps a reference to the root (function) and a statement.
This allows it to be used to keep track of all symbols available to a statement
It is supposed to be created by either a FunctionTraversalHelper or the StatementTraversalHelper of the parent statement,

### Lifetimes

| Lifetime     | Purpose      |
| ------------- | ------------- |
| 'a | How long the traversal helper may life |
| 'b | How long the underlying data may life |
*/
#[derive(Debug)]
pub struct StatementTraversalHelper<'a, 'b, Type: ASTType> {
    // The referenced statement
    inner: &'b ASTNode<Statement<Type>>,
    // The location of this statement, relative to the root
    location: Option<StatementLocation<'a>>,
    // The root of the statement tree (should eventually become the function)
    root: &'a FunctionTraversalHelper<'a, 'b, Type>,
}

impl<'a, 'b, Type: ASTType> StatementTraversalHelper<'a, 'b, Type> {
    /** Creates a new StatementRef where inner is the root
     */
    pub fn new_root(root: &'a FunctionTraversalHelper<'a, 'b, Type>) -> Self {
        Self {
            inner: root.inner().implementation(),
            location: None,
            root,
        }
    }

    pub fn inner(&self) -> &'b ASTNode<Statement<Type>> {
        self.inner
    }

    /** Creates a new StatementRef that is the child of the specified statementRef at the specified index
     */
    fn new_child(
        inner: &'b ASTNode<Statement<Type>>,
        parent: &'a StatementTraversalHelper<'a, 'b, Type>,
        location: usize,
    ) -> Self {
        Self {
            inner,
            location: Some(StatementLocation::new(location, parent.location.as_ref())),
            root: parent.root,
        }
    }

    /** Indexes this with index
     */
    pub fn index(&'a self, index: usize) -> StatementTraversalHelper<'a, 'b, Type> {
        let indexed_statement = self.inner.index(index);
        Self::new_child(indexed_statement, self, index)
    }

    /** Creates a symbol table that iterates over all symbols available to the current statement
       self may not be the root statement in its function
       @return:
       Some(table) The requested table
       None if self is the root statement
    */
    pub fn symbols_available_at<'c>(&'c self) -> Option<impl SymbolTable<'b, Type> + 'c> {
        StatementSymbolTable::new_available_to_statement(self)
    }

    /** Creates a symbol table that iterates over all symbols available after the current statement
       self may not be the root statement in its function
       @return:
       Some(table) The requested table
       None if self is the root statement
    */
    pub fn symbols_available_after(&self) -> Option<impl SymbolTable<'b, Type>> {
        StatementSymbolTable::new_available_after_statement(self)
    }

    /** Gets a vec of all symbols declared in a direct child of self where the index
    of the child is less than index. <br>
    Symbols declared by self directly are not included. <br>
    Panics if `index > self.len_children()`
    */
    pub fn symbols_defined_directly_in_before_index(&self, index: usize) -> Vec<Symbol<'b, Type>> {
        let statement_to_symbol = Statement::get_direct_symbol;
        match &**self.inner {
            Statement::ControlStructure(control) => Self::indexable_into_vec(
                |index| control.child_statement_at(index),
                index,
                statement_to_symbol,
            ),
            Statement::Codeblock(codeblock) => {
                Self::indexable_into_vec(|index| &codeblock[index], index, statement_to_symbol)
            }
            _ => Vec::new(),
        }
    }
    /** Gets a vec of all symbols declared in a direct child of self
    Symbols declared by self directly are not included
    */
    pub fn symbols_defined_directly_in(&self) -> Vec<Symbol<'b, Type>> // Returning an iterator would require a trait object
    {
        self.symbols_defined_directly_in_before_index(self.len_children())
    }

    pub fn len_children(&self) -> usize {
        self.inner.len_children()
    }

    // I was unable to find a better solution
    /** This takes an indexable to_convert, reads the first len elements from it, converts them with
    map_with and returns the inner values of the somes in a vec
    */
    fn indexable_into_vec<'c, T, U, F>(
        to_convert: impl Fn(usize) -> &'c T + 'c,
        len: usize,
        mut map_with: F,
    ) -> Vec<U>
    where
        F: FnMut(&'c T) -> Option<U>,
        T: 'c,
    {
        let mut result = Vec::with_capacity(len);
        for index in 0..len {
            if let Some(converted) = map_with(to_convert(index)) {
                result.push(converted)
            }
        }
        result
    }
}

// Helper struct for getting a symbols defined at a current location
struct StatementSymbolTable<'a, 'b, Type: ASTType> {
    source: &'a StatementTraversalHelper<'a, 'b, Type>,
    current: &'b Statement<Type>,
    current_location: Option<&'a StatementLocation<'a>>,
    prev_index: usize,
    // The symbols from the root, for example functions
    root_symbols: Box<dyn SymbolTable<'b, Type> + 'a>,
}

impl<'a, 'b, Type: ASTType> StatementSymbolTable<'a, 'b, Type> {
    pub(crate) fn new_available_to_statement(
        source: &'a StatementTraversalHelper<'a, 'b, Type>,
    ) -> Option<Self> {
        if source.root.inner().implementation() == source.inner() {
            return None;
        }
        Some(Self {
            source,
            current: source.inner,
            current_location: source.location.as_ref(),
            prev_index: 0,
            root_symbols: Box::new(source.root.symbols()),
        })
    }

    pub(crate) fn new_available_after_statement(
        source: &'a StatementTraversalHelper<'a, 'b, Type>,
    ) -> Option<Self> {
        let mut to_ret = Self::new_available_to_statement(source)?;
        to_ret.go_up_statement_tree()?;
        to_ret.prev_index += 1;
        Some(to_ret)
    }
}

impl<'a, 'b, Type: ASTType> StatementSymbolTable<'a, 'b, Type> {
    fn next_variable_symbol(&mut self) -> Option<Symbol<'b, Type>> {
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
            self.current = self
                .source
                .root
                .index_implementation(inner_current_location);
        } else {
            self.current = self.source.root.inner().implementation();
        }
        Some(())
    }
}

impl<'a, 'b, Type: ASTType> Iterator for StatementSymbolTable<'a, 'b, Type> {
    type Item = Symbol<'b, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_variable_symbol()
            .or_else(|| self.root_symbols.next())
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for StatementSymbolTable<'a, 'b, Type> {}

// Linked list representing the path from the root to the current statement
// The first step is at the end of the list
#[derive(Debug, Eq, PartialEq)]
pub(crate) struct StatementLocation<'a> {
    index: usize,
    prev: Box<Option<&'a StatementLocation<'a>>>,
}

impl<'a> StatementLocation<'a> {
    pub fn new(index: usize, prev: Option<&'a StatementLocation<'a>>) -> Self {
        Self {
            index,
            prev: Box::new(prev),
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn prev(&self) -> Option<&'a StatementLocation<'a>> {
        self.prev.as_ref().as_ref().map(|val| *val)
    }

    pub fn len(&self) -> usize {
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

impl<'a> Index<usize> for StatementLocation<'a> {
    type Output = StatementLocation<'a>;

    /** Indexes self with the specified index
       0 results in self
       len()-1 results in a StatementLocation that just indexes one level deeper than root
    */
    fn index(&self, mut index: usize) -> &Self::Output {
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
