use crate::statement::Statement;
use crate::symbol::{DirectlyAvailableSymbol, ModuleUsageNameSymbol, SymbolTable, VariableSymbol};
use crate::traversal::HasSymbols;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::{ASTNode, ASTType};
use std::collections::HashSet;
use std::ops::Index;
use std::vec::IntoIter;

/// This struct helps with traversing statements
///
/// It keeps a reference to the root (function) and a statement.
/// This allows it to be used to keep track of all symbols available to a statement
/// It is supposed to be created by either a FunctionTraversalHelper or the StatementTraversalHelper of the parent statement,
///
/// # Lifetimes
///
/// | Lifetime     | Purpose      |
/// | ------------- | ------------- |
/// | 'a | How long the traversal helper may life |
/// | 'b | How long the underlying data may life |
#[derive(Debug)]
pub struct StatementTraversalHelper<'a, 'b, Type: ASTType> {
    /// The referenced statement
    inner: &'b ASTNode<Statement<Type>>,
    /// The location of this statement, relative to the root
    location: StatementLocation<'a, 'b, Type>,
    /// The root of the statement tree (should eventually become the function)
    root: &'a FunctionTraversalHelper<'a, 'b, Type>,
}

impl<'a, 'b, Type: ASTType> StatementTraversalHelper<'a, 'b, Type> {
    /// Creates a new StatementRef where inner is the root
    pub fn new_root(root: &'a FunctionTraversalHelper<'a, 'b, Type>) -> Self {
        Self {
            inner: root.inner().implementation(),
            location: StatementLocation::new_root(root.inner().implementation()),
            root,
        }
    }

    pub fn inner(&self) -> &'b ASTNode<Statement<Type>> {
        self.inner
    }

    pub fn location(&self) -> &StatementLocation<'a, 'b, Type> {
        &self.location
    }

    /// Creates a new StatementRef that is the child of the specified statementRef at the specified index
    ///
    /// # Parameter
    ///
    /// - location:
    ///     - The location of the child.
    ///
    /// # Return
    ///
    /// - `None` if `location >= self.amount_children()`
    /// - `Some(<The StatementTraversalHelper of the requested child>)` otherwise
    pub fn get_child(&'a self, location: usize) -> Option<StatementTraversalHelper<'a, 'b, Type>> {
        if location >= self.amount_children() {
            return None;
        }
        let child = self.inner.index(location);
        Some(Self {
            inner: child,
            location: StatementLocation::new_node(location, &self.location, child),
            root: self.root,
        })
    }

    /// Creates a symbol table that iterates over all symbols available to the current statement
    ///
    /// This excludes the symbol defined by the current statement, if it exists
    ///
    /// # Returns
    /// The requested table
    pub fn symbols_available_at<'c>(&'c self) -> impl SymbolTable<'b, Type> + 'c {
        StatementSymbolTable::new_available_to_statement(self)
    }

    /// Creates a symbol table that iterates over all symbols available after the current statement
    ///
    /// This excludes the symbol defined by the current statement, if it exists
    /// self may not be the root statement in its function
    ///
    /// # Returns
    ///
    /// - Some(table) The requested table
    /// - None if self is the root statement
    pub fn symbols_available_after(&self) -> Option<impl SymbolTable<'b, Type>> {
        StatementSymbolTable::new_available_after_statement(self)
    }

    /// Gets a vec of all symbols declared in a direct child of self where the index
    /// of the child is less than index.
    /// Symbols declared by self directly are not included.
    ///
    /// A vec is returned as an iterator would require a trait object
    ///
    /// # Return
    ///
    /// - `None` if `location > self.amount_children()`
    /// - `Some(<The requested symbols>)` otherwise
    pub fn symbols_defined_directly_in_before_index(
        &self,
        index: usize,
    ) -> Option<Vec<DirectlyAvailableSymbol<'b, Type>>> {
        if index > self.amount_children() {
            return None;
        }
        Some(match &**self.inner {
            Statement::ControlStructure(control) => Self::indexable_into_vec(
                |index| control.child_statement_at(index),
                index,
                Self::symbol_mapper,
            ),
            Statement::Codeblock(codeblock) => {
                Self::indexable_into_vec(|index| &codeblock[index], index, Self::symbol_mapper)
            }
            _ => Vec::new(),
        })
    }

    /// Extracts symbols from a statement
    fn symbol_mapper(
        input: &Statement<Type>,
    ) -> impl Iterator<Item = DirectlyAvailableSymbol<'_, Type>> {
        input.get_direct_symbols().into_iter()
    }
    /// Gets a vec of all symbols declared in a direct child of self
    /// Symbols declared by self directly are not included
    ///
    /// A vec is returned as an iterator would require a trait object
    ///
    /// # Return
    ///
    /// The requested symbols
    pub fn symbols_defined_directly_in(&self) -> Vec<DirectlyAvailableSymbol<'b, Type>> {
        // self.amount_children() can never be > self.amount_children()
        // So unwrapping can never panic
        self.symbols_defined_directly_in_before_index(self.amount_children())
            .unwrap()
    }

    /// Gets the amount of direct child statements
    ///
    /// Children of children are not considered
    ///
    /// # Return
    ///
    /// The amount of children
    pub fn amount_children(&self) -> usize {
        self.inner.amount_children()
    }

    /// This takes an indexable to_convert, reads the first len elements from it, converts them with
    /// map_with and returns the inner values of the slices in a vec
    fn indexable_into_vec<'c, T, U, F, I>(
        to_convert: impl Fn(usize) -> &'c T + 'c,
        len: usize,
        mut map_with: F,
    ) -> Vec<U>
    where
        F: FnMut(&'c T) -> I,
        I: Iterator<Item = U>,
        T: 'c,
        U: 'c,
    {
        let mut result = Vec::with_capacity(len);
        for index in 0..len {
            map_with(to_convert(index)).for_each(|elem| result.push(elem));
        }
        result
    }
}

impl<'a, 'b, Type: ASTType> HasSymbols<'b, Type> for StatementTraversalHelper<'a, 'b, Type> {
    fn symbols<'c>(&'c self) -> impl SymbolTable<'b, Type> + 'c {
        StatementSymbolTable::new_available_to_statement(self)
    }

    fn symbols_trait_object(&self) -> Box<dyn SymbolTable<'b, Type> + '_> {
        Box::new(self.symbols())
    }
}

/// Helper struct for getting a symbols defined at a current location
///
/// The intended usage is via the `.next()` method from the iterator trait.
struct StatementSymbolTable<'a, 'b, Type: ASTType> {
    /// The symbol which child symbols we are currently iterating over
    current: &'b Statement<Type>,
    /// The location of this symbol
    current_location: &'a StatementLocation<'a, 'b, Type>,
    prev_index: usize,
    /// The symbols from the root, for example functions
    root_symbols: Box<dyn SymbolTable<'b, Type> + 'a>,
    /// Deduplicates variables for variable shadowing
    found_variables: HashSet<&'b str>,
    current_statement_symbols: IntoIter<&'b VariableSymbol<Type>>,
}

impl<'a, 'b, Type: ASTType> StatementSymbolTable<'a, 'b, Type> {
    /// Creates a symbol table that iterates over all symbols available to the current statement
    ///
    /// This excludes the symbol defined by the current statement, if it exists
    ///
    /// # Returns
    /// The requested table
    pub(crate) fn new_available_to_statement(
        source: &'a StatementTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        let mut to_ret = Self {
            current: source.inner,
            current_location: source.location(),
            // Setting this to zero will make the next call to
            // next_variable_symbol() go up the statement tree for us
            prev_index: 0,
            root_symbols: Box::new(source.root.symbols()),
            found_variables: HashSet::new(),
            current_statement_symbols: Vec::new().into_iter(),
        };
        if to_ret.go_up_statement_tree().is_some() {
            // We don't want to include the statement we started are starting with
            to_ret.prev_index -= 1;
        }
        to_ret
    }

    /// Creates a symbol table that iterates over all symbols available after the current statement
    ///
    /// This excludes the symbol defined by the current statement, if it exists
    /// self may not be the root statement in its function
    ///
    /// # Returns
    ///
    /// - Some(table) The requested table
    /// - None if self is the root statement
    pub(crate) fn new_available_after_statement(
        source: &'a StatementTraversalHelper<'a, 'b, Type>,
    ) -> Option<Self> {
        let mut to_ret = Self::new_available_to_statement(source);
        to_ret.prev_index += 1;
        Some(to_ret)
    }
}

impl<'a, 'b, Type: ASTType> StatementSymbolTable<'a, 'b, Type> {
    /// Gets the next variable symbol
    ///
    /// The underlying state is changed so the next call
    /// returns the variable symbol after the next one
    ///
    /// # Return
    ///
    /// - None
    ///     - If there are no more variable symbols available
    /// - Some(The variable symbol)
    ///     - If there are still variable symbols available
    fn next_variable_symbol(&mut self) -> Option<DirectlyAvailableSymbol<'b, Type>> {
        if let Some(symbol) = self.current_statement_symbols.next() {
            // Insert the symbol into the set
            // Only return it if its new
            if self.found_variables.insert(symbol.name()) {
                // Symbol found, return it
                return Some(DirectlyAvailableSymbol::Variable(symbol));
            }
        }
        // Attempts to get a child symbol from the current symbol
        // If all child statements were traversed, this loop ends
        if self.prev_index > 0 {
            self.prev_index -= 1;
            self.current_statement_symbols = self
                .current
                .index(self.prev_index)
                .get_direct_variable_symbols()
                .into_iter();
            return self.next_variable_symbol();
        }
        // Attempts to go up the statement tree to get symbols from the layer above the current one
        // It fails if there are no more layers
        // In this case, we return None
        self.go_up_statement_tree()?;
        // Recurse after successfully going up a statement layer
        self.next_variable_symbol()
    }

    /// Goes up the statement tree
    ///
    /// Changes three values:
    /// 1. `current_location`
    ///     - Set to the above location
    ///     - The method fails if if `current_location` is None
    ///         - The above location may be None however
    /// 2. prev_index
    ///     - Set to the index of the current `current_statement`
    /// 3. current_statement
    ///     - Set to its parent
    ///
    /// # Return
    ///
    /// - None
    ///     - If going up failed due to us already being at the highest level
    /// - Some(())
    ///     - If going up was successful
    ///     - Note that this doesn't carry any data
    fn go_up_statement_tree(&mut self) -> Option<()> {
        let new_current_location = self.current_location.parent_statement()?;
        // When the container statement is a match, it can itself contain symbols
        self.prev_index = self.current_location.index()? + 1;
        self.current_location = new_current_location;
        self.current = self.current_location.referenced_statement();
        Some(())
    }
}

impl<'a, 'b, Type: ASTType> Iterator for StatementSymbolTable<'a, 'b, Type> {
    /// A tuple of prefix and symbol as required by  [`SymbolTable`]
    type Item = (
        Option<&'b ModuleUsageNameSymbol>,
        DirectlyAvailableSymbol<'b, Type>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        self.next_variable_symbol()
            // A var symbol never requires a prefix
            .map(|var_symbol| (None, var_symbol))
            .or_else(|| self.root_symbols.next())
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for StatementSymbolTable<'a, 'b, Type> {}

/// Linked list representing the path from the current statement to the root
///
/// For each node, the following is stored:
/// - Position, consisting of:
///     - Previous Statement Location
///     - Index of the previous statement to get to the current
/// - Current statement
///
/// The root node has no position.
/// The first step is at the beginning of the list

#[derive(Debug, PartialEq)]
pub struct StatementLocation<'a, 'b, Type: ASTType> {
    /// None means that this references to a root statement that has no parent
    ///
    /// The first part of the tuple is the statement index and the second is the previous part of the list
    position: Option<(usize, &'a StatementLocation<'a, 'b, Type>)>,
    referenced_statement: &'b Statement<Type>,
}

impl<'a, 'b, Type: ASTType> StatementLocation<'a, 'b, Type> {
    /// Creates a new `StatementLocation` where the provided statement is the root
    ///
    /// # Parameter
    ///
    /// - referenced_statement
    ///     - The root statement
    ///
    /// # Return
    ///
    /// A `StatementLocation` referencing the root statement
    pub fn new_root(referenced_statement: &'b Statement<Type>) -> Self {
        Self {
            position: None,
            referenced_statement,
        }
    }

    /// Creates a new `StatementLocation` which is a child node if the provided location
    ///
    /// # Parameters
    ///
    /// - index
    ///     - The index that is required to take from the referenced statement of `prev` to get to
    ///       `statement_referenced`
    /// - parent_statement
    ///     - The parent statement
    /// - referenced_statement
    ///     - The statement
    ///
    /// # Return
    ///
    /// A `StatementLocation` referencing the provided statement including the path from the parent
    /// statement and therefore also from the root.
    pub fn new_node(
        index: usize,
        parent_statement: &'a StatementLocation<'a, 'b, Type>,
        referenced_statement: &'b Statement<Type>,
    ) -> Self {
        Self {
            position: Some((index, parent_statement)),
            referenced_statement,
        }
    }

    pub fn index(&self) -> Option<usize> {
        self.position.map(|pos| pos.0)
    }

    pub fn parent_statement(&self) -> Option<&'a StatementLocation<'a, 'b, Type>> {
        self.position.map(|pos| pos.1)
    }

    pub fn referenced_statement(&self) -> &'b Statement<Type> {
        self.referenced_statement
    }

    /// Calculates the length of this.
    ///
    /// The length is the number of StatementLocation nodes
    /// one can get by following the prev fields plus one for the starting node.
    ///
    /// # Returns
    /// - The calculated length
    // An is_empty method would be redundant as len == 0 is not possible
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        let mut len = 1;
        let mut current: Option<&StatementLocation<Type>> = self.parent_statement();
        while current.is_some() {
            // Unwrap safety:
            // current can't be none as it was checked by the loop condition
            current = current.unwrap().parent_statement();
            len += 1;
        }
        len
    }

    /// Indexes self with the specified index
    /// 0 results in self
    /// len()-1 results in a StatementLocation that just indexes one level deeper than root
    ///
    /// # Return
    ///
    /// - `None` if `self.len() <= index`
    /// - `Some(<StatementLocation>)` otherwise
    pub fn get(&self, index: usize) -> Option<&StatementLocation<'a, 'b, Type>> {
        if index >= self.len() {
            return None;
        }
        // Panic safety:
        // This only panics if self.len() <= index
        // This can't be the case as we checked that
        Some(&self[index])
    }
}

impl<'a, 'b, Type: ASTType> Index<usize> for StatementLocation<'a, 'b, Type> {
    type Output = StatementLocation<'a, 'b, Type>;

    /// Indexes self with the specified index
    /// 0 results in self
    /// len()-1 results in a StatementLocation that just indexes one level deeper than root
    ///
    /// # Panics
    ///
    /// If `self.len() <= index`
    fn index(&self, mut index: usize) -> &Self::Output {
        let mut current = self;
        while index > 0 {
            // Expect safety:
            // If the index is out of bounds, we want to panic
            current = current.parent_statement().expect("Index out of bounds");
            index -= 1;
        }
        current
    }
}
