use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::AST;
use crate::statement::Statement;
use crate::symbol::{Symbol, SymbolTable, VariableSymbol};
use crate::top_level::Function;
use crate::traversal::statement_traversal::{StatementLocation, StatementTraversalHelper};

/** This struct helps with traversing the AST
It keeps a reference to the ast and a function.
This allows it to be used to keep track of all symbols available in a function
*/
#[derive(Debug)]
pub struct FunctionTraversalHelper<'a>
{
    // The referenced function
    inner: &'a Function,
    root: &'a AST
}

impl<'a> FunctionTraversalHelper<'a>
{
    pub fn new(inner: &'a Function, root: &'a AST) -> Self
    {
        Self {
            inner,
            root
        }
    }

    pub fn inner(&self) -> &'a Function
    {
        self.inner
    }

    pub fn root(&self) -> &'a AST
    {
        self.root
    }

    /** Gets a symboltable that has all symbols defined by this (parameters) and symbols from outside this function
    */
    pub fn symbols(&self) -> impl SymbolTable
    {
        FunctionSymbolTable::new(self)
    }

    /** Indexes the implementation with index
    */
    pub(crate) fn index_implementation<'b>(&'b self, index: &StatementLocation) -> &'b Statement
    {
        let mut current_statement = self.implementation();
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

    /** Gets a StatementRef for the top level statement in this function
          This is the intended way to traverse a function
    */
    pub fn ref_to_implementation(&self) -> StatementTraversalHelper
    {
        StatementTraversalHelper::new_root(self)
    }
}

impl<'a> Deref for FunctionTraversalHelper<'a>
{
    type Target = Function;

    fn deref(&self) -> &'a Self::Target
    {
        self.inner
    }
}

struct FunctionSymbolTable<'a>
{
    parameters: &'a [Rc<VariableSymbol>],
    parameter_index: usize,
    functions: Box<dyn Iterator<Item=&'a Function>+'a>
}

impl<'a> Debug for FunctionSymbolTable<'a>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result
    {
        f.debug_struct("FunctionSymbolTable")
            .field("parameters", &self.parameters)
            .field("parameter_index", &self.parameter_index)
            // Unable to debug functions, it does not implement debug
            .finish()
    }
}

impl<'a> FunctionSymbolTable<'a>
{
    fn new(source: & FunctionTraversalHelper<'a>) -> Self
    {
        Self {
            parameters: source.inner().declaration().params(),
            parameter_index: 0,
            functions: Box::new(source.root.functions())
        }
    }
}

impl<'a> Iterator for FunctionSymbolTable<'a>
{
    type Item = Symbol<'a>;

    fn next(&mut self) -> Option<Self::Item>
    {
        next_item_from_slice(self.parameters, &mut self.parameter_index)
            .map(|val| Symbol::Variable(&val))
            .or_else(||
                self.functions.next()
                    .map(|val| Symbol::Function(&val.declaration()))
            )

    }
}

fn next_item_from_slice<'a, T>(slice: &'a [T], index: &mut usize) -> Option<&'a T>
{
    let item = slice
        .get(*index);
    // Prevent overflows of index
    if item.is_some() {
        *index += 1;
    }
    item
}

impl<'a> SymbolTable<'a> for FunctionSymbolTable<'a> {}