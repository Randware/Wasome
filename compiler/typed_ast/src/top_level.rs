use std::ops::Deref;
use std::rc::Rc;
use crate::AST;
use crate::statement::{Statement, StatementRef};
use crate::symbol::{FunctionSymbol, Symbol, SymbolTable, VariableSymbol};

#[derive(Debug)]
pub struct Function
{
    declaration: Rc<FunctionSymbol>,
    implementation: Statement
}

impl Function
{
    pub fn new(declaration: Rc<FunctionSymbol>, implementation: Statement) -> Self
    {
        Self {
            declaration,
            implementation
        }
    }

    pub fn declaration(&self) -> &FunctionSymbol
    {
        &self.declaration
    }

    /** Gets the declaration by cloning the rc
    */
    pub fn declaration_owned(&self) -> Rc<FunctionSymbol>
    {
        self.declaration.clone()
    }

    pub fn implementation(&self) -> &Statement
    {
        &self.implementation
    }
}

#[derive(Debug)]
pub struct FunctionRef<'a>
{
    // The referenced function
    inner: &'a Function,
    root: &'a AST
}

impl<'a> FunctionRef<'a>
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
    pub(crate) fn index_implementation<'b>(&'b self, index: &crate::statement::StatementLocation) -> &'b Statement
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
    pub fn ref_to_implementation(&self) -> StatementRef
    {
        StatementRef::new_root(self)
    }
}

impl<'a> Deref for FunctionRef<'a>
{
    type Target = Function;

    fn deref(&self) -> &'a Self::Target
    {
        self.inner
    }
}

#[derive(Debug)]
struct FunctionSymbolTable<'a>
{
    parameters: &'a [VariableSymbol],
    parameter_index: usize,
    functions: &'a [Function],
    function_index: usize
}

impl<'a> FunctionSymbolTable<'a>
{
    fn new(source: & FunctionRef<'a>) -> Self
    {
        Self {
            parameters: source.inner().declaration().params(),
            parameter_index: 0,
            functions: source.root.functions(),
            function_index: 0
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
                next_item_from_slice(self.functions, &mut self.function_index)
                    .map(|val| Symbol::Function(&val.declaration))
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

