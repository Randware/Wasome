use std::ops::Deref;
use std::rc::Rc;
use crate::AST;
use crate::statement::{Statement, StatementRef};
use crate::symbol::{FunctionSymbol, Symbol, SymbolTable};

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
        Self
        {
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

    fn deref(&self) -> &Self::Target
    {
        self.inner
    }
}

#[derive(Debug)]
struct FunctionSymbolTable<'a>
{
    functions: &'a [Function],
    index: usize
}

impl<'a> FunctionSymbolTable<'a>
{
    fn new(source: &FunctionRef<'a>) -> Self
    {
        Self {
            functions: source.root.functions(),
            index: 0
        }
    }
}

impl<'a> Iterator for FunctionSymbolTable<'a>
{
    type Item = Symbol<'a>;

    fn next(&mut self) -> Option<Self::Item>
    {
        let item = self.functions
            .get(self.index)
            .map(|val| Symbol::Function(&val.declaration));
        // Prevent overflows of index
        if item.is_some()
        {
            self.index += 1;
        }
        item
    }
}

impl<'a> SymbolTable<'a> for FunctionSymbolTable<'a> {}

