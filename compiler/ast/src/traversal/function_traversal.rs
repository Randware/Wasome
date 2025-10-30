use crate::statement::Statement;
use crate::symbol::{FunctionSymbol, Symbol, SymbolTable, VariableSymbol};
use crate::top_level::Function;
use crate::traversal::file_traversal::FileTraversalHelper;
use crate::traversal::statement_traversal::{StatementLocation, StatementTraversalHelper};
use crate::{AST, ASTNode, ASTType};
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;

/** This struct helps with traversing the AST
It keeps a reference to the ast and a function.
This allows it to be used to keep track of all symbols available in a function
*/
#[derive(Debug)]
pub struct FunctionTraversalHelper<'a, Type: ASTType> {
    // The referenced function
    inner: &'a ASTNode<Function<Type>>,
    parent: &'a FileTraversalHelper<'a, Type>,
}

impl<'a, Type: ASTType> FunctionTraversalHelper<'a, Type> {
    pub fn new(
        inner: &'a ASTNode<Function<Type>>,
        root: &'a FileTraversalHelper<'a, Type>,
    ) -> Self {
        Self {
            inner,
            parent: root,
        }
    }

    pub fn inner(&self) -> &'a Function<Type> {
        self.inner
    }

    pub fn root(&self) -> &'a FileTraversalHelper<'_, Type> {
        self.parent
    }

    /** Gets a symboltable that has all symbols defined by this (parameters) and symbols from outside this function
     */
    pub fn symbols(&self) -> impl SymbolTable<'_, Type> {
        FunctionSymbolTable::new(self)
    }

    /** Indexes the implementation with index
     */
    pub(crate) fn index_implementation<'b>(
        &'b self,
        index: &StatementLocation,
    ) -> &'b Statement<Type> {
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
    pub fn ref_to_implementation(&self) -> StatementTraversalHelper<'_, Type> {
        StatementTraversalHelper::new_root(self)
    }
}

impl<'a, Type: ASTType> Deref for FunctionTraversalHelper<'a, Type> {
    type Target = Function<Type>;

    fn deref(&self) -> &'a Self::Target {
        self.inner
    }
}

struct FunctionSymbolTable<'a, Type: ASTType> {
    parameters: &'a [Rc<VariableSymbol<Type>>],
    parameter_index: usize,
    functions_declarations: Box<dyn Iterator<Item = &'a FunctionSymbol<Type>> + 'a>,
}

impl<Type: ASTType> Debug for FunctionSymbolTable<'_, Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionSymbolTable")
            .field("parameters", &self.parameters)
            .field("parameter_index", &self.parameter_index)
            // Unable to debug functions, it does not implement debug
            .finish()
    }
}

impl<'a, Type: ASTType> FunctionSymbolTable<'a, Type> {
    fn new(source: &FunctionTraversalHelper<'a, Type>) -> Self {
        Self {
            parameters: source.inner().declaration().params(),
            parameter_index: 0,
            functions_declarations: Box::new(
                source
                    .parent
                    .inner()
                    .functions()
                    .iter()
                    .map(|function| function.declaration()),
            ),
        }
    }
}

impl<'a, Type: ASTType> Iterator for FunctionSymbolTable<'a, Type> {
    type Item = Symbol<'a, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        next_item_from_slice(self.parameters, &mut self.parameter_index)
            .map(|val| Symbol::Variable(val))
            .or_else(|| {
                self.functions_declarations
                    .next()
                    .map(|val| Symbol::Function(val))
            })
    }
}

fn next_item_from_slice<'a, T>(slice: &'a [T], index: &mut usize) -> Option<&'a T> {
    let item = slice.get(*index);
    // Prevent overflows of index
    if item.is_some() {
        *index += 1;
    }
    item
}

impl<'a, Type: ASTType> SymbolTable<'a, Type> for FunctionSymbolTable<'a, Type> {}
