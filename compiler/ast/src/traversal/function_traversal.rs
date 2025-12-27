use crate::symbol::{ModuleUsageNameSymbol, DirectlyAvailableSymbol, SymbolTable, VariableSymbol};
use crate::top_level::Function;
use crate::traversal::file_traversal::FileTraversalHelper;
use crate::traversal::statement_traversal::StatementTraversalHelper;
use crate::{ASTNode, ASTType};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use crate::traversal::HasSymbols;

/// This struct helps with traversing the AST
/// It keeps a reference to the ast and a function.
/// This allows it to be used to keep track of all symbols available in a function
///
/// # Lifetimes
///
/// | Lifetime     | Purpose      |
/// | ------------- | ------------- |
/// | 'a | How long the traversal helper may life |
/// | 'b | How long the underlying data may life |
#[derive(Debug)]
pub struct FunctionTraversalHelper<'a, 'b, Type: ASTType> {
    // The referenced function
    inner: &'b ASTNode<Function<Type>>,
    parent: &'a dyn HasSymbols<'b, Type>,
}

impl<'a, 'b, Type: ASTType> FunctionTraversalHelper<'a, 'b, Type> {
    pub fn new(inner: &'b ASTNode<Function<Type>>, root: &'a dyn HasSymbols<'b, Type>) -> Self {
        Self {
            inner,
            parent: root,
        }
    }

    pub fn inner(&self) -> &'b Function<Type> {
        self.inner
    }

    pub fn root(&self) -> &'a dyn HasSymbols<'b, Type> {
        self.parent
    }

    /// Gets a symboltable that has all symbols defined by this (parameters) and symbols from outside this function
    pub fn symbols(&self) -> impl SymbolTable<'b, Type> + 'a {
        FunctionSymbolTable::new(self)
    }

    /// Gets a StatementRef for the top level statement in this function
    /// This is the intended way to traverse a function
    pub fn ref_to_implementation(&self) -> StatementTraversalHelper<'_, 'b, Type> {
        StatementTraversalHelper::new_root(self)
    }
}

impl<'a, 'b, Type: ASTType> HasSymbols<'b, Type> for FunctionTraversalHelper<'a, 'b, Type> {
    fn symbols(&self) -> impl SymbolTable<'b, Type> {
        FunctionSymbolTable::new(self)
    }

    fn symbols_trait_object(&self) -> Box<dyn SymbolTable<'b, Type> + '_> {
        Box::new(self.symbols())
    }
}

struct FunctionSymbolTable<'a, 'b, Type: ASTType> {
    parent_symbols: Box<dyn SymbolTable<'b, Type> + 'a>,
    parameters: &'b [Rc<VariableSymbol<Type>>],
    parameter_index: usize,
}

impl<Type: ASTType> Debug for FunctionSymbolTable<'_, '_, Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionSymbolTable")
            .field("parameters", &self.parameters)
            .field("parameter_index", &self.parameter_index)
            // Unable to debug functions, it does not implement debug
            .finish()
    }
}

impl<'a, 'b, Type: ASTType> FunctionSymbolTable<'a, 'b, Type> {
    fn new(source: &FunctionTraversalHelper<'a, 'b, Type>) -> Self {
        Self {
            parent_symbols: source.root().symbols_trait_object(),
            parameters: source.inner().declaration().params(),
            parameter_index: 0,
        }
    }
}

impl<'a, 'b, Type: ASTType> Iterator for FunctionSymbolTable<'a, 'b, Type> {
    type Item = (Option<&'b ModuleUsageNameSymbol>, DirectlyAvailableSymbol<'b, Type>);

    fn next(&mut self) -> Option<Self::Item> {
        next_item_from_slice(self.parameters, &mut self.parameter_index)
            .map(|val| (None, DirectlyAvailableSymbol::Variable(val)))
            .or_else(|| self.parent_symbols.next())
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

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for FunctionSymbolTable<'a, 'b, Type> {}
