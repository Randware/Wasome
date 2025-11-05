use itertools::Itertools;
use crate::{ASTNode, ASTType};
use crate::composite::Struct;
use crate::symbol::{FunctionSymbol, StructSymbol, Symbol, SymbolTable};
use crate::traversal::file_traversal::FileTraversalHelper;
use crate::traversal::function_traversal::FunctionTraversalHelper;
use crate::traversal::{FunctionContainer, HasSymbols};
use crate::traversal::statement_traversal::StatementTraversalHelper;

#[derive(Debug)]
pub struct StructTraversalHelper<'a, 'b, Type: ASTType>
{
    inner: &'b ASTNode<Struct<Type>>,
    parent: &'a FileTraversalHelper<'a, 'b, Type>
}

impl<'a, 'b, Type: ASTType> StructTraversalHelper<'a, 'b, Type>
{
    pub fn new(inner: &'b ASTNode<Struct<Type>>, parent: &'a FileTraversalHelper<'a, 'b, Type>) -> Self {
        Self { inner, parent }
    }

    pub fn inner(&self) -> &'b ASTNode<Struct<Type>> {
        self.inner
    }
}

impl<'a, 'b, Type: ASTType> FunctionContainer<'b, Type> for StructTraversalHelper<'a, 'b, Type>
{
    fn len_functions(&self) -> usize {
        self.inner.functions().len()
    }

    fn index_function(&self, index: usize) -> FunctionTraversalHelper<'_, 'b, Type> {
        FunctionTraversalHelper::new(&self.inner.functions()[index], self)
    }

    fn function_iterator<'c>(&'c self) -> impl DoubleEndedIterator<Item=FunctionTraversalHelper<'c, 'b, Type>> + 'c
    where
        'b: 'c
    {
        self.inner
            .functions()
            .iter()
            .map(|function| FunctionTraversalHelper::new(function, self))
    }
}

impl<'a, 'b, Type: ASTType> HasSymbols<'b, Type> for StructTraversalHelper<'a, 'b, Type>
{
    fn symbols<'c>(&'c self) -> impl SymbolTable<'b, Type> + 'c {
        StructSymbolTable::new(self)
    }

    fn symbols_trait_object(&self) -> Box<dyn SymbolTable<'b, Type> + '_> {
        Box::new(self.symbols())
    }
}

struct StructSymbolTable<'a, 'b, Type: ASTType> {
    parent_symbols: Box<dyn SymbolTable<'b, Type> + 'a>,
    self_function_symbols: Box<dyn Iterator<Item=&'b FunctionSymbol<Type>> + 'a>,
    self_symbol: &'b StructSymbol<Type>,
    self_symbol_returned: bool
}

impl<'a, 'b, Type: ASTType> StructSymbolTable<'a, 'b, Type> {
    pub(crate) fn new(
        symbol_source: &'a StructTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self {
            parent_symbols: symbol_source.symbols_trait_object(),
            self_function_symbols: Box::new(symbol_source.function_iterator().map(|func| func.inner().declaration())),
            self_symbol: symbol_source.inner().symbol(),
            self_symbol_returned: false,
        }
    }
}

impl<'a, 'b, Type: ASTType> Iterator for StructSymbolTable<'a, 'b, Type> {
    type Item = Symbol<'b, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.self_symbol_returned
        {
            self.self_symbol_returned = true;
            return Some(Symbol::Struct(self.self_symbol));
        }
        self.parent_symbols.next()
            .or_else(|| self.self_function_symbols.next().map(|func| Symbol::Function(func)))
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for StructSymbolTable<'a, 'b, Type> {}