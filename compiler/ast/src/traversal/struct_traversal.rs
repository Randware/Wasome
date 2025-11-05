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

    pub fn parent(&self) -> &'a FileTraversalHelper<'a, 'b, Type> {
        self.parent
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
    symbols: Box<dyn Iterator<Item=Symbol<'b, Type>> + 'a>,
}

impl<'a, 'b, Type: ASTType> StructSymbolTable<'a, 'b, Type> {
    pub(crate) fn new(
        symbol_source: &'a StructTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self {
            symbols: Box::new(symbol_source.parent().symbols_trait_object().chain(
                // A symbol might be both imported and directly available
            symbol_source.function_iterator().map(|func| Symbol::Function(func.inner().declaration()))).unique()),
        }
    }
}

impl<'a, 'b, Type: ASTType> Iterator for StructSymbolTable<'a, 'b, Type> {
    type Item = Symbol<'b, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.next()
    }
}

impl<'a, 'b, Type: ASTType> SymbolTable<'b, Type> for StructSymbolTable<'a, 'b, Type> {}