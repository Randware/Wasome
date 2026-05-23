use crate::composite::Enum;
use crate::symbol::{DirectlyAvailableSymbol, ModuleUsageNameSymbol, SymbolTable};
use crate::traversal::file_traversal::FileTraversalHelper;
use crate::traversal::HasSymbols;
use crate::{ASTNode, ASTType};

#[derive(Debug, Clone)]
pub struct EnumTraversalHelper<'a, 'b, Type: ASTType> {
    inner: &'b ASTNode<Enum<Type>>,
    parent: &'a FileTraversalHelper<'a, 'b, Type>,
}

impl<'a, 'b, Type: ASTType> EnumTraversalHelper<'a, 'b, Type> {
    #[must_use]
    pub const fn new(
        inner: &'b ASTNode<Enum<Type>>,
        parent: &'a FileTraversalHelper<'a, 'b, Type>,
    ) -> Self {
        Self { inner, parent }
    }

    #[must_use]
    pub const fn inner(&self) -> &'b ASTNode<Enum<Type>> {
        self.inner
    }

    #[must_use]
    pub const fn parent(&self) -> &'a FileTraversalHelper<'a, 'b, Type> {
        self.parent
    }
}

impl<'b, Type: ASTType> HasSymbols<'b, Type> for EnumTraversalHelper<'_, 'b, Type> {
    fn symbols<'c>(&'c self) -> impl SymbolTable<'b, Type> + 'c {
        EnumSymbolTable::new(self)
    }

    fn symbols_trait_object(&self) -> Box<dyn SymbolTable<'b, Type> + '_> {
        Box::new(self.symbols())
    }
}

struct EnumSymbolTable<'a, 'b, Type: ASTType> {
    symbols: Box<
        dyn Iterator<
                Item = (
                    Option<&'b ModuleUsageNameSymbol>,
                    DirectlyAvailableSymbol<'b, Type>,
                ),
            > + 'a,
    >,
}

impl<'a, 'b, Type: ASTType> EnumSymbolTable<'a, 'b, Type> {
    pub(crate) fn new(symbol_source: &'a EnumTraversalHelper<'a, 'b, Type>) -> Self {
        Self {
            symbols: Box::new(
                symbol_source.parent().symbols_trait_object().chain(
                    Type::type_parameter_symbols_of_symbol_with_type_parameter(
                        symbol_source.inner().symbol(),
                    )
                    .map(|type_param| {
                        (
                            None,
                            // For typed enums, there are never any type parameter symbols,
                            // so this is fine
                            DirectlyAvailableSymbol::UntypedTypeParameter(type_param),
                        )
                    }),
                ),
            ),
        }
    }
}

impl<'b, Type: ASTType> Iterator for EnumSymbolTable<'_, 'b, Type> {
    type Item = (
        Option<&'b ModuleUsageNameSymbol>,
        DirectlyAvailableSymbol<'b, Type>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        self.symbols.next()
    }
}

impl<'b, Type: ASTType> SymbolTable<'b, Type> for EnumSymbolTable<'_, 'b, Type> {}
