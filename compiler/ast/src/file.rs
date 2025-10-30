use crate::block::FunctionBlock;
use crate::symbol::{FunctionSymbol, Symbol};
use crate::top_level::{Function, Import};
use crate::{ASTNode, ASTType, SemanticEquality};
use crate::visibility::{Visibility, Visible};

#[derive(Debug, PartialEq)]
pub struct File<Type: ASTType> {
    name: String,
    imports: Vec<ASTNode<Import>>,
    functions: FunctionBlock<Type>,
}

impl<Type: ASTType> File<Type> {
    pub fn new(
        name: String,
        imports: Vec<ASTNode<Import>>,
        functions: FunctionBlock<Type>,
    ) -> Self {
        Self {
            name,
            imports,
            functions,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn imports(&self) -> &[ASTNode<Import>] {
        &self.imports
    }

    pub fn functions(&self) -> &FunctionBlock<Type> {
        &self.functions
    }

    pub fn symbol(&self, name: &str) -> Option<Symbol<'_, Type>> {
        self.symbol_specified_origin(name, false)

    }

    pub fn symbol_visible_outside(&self, name: &str) -> Option<Symbol<'_, Type>> {
        self.symbol_specified_origin(name, true)
    }

    fn symbol_specified_origin(&self, name: &str, outside: bool) -> Option<Symbol<'_, Type>> {
        self.function_symbol_outside(name, outside)
            .map(|function_symbol| Symbol::Function(function_symbol))
    }
    
    pub fn specific_function(&self, name: &str) -> Option<&ASTNode<Function<Type>>>
    {
        self.functions()
            .iter()
            .filter(|function| function.declaration().name() == name).next()
    }

    fn function_symbol_outside(&self, name: &str, only_outside: bool) -> Option<&FunctionSymbol<Type>>
    {
        self.specific_function(name).filter(|function| !only_outside || function.visibility() == Visibility::Public).map(|function| function.declaration())

    }
}

impl<Type: ASTType> SemanticEquality for File<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.imports().semantic_equals(other.imports())
            && self.functions().semantic_equals(other.functions())
    }
}
