use crate::block::FunctionBlock;
use crate::symbol::{FunctionSymbol, Symbol};
use crate::top_level::Import;
use crate::{ASTNode, ASTType, SemanticEquality};

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

    pub fn get_top_level_symbol(&self, name: &str) -> Option<Symbol<'_, Type>> {
        self.get_function_symbol(name)
            .map(|function_symbol| Symbol::Function(function_symbol))
    }

    fn get_function_symbol(&self, name: &str) -> Option<&FunctionSymbol<Type>> {
        self.functions()
            .iter()
            .filter(|function| function.declaration().name() == name)
            .map(|function| function.declaration())
            .next()
    }
}

impl<Type: ASTType> SemanticEquality for File<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.imports().semantic_equals(other.imports())
            && self.functions().semantic_equals(other.functions())
    }
}
