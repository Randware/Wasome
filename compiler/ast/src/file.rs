use crate::block::FunctionBlock;
use crate::symbol::{FunctionSymbol, Symbol};
use crate::top_level::{Function, Import};
use crate::visibility::{Visibility, Visible};
use crate::{ASTNode, ASTType, SemanticEquality};

#[derive(Debug, PartialEq)]
pub struct File<Type: ASTType> {
    /// Filename without the file extension
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

    /** Gets the symbol with the specified name
     */
    pub fn symbol(&self, name: &str) -> Option<Symbol<'_, Type>> {
        self.symbol_specified_origin(name, false)
    }

    /** Gets the symbol with the specified name if it is public
     */
    pub fn symbol_public(&self, name: &str) -> Option<Symbol<'_, Type>> {
        self.symbol_specified_origin(name, true)
    }

    /** Gets the symbol with the specified name if it is public or outside is false
     */
    fn symbol_specified_origin(&self, name: &str, outside: bool) -> Option<Symbol<'_, Type>> {
        self.function_symbol(name, outside)
            .map(|function_symbol| Symbol::Function(function_symbol))
    }

    /** Gets the function with the specified name
     */
    pub fn specific_function(&self, name: &str) -> Option<&ASTNode<Function<Type>>> {
        self.functions()
            .iter()
            .find(|function| function.declaration().name() == name)
    }

    /** Gets the function with the specified name if it is public or only_public is false
     */
    fn function_symbol(&self, name: &str, only_public: bool) -> Option<&FunctionSymbol<Type>> {
        self.specific_function(name)
            .filter(|function| !only_public || function.visibility() == Visibility::Public)
            .map(|function| function.declaration())
    }
}

impl<Type: ASTType> SemanticEquality for File<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.imports().semantic_equals(other.imports())
            && self.functions().semantic_equals(other.functions())
    }
}
