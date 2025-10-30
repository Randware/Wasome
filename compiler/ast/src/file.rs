use crate::block::FunctionBlock;
use crate::{ASTNode, ASTType, SemanticEquality};

#[derive(Debug, PartialEq)]
pub struct File<Type: ASTType> {
    name: String,
    imports: Vec<ASTNode<Type::ImportType>>,
    functions: FunctionBlock<Type>,
}

impl<Type: ASTType> File<Type> {
    pub fn new(
        name: String,
        imports: Vec<ASTNode<Type::ImportType>>,
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

    pub fn imports(&self) -> &[ASTNode<Type::ImportType>] {
        &self.imports
    }

    pub fn functions(&self) -> &FunctionBlock<Type> {
        &self.functions
    }
}

impl<Type: ASTType> SemanticEquality for File<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.imports().semantic_equals(other.imports())
            && self.functions().semantic_equals(other.functions())
    }
}
