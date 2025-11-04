use crate::{ASTNode, ASTType};
use crate::composite::Struct;
use crate::traversal::file_traversal::FileTraversalHelper;

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