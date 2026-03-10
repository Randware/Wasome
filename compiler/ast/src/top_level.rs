use crate::statement::Statement;
use crate::symbol::{FunctionSymbol, ModuleUsageNameSymbol};
use crate::visibility::{Visibility, Visible};
use crate::{ASTNode, ASTType, SemanticEq};
use std::fmt::Debug;
use std::rc::Rc;

/// A function
///
/// This can be called via a [`FunctionCall`](crate::expression::FunctionCall)
#[derive(Debug, PartialEq)]
pub struct Function<Type: ASTType> {
    declaration: Rc<FunctionSymbol<Type>>,
    implementation: ASTNode<Statement<Type>>,
    // The visibility is irrelevant when calling
    // Therefore, it doesn't belong into FunctionSymbol and should be here
    visibility: Visibility,
}

impl<Type: ASTType> Function<Type> {
    pub const fn new(
        declaration: Rc<FunctionSymbol<Type>>,
        implementation: ASTNode<Statement<Type>>,
        visibility: Visibility,
    ) -> Self {
        Self {
            declaration,
            implementation,
            visibility,
        }
    }

    pub fn declaration(&self) -> &FunctionSymbol<Type> {
        &self.declaration
    }

    /// Gets the declaration by cloning the rc
    pub fn declaration_owned(&self) -> Rc<FunctionSymbol<Type>> {
        self.declaration.clone()
    }

    pub const fn implementation(&self) -> &ASTNode<Statement<Type>> {
        &self.implementation
    }
}

impl<Type: ASTType> SemanticEq for Function<Type> {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.declaration().semantic_eq(other.declaration())
            && self.implementation.semantic_eq(&other.implementation)
    }
}

impl<Type: ASTType> Visible for Function<Type> {
    fn visibility(&self) -> Visibility {
        self.visibility
    }
}

/// An import
/// In the typed AST, this has no semantic meaning and is only there to not lose any information
#[derive(Debug, PartialEq, Eq)]
pub struct Import {
    root: ImportRoot,
    path: Vec<String>,
    /// The name under which the result can be used
    ///
    /// This is:
    /// - **The name provided in the as-syntax** if one was provided
    /// - **The last part of path** if path is not empty**
    /// - **./** if root is [`ImportRoot::CurrentModule`]
    /// - **<The project name** if root is [`ImportRoot::Root`]
    ///
    /// If multiple match, only the first match is considered
    usage_name: Rc<ModuleUsageNameSymbol>,
}

impl Import {
    #[must_use]
    pub const fn new(
        root: ImportRoot,
        path: Vec<String>,
        usage_name: Rc<ModuleUsageNameSymbol>,
    ) -> Self {
        Self {
            root,
            path,
            usage_name,
        }
    }

    #[must_use]
    pub const fn root(&self) -> &ImportRoot {
        &self.root
    }

    #[must_use]
    pub fn path(&self) -> &[String] {
        &self.path
    }

    #[must_use]
    pub fn usage_name(&self) -> &ModuleUsageNameSymbol {
        &self.usage_name
    }

    /// Gets the usage name by cloning the underlying Rc
    #[must_use]
    pub fn usage_name_owned(&self) -> Rc<ModuleUsageNameSymbol> {
        self.usage_name.clone()
    }
}

impl SemanticEq for Import {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.root() == other.root()
            && self.path() == other.path()
            && self.usage_name().semantic_eq(other.usage_name())
    }
}

/// The place from where the provided path in an import originates
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ImportRoot {
    /// In the current module, comparable with ./
    CurrentModule,
    /// In the AST root
    Root,
}
