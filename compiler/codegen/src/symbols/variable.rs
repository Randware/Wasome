use ast::TypedAST;
use ast::symbol::VariableSymbol;
use inkwell::values::PointerValue;
use std::collections::HashMap;
use std::rc::Rc;

/// Holds the LLVM pointer value for a local variable within a function scope.
///
/// Each local variable (including function parameters) is allocated via `alloca` during
/// function prologue code generation. This struct stores the resulting [`PointerValue`]
/// that points to the variable's stack slot.
pub struct VariableInfo<'ctx> {
    /// The LLVM [`PointerValue`] pointing to the variable's `alloca`-allocated stack slot.
    ///
    /// Values are loaded from and stored to this pointer using [`Builder`](inkwell::builder::Builder)
    /// operations during expression and statement code generation.
    pub pointer: PointerValue<'ctx>,
}

/// Maps variable symbols to their corresponding LLVM alloca pointers.
///
/// The [`VariableTable`] is used to track locally declared variables and function parameters
/// during code generation for a single function. It provides lookup by variable symbol to
/// retrieve the LLVM pointer to the variable's stack slot.
///
/// The table is created fresh for each function compilation and is passed through the
/// expression and statement code generation pipeline.
pub struct VariableTable<'ctx> {
    /// Maps each variable symbol to its [`VariableInfo`], which contains the LLVM alloca pointer.
    ///
    /// Uses `Rc<VariableSymbol<TypedAST>>` as the key to allow efficient sharing across
    /// the AST traversal and code generation pipeline.
    vars: HashMap<Rc<VariableSymbol<TypedAST>>, VariableInfo<'ctx>>,
}

impl<'ctx> VariableTable<'ctx> {
    /// Creates an empty variable table.
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    /// Inserts a variable symbol and its corresponding LLVM pointer into the table.
    ///
    /// Called during function compilation to register parameters and during variable
    /// declaration statement compilation to register local variables.
    ///
    /// # Arguments
    ///
    /// * `var` - The variable symbol to register
    /// * `ptr` - The LLVM [`PointerValue`] pointing to the variable's `alloca`-allocated slot
    pub fn insert(&mut self, var: Rc<VariableSymbol<TypedAST>>, ptr: PointerValue<'ctx>) {
        self.vars.insert(var, VariableInfo { pointer: ptr });
    }

    /// Looks up a variable symbol and returns its [`VariableInfo`] if present.
    ///
    /// # Arguments
    ///
    /// * `id` - The variable symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(&VariableInfo)` - The variable info containing the LLVM pointer
    /// * `None` - The variable is not registered in this table
    pub fn lookup(&self, id: &VariableSymbol<TypedAST>) -> Option<&VariableInfo<'ctx>> {
        self.vars.get(id)
    }
}
