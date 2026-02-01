use ast::TypedAST;
use ast::data_type::DataType;
use ast::symbol::VariableSymbol;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    variables: HashMap<String, Rc<VariableSymbol<TypedAST>>>,
}

pub struct FunctionSymbolMapper {
    scope_stack: Vec<Scope>,
    current_function_return_type: Option<DataType>,
}

impl FunctionSymbolMapper {
    /// Creates a new instance of `FunctionSymbolMapper`.
    ///
    /// Initializes with a reference to the global file scope and establishes the base scope.
    ///
    /// # Parameters
    /// * `file_mapper` - The global symbol table for the file (`&'a mut FileSymbolMapper`).
    ///
    /// # Returns
    /// * A new `FunctionSymbolMapper` with initialized fields and an active base scope.
    pub fn new() -> Self {
        let mut mapper = Self {
            scope_stack: Vec::new(),
            current_function_return_type: None,
        };
        mapper.enter_scope();
        mapper
    }

    /// Enters a new scope on the scope stack.
    ///
    /// # Returns
    /// * `()` - Pushes a new `Scope` (with no variables) onto the internal `scope_stack`.
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(Scope {
            variables: HashMap::new(),
        });
    }

    /// Exits the current scope.
    ///
    /// Removes the topmost scope from the stack.
    ///
    /// # Errors
    /// *   Returns `Err` if the stack is empty (Internal Compiler Error).
    /// *   Returns `Err` if attempting to pop the **base scope** (stack size 1). The base scope must persist for the function's lifetime.
    ///
    /// # Returns
    /// *   `Ok(())` if a scope was successfully popped.
    pub fn exit_scope(&mut self) -> Result<(), String> {
        if self.scope_stack.is_empty() {
            return Err(String::from(
                "Internal Compiler Error: Attempted to exit scope on an empty stack.",
            ));
        }

        if self.scope_stack.len() == 1 {
            return Err(String::from(
                "Internal Compiler Error: Attempted to pop the function's base scope.",
            ));
        }

        self.scope_stack.pop();
        Ok(())
    }

    /// Adds a variable symbol to the current scope.
    ///
    /// # Shadowing
    /// Forbids shadowing within the **same** scope. If a variable with the same name already exists in the current scope, returns an error.
    /// Shadowing variables from outer scopes is permitted.
    ///
    /// # Parameters
    /// * `symbol` - The variable symbol to insert (`Rc<VariableSymbol<TypedAST>>`).
    ///
    /// # Returns
    /// * `Ok(())` if inserted.
    /// * `Err(String)` if a variable with the same name already exists in the current scope.
    pub fn add_variable(&mut self, symbol: Rc<VariableSymbol<TypedAST>>) -> Result<(), String> {
        let current_scope = self
            .scope_stack
            .last_mut()
            .expect("Internal Compiler Error: Scope stack is empty in add_variable.");

        let name = symbol.name().to_string();

        if current_scope.variables.contains_key(&name) {
            return Err(format!(
                "Error: Variable '{}' is already defined in the current scope.",
                name
            ));
        }

        current_scope.variables.insert(name, symbol);
        Ok(())
    }

    /// Looks up a variable symbol by name in the scope stack.
    ///
    /// Searches from the innermost scope outwards.
    ///
    /// # Parameters
    /// * `name` - The identifier of the variable to search for (`&str`).
    ///
    /// # Returns
    /// * `Some(Rc<VariableSymbol<TypedAST>>)` if a variable with `name` is found.
    /// * `None` otherwise.
    pub fn lookup_variable(&self, name: &str) -> Option<Rc<VariableSymbol<TypedAST>>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.variables.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }

    /// Sets the current function return type for subsequent analysis.
    ///
    /// # Parameters
    /// * `return_type` - The return type to record for the currently analyzed function (`None` for void).
    pub fn set_current_function_return_type(&mut self, return_type: Option<DataType>) {
        self.current_function_return_type = return_type;
    }

    /// Gets the currently recorded function return type.
    ///
    /// # Returns
    /// * `Option<DataType>` - The recorded return type for the current function, or `None` if not set.
    pub fn get_current_function_return_type(&self) -> Option<&DataType> {
        self.current_function_return_type.as_ref()
    }
}
