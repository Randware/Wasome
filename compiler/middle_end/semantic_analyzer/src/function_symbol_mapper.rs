use ast::TypedAST;
use ast::data_type::DataType;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    variables: HashMap<String, Rc<VariableSymbol<TypedAST>>>,
    functions: HashMap<String, Rc<FunctionSymbol<TypedAST>>>,
}

pub struct FunctionSymbolMapper {
    scope_stack: Vec<Scope>,
    current_function_return_type: Option<DataType>,
}

impl FunctionSymbolMapper {
    /** Creates a new instance of SymbolMapper
     *  @params  None
     *  @return A new SymbolMapper initialized with a base scope and no current function return type
     */
    pub fn new() -> Self {
        let mut mapper = Self {
            scope_stack: Vec::new(),
            current_function_return_type: None,
        };
        mapper.enter_scope();
        mapper
    }

    /** Enters a new scope on the scope stack
     *  @params  self: The SymbolMapper whose scope stack will receive a new, empty scope
     *  @return () - pushes a new Scope onto the internal scope_stack
     */
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        });
    }

    /** Exits the current scope on the scope stack
     *  @params  self: The SymbolMapper whose current scope will be removed
     *  @return () - pops the last Scope from the internal scope_stack (if any)
     */
    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }
}

/** Creates a default SymbolMapper
 *  @params  None
 *  @return Self - same as SymbolMapper::new(), used for Default trait implementation
 */
impl Default for FunctionSymbolMapper {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionSymbolMapper {
    /** Looks up a variable symbol by name in the scope stack
     *  @params  self: The SymbolMapper performing the lookup
     *           name: &str - the identifier of the variable to search for
     *  @return Some(Rc<VariableSymbol<TypedAST>>) if a variable with `name` is found in any scope; None otherwise
     */
    pub fn lookup_variable(&self, name: &str) -> Option<Rc<VariableSymbol<TypedAST>>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.variables.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }

    /** Adds a function symbol to the current scope
     * @params  self: The SymbolMapper to modify
     * symbol: Rc<FunctionSymbol<TypedAST>> - the function symbol to insert into the current scope
     * @return Result<(), String> - returns Ok(()) on success or an error message if the name is already defined in the current scope
     */
    pub fn add_function(&mut self, symbol: Rc<FunctionSymbol<TypedAST>>) -> Result<(), String> {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            let name = symbol.name().to_string();

            if current_scope.functions.contains_key(&name) {
                return Err(format!(
                    "Error: Redefinition of function '{}' in current scope.",
                    name
                ));
            }

            current_scope.functions.insert(name, symbol);
            Ok(())
        } else {
            Err("Cannot add function: No active scope.".to_string())
        }
    }

    /** Looks up a function symbol by name in the scope stack
     *  @params  self: The SymbolMapper performing the lookup
     *           name: &str - the identifier of the function to search for
     *  @return Some(Rc<FunctionSymbol<TypedAST>>) if a function with `name` is found in any scope; None otherwise
     */
    pub fn lookup_function(&self, name: &str) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.functions.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }
}

impl FunctionSymbolMapper {
    /** Adds a variable symbol to the current scope
     *  @params  self: The SymbolMapper to modify
     *           symbol: Rc<VariableSymbol<TypedAST>> - the variable symbol to insert into the current scope
     *  @return () - inserts the symbol into the variables map of the last scope if present
     */
    pub fn add_variable(&mut self, symbol: Rc<VariableSymbol<TypedAST>>) -> Result<(), String> {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            let name = symbol.name().to_string();

            if current_scope.variables.contains_key(&name) {
                return Err(format!(
                    "Error: Variable '{}' is already defined in the current scope.",
                    name
                ));
            }

            current_scope.variables.insert(name, symbol);
            Ok(())
        } else {
            Err("Cannot define variable: No active scope.".to_string())
        }
    }

    /** Sets the current function return type for subsequent analysis
     *  @params  self: The SymbolMapper to modify
     *           return_type: Option<DataType> - the return type to record for the currently analyzed function (None if not applicable)
     *  @return () - updates the internal current_function_return_type
     */
    pub fn set_current_function_return_type(&mut self, return_type: Option<DataType>) {
        self.current_function_return_type = return_type;
    }

    /** Gets the currently recorded function return type
     *  @params  self: The SymbolMapper used to query the return type
     *  @return Option<DataType> - the recorded return type for the current function, or None if none is set
     */
    pub fn get_current_function_return_type(&self) -> Option<DataType> {
        self.current_function_return_type
    }


}


#[cfg(test)]
mod tests {
    use super::FunctionSymbolMapper;
    use ast::data_type::DataType;

    #[test]
    fn new_and_default_have_no_return_type() {
        let mapper = FunctionSymbolMapper::new();
        let default = FunctionSymbolMapper::default();

        assert!(mapper.get_current_function_return_type().is_none(), "Expected no current return type for new mapper.");
        assert!(default.get_current_function_return_type().is_none(), "Expected no current return type for default mapper.");
    }

    #[test]
    fn lookup_empty_returns_none() {
        let mapper = FunctionSymbolMapper::new();

        assert!(mapper.lookup_variable("x").is_none(), "Expected lookup_variable to return None for unknown name.");
        assert!(mapper.lookup_function("f").is_none(), "Expected lookup_function to return None for unknown name.");
    }

    #[test]
    fn set_and_get_current_function_return_type() {
        let mut mapper = FunctionSymbolMapper::new();

        mapper.set_current_function_return_type(Some(DataType::S32));
        assert_eq!(mapper.get_current_function_return_type(), Some(DataType::S32), "Should return the set return type.");

        mapper.set_current_function_return_type(None);
        assert!(mapper.get_current_function_return_type().is_none(), "Should be None after clearing the return type.");
    }

    #[test]
    fn enter_and_exit_scope_do_not_panic_and_keep_lookups_none() {
        let mut mapper = FunctionSymbolMapper::new();

        mapper.enter_scope();
        assert!(mapper.lookup_variable("nope").is_none(), "Lookup should still be None in new inner scope.");
        mapper.exit_scope();

        mapper.exit_scope();
        assert!(mapper.lookup_function("nope").is_none(), "Lookup should remain None after extra exit.");
    }

    #[test]
    fn add_multiple_variables_same_scope() {
        use std::rc::Rc;
        use ast::symbol::VariableSymbol;

        let mut mapper = FunctionSymbolMapper::new();

        let v1 = Rc::new(VariableSymbol::new(String::from("a"), DataType::S32));
        let v2 = Rc::new(VariableSymbol::new(String::from("b"), DataType::S64));

        assert!(mapper.add_variable(v1.clone()).is_ok(), "Should add first variable");
        assert!(mapper.add_variable(v2.clone()).is_ok(), "Should add second variable with different name");

        let found_a = mapper.lookup_variable("a").expect("`a` should be found");
        assert_eq!(found_a.name(), "a", "Found variable should have name 'a'");

        let found_b = mapper.lookup_variable("b").expect("`b` should be found");
        assert_eq!(found_b.name(), "b", "Found variable should have name 'b'");
    }

    #[test]
    fn add_duplicate_variable_errors() {
        use std::rc::Rc;
        use ast::symbol::VariableSymbol;

        let mut mapper = FunctionSymbolMapper::new();

        let v1 = Rc::new(VariableSymbol::new(String::from("x"), DataType::S32));
        let v2 = Rc::new(VariableSymbol::new(String::from("x"), DataType::S64));

        assert!(mapper.add_variable(v1).is_ok(), "First definition should succeed");
        assert!(mapper.add_variable(v2).is_err(), "Second definition with same name should error");
    }

    #[test]
    fn shadow_variable_in_inner_scope() {
        use std::rc::Rc;
        use ast::symbol::VariableSymbol;

        let mut mapper = FunctionSymbolMapper::new();

        let outer = Rc::new(VariableSymbol::new(String::from("s"), DataType::S32));
        mapper.add_variable(outer).expect("Add outer variable");

        mapper.enter_scope();

        let inner = Rc::new(VariableSymbol::new(String::from("s"), DataType::S64));
        assert!(mapper.add_variable(inner).is_ok(), "Inner scope should allow same name (shadowing)");

        let found = mapper.lookup_variable("s").expect("`s` should be found");
        assert_eq!(found.name(), "s", "Lookup should return the (inner) symbol named 's'");
    }

}
