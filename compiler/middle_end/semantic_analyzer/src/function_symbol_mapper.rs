use crate::file_symbol_mapper::FileSymbolMapper;
use ast::TypedAST;
use ast::data_type::DataType;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    variables: HashMap<String, Rc<VariableSymbol<TypedAST>>>,
}

pub struct FunctionSymbolMapper<'a> {
    scope_stack: Vec<Scope>,
    current_function_return_type: Option<DataType>,
    file_mapper: &'a mut FileSymbolMapper,
}

impl<'a> FunctionSymbolMapper<'a> {
    /** Creates a new instance of FunctionSymbolMapper.
     * Initializes with a reference to the global file scope and establishes the base scope.
     * @param file_mapper: &'a mut FileSymbolMapper - The global symbol table for the file.
     * @return A new FunctionSymbolMapper with initialized fields and an active base scope.
     */
    pub fn new(file_mapper: &'a mut FileSymbolMapper) -> Self {
        let mut mapper = Self {
            scope_stack: Vec::new(),
            current_function_return_type: None,
            file_mapper,
        };
        mapper.enter_scope();
        mapper
    }

    /** Enters a new scope on the scope stack.
     * @param self: The mapper whose scope stack will receive a new, empty scope.
     * @return () - Pushes a new Scope (with no variables) onto the internal scope_stack.
     */
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(Scope {
            variables: HashMap::new(),
        });
    }

    /** Exits the current scope on the scope stack.
     * @param self: The mapper whose current scope will be removed.
     * @return () - Pops the last Scope from the internal scope_stack (if any).
     */
    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    /** Looks up a function symbol by name.
     * Delegates the lookup to the associated FileSymbolMapper (global scope).
     * @param name: &str - The identifier of the function to search for.
     * @return Some(Rc<FunctionSymbol<TypedAST>>) if found; None otherwise.
     */
    pub fn lookup_function(&self, name: &str) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        self.file_mapper.lookup_function(name)
    }

    /** Adds a variable symbol to the current scope.
     * @param self: The SymbolMapper to modify.
     * @param symbol: Rc<VariableSymbol<TypedAST>> - The variable symbol to insert.
     * @return Ok(()) if inserted; Err(String) if a variable with the same name already exists in the current scope.
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

    /** Looks up a variable symbol by name in the scope stack.
     * Searches from the innermost scope outwards.
     * @param self: The SymbolMapper performing the lookup.
     * @param name: &str - The identifier of the variable to search for.
     * @return Some(Rc<VariableSymbol<TypedAST>>) if a variable with `name` is found; None otherwise.
     */
    pub fn lookup_variable(&self, name: &str) -> Option<Rc<VariableSymbol<TypedAST>>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.variables.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }

    /** Sets the current function return type for subsequent analysis.
     * @param self: The SymbolMapper to modify.
     * @param return_type: Option<DataType> - The return type to record for the currently analyzed function (None for void).
     * @return () - Updates the internal current_function_return_type.
     */
    pub fn set_current_function_return_type(&mut self, return_type: Option<DataType>) {
        self.current_function_return_type = return_type;
    }

    /** Gets the currently recorded function return type.
     * @param self: The SymbolMapper used to query the return type.
     * @return Option<DataType> - The recorded return type for the current function, or None if not set.
     */
    pub fn get_current_function_return_type(&self) -> Option<DataType> {
        self.current_function_return_type
    }
}

#[cfg(test)]
mod tests {
    use super::FunctionSymbolMapper;
    use crate::file_symbol_mapper::FileSymbolMapper;
    use ast::data_type::DataType;

    #[test]
    fn new_has_no_return_type() {
        let mut file_mapper = FileSymbolMapper::new();
        let mapper = FunctionSymbolMapper::new(&mut file_mapper);

        assert!(
            mapper.get_current_function_return_type().is_none(),
            "Expected no current return type for new mapper."
        );
    }

    #[test]
    fn set_and_get_current_function_return_type() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        mapper.set_current_function_return_type(Some(DataType::S32));
        assert_eq!(
            mapper.get_current_function_return_type(),
            Some(DataType::S32),
            "Should return the set return type."
        );

        mapper.set_current_function_return_type(None);
        assert!(
            mapper.get_current_function_return_type().is_none(),
            "Should be None after clearing the return type."
        );
    }

    #[test]
    fn enter_and_exit_scope_do_not_panic_and_keep_lookups_none() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        mapper.enter_scope();
        assert!(
            mapper.lookup_variable("nope").is_none(),
            "Lookup should still be None in new inner scope."
        );
        mapper.exit_scope();
    }

    #[test]
    fn add_multiple_variables_same_scope() {
        use ast::symbol::VariableSymbol;
        use std::rc::Rc;

        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let v1 = Rc::new(VariableSymbol::new(String::from("a"), DataType::S32));
        let v2 = Rc::new(VariableSymbol::new(String::from("b"), DataType::S64));

        assert!(
            mapper.add_variable(v1.clone()).is_ok(),
            "Should add first variable"
        );
        assert!(
            mapper.add_variable(v2.clone()).is_ok(),
            "Should add second variable with different name"
        );

        let found_a = mapper.lookup_variable("a").expect("`a` should be found");
        assert_eq!(found_a.name(), "a", "Found variable should have name 'a'");

        let found_b = mapper.lookup_variable("b").expect("`b` should be found");
        assert_eq!(found_b.name(), "b", "Found variable should have name 'b'");
    }

    #[test]
    fn add_duplicate_variable_errors() {
        use ast::symbol::VariableSymbol;
        use std::rc::Rc;

        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let v1 = Rc::new(VariableSymbol::new(String::from("x"), DataType::S32));
        let v2 = Rc::new(VariableSymbol::new(String::from("x"), DataType::S64));

        assert!(
            mapper.add_variable(v1).is_ok(),
            "First definition should succeed"
        );
        assert!(
            mapper.add_variable(v2).is_err(),
            "Second definition with same name should error"
        );
    }

    #[test]
    fn shadow_variable_in_inner_scope() {
        use ast::symbol::VariableSymbol;
        use std::rc::Rc;

        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let outer = Rc::new(VariableSymbol::new(String::from("s"), DataType::S32));
        mapper.add_variable(outer).expect("Add outer variable");

        mapper.enter_scope();

        let inner = Rc::new(VariableSymbol::new(String::from("s"), DataType::S64));
        assert!(
            mapper.add_variable(inner).is_ok(),
            "Inner scope should allow same name (shadowing)"
        );

        let found = mapper.lookup_variable("s").expect("`s` should be found");
        assert_eq!(
            found.data_type(),
            &DataType::S64,
            "Lookup should return the inner (shadowing) variable's type (S64)"
        );
    }
}
