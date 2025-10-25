use ast::TypedAST;
use ast::data_type::DataType;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    variables: HashMap<String, Rc<VariableSymbol<TypedAST>>>,
    functions: HashMap<String, Rc<FunctionSymbol<TypedAST>>>,
}

pub struct SymbolMapper {
    scope_stack: Vec<Scope>,
    current_function_return_type: Option<DataType>,
}

impl SymbolMapper {
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
impl Default for SymbolMapper {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolMapper {
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

impl SymbolMapper {
    /** Adds a variable symbol to the current scope
     *  @params  self: The SymbolMapper to modify
     *           symbol: Rc<VariableSymbol<TypedAST>> - the variable symbol to insert into the current scope
     *  @return () - inserts the symbol into the variables map of the last scope if present
     */
    pub fn add_variable(&mut self, symbol: Rc<VariableSymbol<TypedAST>>) {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope
                .variables
                .insert(symbol.name().to_string(), symbol);
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