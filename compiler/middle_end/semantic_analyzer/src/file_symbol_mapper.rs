use crate::function_symbol_mapper::FunctionSymbolMapper;
use ast::TypedAST;
use ast::data_type::DataType;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use std::collections::HashMap;
use std::rc::Rc;

pub struct FileSymbolMapper {
    file_functions: HashMap<String, Rc<FunctionSymbol<TypedAST>>>,
    pub local_mapper: FunctionSymbolMapper,
}

impl FileSymbolMapper {
    /** Creates a new FileSymbolMapper instance
      @params  self: (constructor) - no instance yet, creates a new mapper with empty file function table and a fresh FunctionSymbolMapper
      @return A new FileSymbolMapper with initialized fields
    */
    pub fn new() -> Self {
        Self {
            file_functions: HashMap::new(),
            local_mapper: FunctionSymbolMapper::new(),
        }
    }

    /** Adds a function symbol to the file-level symbol table
      @params  self: &mut FileSymbolMapper - the mapper to modify
               symbol: Rc<FunctionSymbol<TypedAST>> - the function symbol to add
      @return Ok(()) if the function was inserted; Err(String) if a function with the same name already exists
    */
    pub fn add_function_to_file(
        &mut self,
        symbol: Rc<FunctionSymbol<TypedAST>>,
    ) -> Result<(), String> {
        let name = symbol.name().to_string();
        if self.file_functions.contains_key(&name) {
            return Err(format!(
                "Error: Function '{}' is already defined in this file.",
                name
            ));
        }
        self.file_functions.insert(name, symbol);
        Ok(())
    }

    /** Looks up a function symbol by name in the file-level table
      @params  self: &FileSymbolMapper - the mapper performing the lookup
               name: &str - the identifier of the function to search for
      @return Some(Rc<FunctionSymbol<TypedAST>>) if a function with `name` is found; None otherwise
    */
    pub fn lookup_function(&self, name: &str) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        self.file_functions.get(name).cloned()
    }

    /** Sets the current function return type in the local FunctionSymbolMapper
      @params  self: &mut FileSymbolMapper - the mapper to modify
               return_type: Option<DataType> - the return type to set (or None to clear)
      @return () - updates the internal local mapper
    */
    pub fn set_current_function_return_type(&mut self, return_type: Option<DataType>) {
        self.local_mapper
            .set_current_function_return_type(return_type);
    }

    /** Gets the current function return type from the local FunctionSymbolMapper
      @params  self: &FileSymbolMapper - the mapper providing the information
      @return Option<DataType> representing the current function return type, or None if not set
    */

    pub fn get_current_function_return_type(&self) -> Option<DataType> {
        self.local_mapper.get_current_function_return_type()
    }

    /** Enters a new local scope in the FunctionSymbolMapper
     @params  self: &mut FileSymbolMapper - the mapper to modify
      @return () - pushes a new scope onto the local scope stack
    */
    pub fn enter_scope(&mut self) {
        self.local_mapper.enter_scope();
    }

    /** Exits the current local scope in the FunctionSymbolMapper
      @params  self: &mut FileSymbolMapper - the mapper to modify
      @return () - pops the current scope from the local scope stack
    */
    pub fn exit_scope(&mut self) {
        self.local_mapper.exit_scope();
    }

    /** Looks up a variable symbol by name in the local scope stack
     @params  self: &FileSymbolMapper - the SymbolMapper performing the lookup
              name: &str - the identifier of the variable to search for
      @return Some(Rc<VariableSymbol<TypedAST>>) if a variable with `name` is found in any scope; None otherwise
    */
    pub fn lookup_variable(&self, name: &str) -> Option<Rc<VariableSymbol<TypedAST>>> {
        self.local_mapper.lookup_variable(name)
    }
}
