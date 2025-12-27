use ast::TypedAST;
use ast::symbol::FunctionSymbol;
use std::collections::HashMap;
use std::rc::Rc;

pub struct FileSymbolMapper {
    file_functions: HashMap<String, Rc<FunctionSymbol<TypedAST>>>,
}

impl FileSymbolMapper {
    /// Creates a new `FileSymbolMapper` instance.
    ///
    /// # Returns
    /// * A new `FileSymbolMapper` with an empty function table.
    pub fn new() -> Self {
        Self {
            file_functions: HashMap::new(),
        }
    }

    /// Adds a function symbol to the file-level symbol table.
    ///
    /// # Parameters
    /// * `symbol` - The function symbol to add (`Rc<FunctionSymbol<TypedAST>>`).
    ///
    /// # Returns
    /// * `Ok(())` if the function was inserted.
    /// * `Err(String)` if a function with the same name already exists.
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

    /// Looks up a function symbol by name in the file-level table.
    ///
    /// This method is used by the `FunctionSymbolMapper` to resolve global function calls.
    ///
    /// # Parameters
    /// * `name` - The identifier of the function to search for (`&str`).
    ///
    /// # Returns
    /// * `Some(Rc<FunctionSymbol<TypedAST>>)` if a function with `name` is found.
    /// * `None` otherwise.
    pub fn lookup_function(&self, name: &str) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        self.file_functions.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::data_type::DataType;
    use ast::symbol::{FunctionSymbol, VariableSymbol};
    use std::rc::Rc;

    fn create_test_function_symbol(
        name: &str,
        return_type: Option<DataType>,
    ) -> Rc<FunctionSymbol<TypedAST>> {
        Rc::new(FunctionSymbol::new(
            name.to_string(),
            return_type,
            vec![Rc::new(VariableSymbol::new(
                "p1".to_string(),
                DataType::S32,
            ))],
        ))
    }

    #[test]
    fn new_mapper_is_empty() {
        let mapper = FileSymbolMapper::new();
        assert!(
            mapper.lookup_function("any_func").is_none(),
            "Newly created mapper should contain no functions."
        );
    }

    #[test]
    fn add_and_lookup_function_ok() {
        let mut mapper = FileSymbolMapper::new();
        let func_name = "test_func_s32";

        let symbol = create_test_function_symbol(func_name, Some(DataType::S32));

        let result = mapper.add_function_to_file(symbol.clone());
        assert!(result.is_ok(), "Adding the function should succeed.");

        let found_symbol = mapper.lookup_function(func_name);
        assert!(
            found_symbol.is_some(),
            "Lookup should find the added function."
        );

        let found_symbol = found_symbol.unwrap();
        assert_eq!(
            found_symbol.name(),
            func_name,
            "Found function name must match."
        );
        assert_eq!(
            found_symbol.return_type(),
            Some(&DataType::S32),
            "Found function type must match."
        );
    }

    #[test]
    fn add_duplicate_function_errors() {
        let mut mapper = FileSymbolMapper::new();
        let func_name = "duplicate_func";

        let symbol1 = create_test_function_symbol(func_name, Some(DataType::S32));
        let symbol2 = create_test_function_symbol(func_name, Some(DataType::F64));

        assert!(
            mapper.add_function_to_file(symbol1).is_ok(),
            "First function definition should succeed."
        );

        let result = mapper.add_function_to_file(symbol2);
        assert!(
            result.is_err(),
            "Adding a function with the same name must return an error."
        );

        let error_msg = result.err().unwrap();
        assert!(
            error_msg.contains("already defined"),
            "Error message should indicate a duplicate definition."
        );
    }

    #[test]
    fn lookup_non_existent_function_returns_none() {
        let mapper = FileSymbolMapper::new();

        assert!(
            mapper.lookup_function("non_existent").is_none(),
            "Lookup for non-existent function must return None."
        );
    }
}
