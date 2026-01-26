/*use crate::symbol_translation::global_system_collector::GlobalSymbolMap;
use ast::symbol::{FunctionSymbol, DirectlyAvailableSymbol};
use ast::{TypedAST, UntypedAST};
use std::rc::Rc;

/// Acts as a bridge between the local file scope and global function definitions.
pub struct FileSymbolMapper<'a, 'b> {
    global_functions: &'a GlobalSymbolMap<'b>,
}

impl<'a, 'b> FileSymbolMapper<'a, 'b> {
    /// Creates a new `FileSymbolMapper`.
    pub fn new(global_functions: &'a GlobalSymbolMap<'b>) -> Self {
        Self { global_functions }
    }

    /// Looks up a function symbol based on its name within the code.
    ///
    /// Priority:
    /// 1. **Module Imports**: "math.cos" -> resolves "math" alias.
    /// 2. **Local Definition**: "my_func" -> checks "current_package::my_func".
    /// 3. **Global/Absolute**: "print" -> checks global "print".
    pub fn lookup_function(
        &self,
        function: &FunctionSymbol<UntypedAST>,
    ) -> Option<DirectlyAvailableSymbol<'a, TypedAST>> {
        self.global_functions
            .get(function)
            .map(|res| DirectlyAvailableSymbol::Function(res.as_ref()))
    }

    /// Looks up a function symbol by its name and returns a shared pointer (`Rc`) to it.
    ///
    /// Unlike `lookup_function` (which might return a generic `Symbol` reference), this method
    /// specifically provides ownership of the `Rc`. This is required during AST construction
    /// (e.g., creating a `FunctionCall` node), as the AST node needs to hold a reference
    /// to the function symbol to ensure it remains valid.
    ///
    /// Priority:
    /// 1. **Module Imports**: "math.cos" -> resolves "math" alias.
    /// 2. **Local Definition**: "my_func" -> checks "current_package::my_func".
    /// 3. **Global/Absolute**: "print" -> checks global "print".
    pub fn lookup_function_rc(
        &self,
        function: &FunctionSymbol<UntypedAST>,
    ) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        self.global_functions.get(function).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::data_type::DataType;
    use ast::symbol::VariableSymbol;
    use std::collections::HashMap;
    use std::rc::Rc;

    struct MockFileContext {
        current_path: String,
        imports: HashMap<String, String>,
    }

    impl MockFileContext {
        fn new(path: &str) -> Self {
            Self {
                current_path: path.to_string(),
                imports: HashMap::new(),
            }
        }

        fn add_import(&mut self, alias: &str, real_path: &str) {
            self.imports
                .insert(alias.to_string(), real_path.to_string());
        }
    }

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
            Vec::new()
        ))
    }

    /*#[test]
    fn lookup_local_function_ok() {
        let func_name = "test_func";
        let pkg_name = "my_pkg";
        let full_id = format!("{}::{}", pkg_name, func_name);

        let mut global_map = GlobalFunctionMap::new();
        let symbol = create_test_function_symbol(func_name, Some(DataType::S32));
        global_map.insert(full_id, symbol);

        let context = MockFileContext::new(pkg_name);
        let mapper = FileSymbolMapper::new(&global_map, &context);

        let found = mapper.lookup_function(func_name);

        assert!(
            found.is_some(),
            "Should find local function via package path"
        );

        if let Some(Symbol::Function(f)) = found {
            assert_eq!(f.name(), func_name);
            assert_eq!(f.return_type(), Some(&DataType::S32));
        } else {
            panic!("Expected Symbol::Function");
        }
    }

    #[test]
    fn lookup_imported_module_function_ok() {
        let mut global_map = GlobalFunctionMap::new();
        let cos_symbol = create_test_function_symbol("cos", Some(DataType::F64));
        global_map.insert("std::math::cos".to_string(), cos_symbol);

        let mut context = MockFileContext::new("user_code");
        context.add_import("m", "std::math");

        let mapper = FileSymbolMapper::new(&global_map, &context);

        let found = mapper.lookup_function("m.cos");

        assert!(found.is_some());
        if let Some(Symbol::Function(f)) = found {
            assert_eq!(f.name(), "cos");
            assert_eq!(f.return_type(), Some(&DataType::F64));
        }
    }

    #[test]
    fn lookup_absolute_global_function_ok() {
        let mut global_map = GlobalFunctionMap::new();
        let print_symbol = create_test_function_symbol("print", None);
        global_map.insert("print".to_string(), print_symbol);

        let context = MockFileContext::new("deep::nested::pkg");
        let mapper = FileSymbolMapper::new(&global_map, &context);

        let found = mapper.lookup_function("print");

        assert!(found.is_some());
        if let Some(Symbol::Function(f)) = found {
            assert_eq!(f.name(), "print");
            assert!(f.return_type().is_none());
        }
    }

    #[test]
    fn lookup_fails_for_missing_function() {
        let global_map = GlobalFunctionMap::new();
        let context = MockFileContext::new("my_pkg");
        let mapper = FileSymbolMapper::new(&global_map, &context);

        assert!(mapper.lookup_function("non_existent").is_none());
    }

    #[test]
    fn lookup_fails_for_missing_import_alias() {
        let mut global_map = GlobalFunctionMap::new();
        global_map.insert(
            "std::math::cos".to_string(),
            create_test_function_symbol("cos", None),
        );

        let context = MockFileContext::new("my_pkg"); // No imports
        let mapper = FileSymbolMapper::new(&global_map, &context);

        assert!(mapper.lookup_function("m.cos").is_none());
    }*/
}*/
