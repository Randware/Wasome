use ast::TypedAST;
use ast::symbol::{FunctionSymbol, Symbol};
use std::collections::HashMap;
use std::rc::Rc;

/// Type alias for the global registry of all functions in the program.
/// Keys are canonical IDs (e.g., "std::math::cos").
pub type GlobalFunctionMap = HashMap<String, Rc<FunctionSymbol<TypedAST>>>;

/// Defines the interface for accessing file-specific information.
/// This trait abstracts the `FileTraversalHelper`, allowing the `FileSymbolMapper`
/// to resolve imports and determine the current package path.
pub trait FileContext {
    /// Returns the canonical path of the current file/package (e.g., "my_project::utils").
    fn get_canonical_path(&self) -> &str;

    /// Resolves an import alias to its full module path.
    /// Example: If "import std::math as m" exists, resolving "m" returns Some("std::math").
    fn resolve_import(&self, alias: &str) -> Option<String>;
}

/// Acts as a bridge between the local file scope and global function definitions.
pub struct FileSymbolMapper<'a> {
    global_functions: &'a GlobalFunctionMap,
    file_context: &'a dyn FileContext,
}

impl<'a> FileSymbolMapper<'a> {
    /// Creates a new `FileSymbolMapper`.
    pub fn new(global_functions: &'a GlobalFunctionMap, file_context: &'a dyn FileContext) -> Self {
        Self {
            global_functions,
            file_context,
        }
    }

    /// Looks up a function symbol based on its name within the code.
    ///
    /// Priority:
    /// 1. **Module Imports**: "math.cos" -> resolves "math" alias.
    /// 2. **Local Definition**: "my_func" -> checks "current_package::my_func".
    /// 3. **Global/Absolute**: "print" -> checks global "print".
    pub fn lookup_function(&self, name: &str) -> Option<Symbol<'a, TypedAST>> {
        if let Some((module_alias, func_name)) = name.split_once('.') {
            return self.resolve_imported_function(module_alias, func_name);
        }

        let local_id = self.build_canonical_id(self.file_context.get_canonical_path(), name);

        if let Some(func) = self.global_functions.get(&local_id) {
            return Some(Symbol::Function(func.as_ref()));
        }

        if let Some(func) = self.global_functions.get(name) {
            return Some(Symbol::Function(func.as_ref()));
        }

        None
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
    pub fn lookup_function_rc(&self, name: &str) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        if let Some((module_alias, func_name)) = name.split_once('.') {
            let module_path = self.file_context.resolve_import(module_alias)?;
            let canonical_id = self.build_canonical_id(&module_path, func_name);
            return self.global_functions.get(&canonical_id).cloned();
        }

        let local_id = self.build_canonical_id(self.file_context.get_canonical_path(), name);
        if let Some(func) = self.global_functions.get(&local_id) {
            return Some(func.clone());
        }

        if let Some(func) = self.global_functions.get(name) {
            return Some(func.clone());
        }

        None
    }

    /// Helper method to resolve functions accessed via import aliases.
    fn resolve_imported_function(
        &self,
        module_alias: &str,
        func_name: &str,
    ) -> Option<Symbol<'a, TypedAST>> {
        let module_path = self.file_context.resolve_import(module_alias)?;
        let canonical_id = self.build_canonical_id(&module_path, func_name);

        self.global_functions
            .get(&canonical_id)
            .map(|f| Symbol::Function(f.as_ref()))
    }

    /// Constructs the canonical ID for a function.
    fn build_canonical_id(&self, path: &str, name: &str) -> String {
        if path.is_empty() {
            name.to_string()
        } else {
            format!("{}::{}", path, name)
        }
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

    impl FileContext for MockFileContext {
        fn get_canonical_path(&self) -> &str {
            &self.current_path
        }

        fn resolve_import(&self, alias: &str) -> Option<String> {
            self.imports.get(alias).cloned()
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
        ))
    }

    #[test]
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
    }
}
