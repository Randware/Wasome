use crate::mics_sa::analyze_data_type;
use ast::directory::Directory;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::{AST, TypedAST, UntypedAST};
use std::collections::HashMap;
use std::rc::Rc;

/// The global symbol map.
///
/// It maps an `UntypedAST` symbol (used as the key) to its corresponding `TypedAST` symbol (the value).
/// We use the symbol itself as the key. Thanks to our custom `Hash` implementation (which only hashes the ID),
/// this is very efficient.
pub type GlobalSymbolMap<'a> = HashMap<&'a FunctionSymbol<UntypedAST>, Rc<FunctionSymbol<TypedAST>>>;

/// Entry Point: Collects all global symbols from the AST.
///
/// This function initiates the traversal starting from the root directory of the AST.
/// It collects all function signatures, converts them to their typed counterparts,
/// and stores them in a global map for later retrieval during semantic analysis.
///
/// # Parameters
/// * `ast` - The entire Untyped Abstract Syntax Tree.
///
/// # Returns
/// * `Ok(GlobalSymbolMap)` - The populated map of symbols.
/// * `Err(String)` - If a semantic error occurs during type conversion (e.g., unknown types).
pub fn collect_global_symbols(ast: &AST<UntypedAST>) -> Result<GlobalSymbolMap, String> {
    let mut map = GlobalSymbolMap::new();
    collect_from_directory(ast, &mut map)?;
    Ok(map)
}

/// Recursive helper function that traverses directories and files.
///
/// It iterates through all files in the current directory to register their functions
/// and then recursively calls itself for all subdirectories.
///
/// # Parameters
/// * `dir` - The current directory to traverse.
/// * `map` - The mutable reference to the global symbol map being populated.
fn collect_from_directory<'a>(
    dir: &'a Directory<UntypedAST>,
    map: &mut GlobalSymbolMap<'a>,
) -> Result<(), String> {
    for file in dir.files_iterator() {
        for function in file.functions() {
            let untyped_symbol = function.declaration();

            let typed_symbol = convert_function_symbol(untyped_symbol)?;

            map.insert(untyped_symbol, typed_symbol);
        }
    }

    for subdir in dir.subdirectories_iterator() {
        collect_from_directory(subdir, map)?;
    }

    Ok(())
}

/// Converts an untyped function symbol (with String types) into a typed symbol (with Enum types).
///
/// This function validates that the types used in the return type and parameters actually exist
/// and converts them from their string representation to `DataType`.
///
/// # Parameters
/// * `untyped` - The untyped function symbol from the parser.
///
/// # Returns
/// * `Ok(Rc<FunctionSymbol<TypedAST>>)` - The newly created typed symbol wrapped in an Rc.
/// * `Err(String)` - If a type name cannot be resolved.
fn convert_function_symbol(
    untyped: &FunctionSymbol<UntypedAST>,
) -> Result<Rc<FunctionSymbol<TypedAST>>, String> {
    let return_type = match untyped.return_type() {
        Some(type_name) => {
            let dt = analyze_data_type(type_name).ok_or_else(|| {
                format!(
                    "Semantic Error: Unknown return type '{}' in function '{}'",
                    type_name,
                    untyped.name()
                )
            })?;
            Some(dt)
        }
        None => None,
    };

    let mut typed_params = Vec::new();
    for param in untyped.params() {
        let param_type_name = param.data_type();
        let dt = analyze_data_type(param_type_name).ok_or_else(|| {
            format!(
                "Semantic Error: Unknown parameter type '{}' for parameter '{}' in function '{}'",
                param_type_name,
                param.name(),
                untyped.name()
            )
        })?;

        let typed_param = Rc::new(VariableSymbol::new(param.name().to_string(), dt));
        typed_params.push(typed_param);
    }

    Ok(Rc::new(FunctionSymbol::new(
        untyped.name().to_string(),
        return_type,
        typed_params,
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression_sa::sample_codearea;
    use ast::data_type::DataType;
    use ast::directory::Directory;
    use ast::file::File;
    use ast::statement::{CodeBlock, Statement};
    use ast::top_level::Function;
    use ast::visibility::Visibility;
    use ast::{AST, ASTNode};
    use std::path::PathBuf;

    /// Creates an untyped function node and its corresponding symbol.
    ///
    /// Returns:
    /// - The `FunctionSymbol` (to use as a key for lookup verification).
    /// - The `ASTNode<Function>` (to build the AST).
    fn create_untyped_func(
        name: &str,
        ret_type: Option<&str>,
        params: Vec<(&str, &str)>,
    ) -> (
        Rc<FunctionSymbol<UntypedAST>>,
        ASTNode<Function<UntypedAST>>,
    ) {
        let mut built_params = Vec::new();
        for (p_name, p_type) in params {
            built_params.push(Rc::new(VariableSymbol::new(
                p_name.to_string(),
                p_type.to_string(),
            )));
        }

        let symbol = Rc::new(FunctionSymbol::new(
            name.to_string(),
            ret_type.map(|s| s.to_string()),
            built_params,
        ));

        let implementation = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(Vec::new())),
            sample_codearea(),
        );

        let func_node = ASTNode::new(
            Function::new(symbol.clone(), implementation, Visibility::Public),
            sample_codearea(),
        );

        (symbol, func_node)
    }

    /// Wraps a list of functions into a valid minimal AST structure.
    /// Creates a structure like: src/ (Directory) -> main.wa (File) -> [functions]
    fn create_simple_ast(functions: Vec<ASTNode<Function<UntypedAST>>>) -> AST<UntypedAST> {
        let file = ASTNode::new(
            File::new("main.wa".to_string(), Vec::new(), functions),
            PathBuf::from("src/main.wa"),
        );

        let dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::from("src"),
        );

        AST::new(dir).expect("Failed to create AST from dummy nodes")
    }

    /// Tests the "happy path" where valid functions are successfully collected.
    /// It verifies that the collector correctly converts untyped strings (e.g., "s32")
    /// into their corresponding typed enums (e.g., DataType::S32) and stores them in the map.
    #[test]
    fn collect_symbols_successfully() {
        let (sym_add, node_add) =
            create_untyped_func("add", Some("s32"), vec![("a", "s32"), ("b", "s32")]);

        let (sym_start, node_start) = create_untyped_func("start", None, vec![]);

        let ast = create_simple_ast(vec![node_add, node_start]);

        let result = collect_global_symbols(&ast);

        assert!(result.is_ok(), "Symbol collection should succeed");
        let map = result.unwrap();

        assert!(
            map.contains_key(sym_add.as_ref()),
            "Map should contain 'add' symbol"
        );
        let typed_add = map.get(sym_add.as_ref()).unwrap();

        assert_eq!(typed_add.name(), "add");
        assert_eq!(typed_add.return_type(), Some(&DataType::S32));
        assert_eq!(typed_add.params().len(), 2);
        assert_eq!(*typed_add.params()[0].data_type(), DataType::S32);

        assert!(
            map.contains_key(sym_start.as_ref()),
            "Map should contain 'start' symbol"
        );
        let typed_start = map.get(sym_start.as_ref()).unwrap();
        assert_eq!(typed_start.return_type(), None);
    }

    /// Tests that the collector properly validates data types during collection.
    /// It ensures that if a function signature uses a non-existent type (e.g., "non_existent_type"),
    /// the collector returns an error instead of creating an invalid symbol.
    #[test]
    fn collect_symbols_fails_on_invalid_type() {
        let (_, node_bad) = create_untyped_func("bad", Some("non_existent_type"), vec![]);

        let ast = create_simple_ast(vec![node_bad]);

        let result = collect_global_symbols(&ast);

        assert!(
            result.is_err(),
            "Collection should fail due to invalid type"
        );
        let err = result.unwrap_err();
        assert!(err.contains("Unknown return type"));
        assert!(err.contains("non_existent_type"));
    }
}
