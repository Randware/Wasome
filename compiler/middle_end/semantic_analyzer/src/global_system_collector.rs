use crate::file_symbol_mapper::GlobalFunctionMap;
use crate::mics_sa::analyze_data_type;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::{AST, TypedAST, UntypedAST};
use std::rc::Rc;

// The new ast makes the current implementation non-functional
/*/// Collects all function symbols from a list of parsed modules.
///
/// # Parameters
/// * `modules` - An iterator yielding pairs of `(Canonical Module Path, The AST)`.
///               Example tuple: ("std::math", &AST<UntypedAST>)
///
/// # Returns
/// * `Ok(GlobalFunctionMap)` containing all discoverable functions.
/// * `Err(String)` on duplicate definitions or invalid types.
pub fn collect_global_symbols<'a, I>(modules: I) -> Result<GlobalFunctionMap, String>
where
    I: IntoIterator<Item = (String, &'a AST<UntypedAST>)>,
{
    let mut map = GlobalFunctionMap::new();

    for (module_path, ast) in modules {
        for node in ast.iter() {
            if let TopLevelElement::Function(func) = &**node {
                let untyped_symbol = func.declaration();

                let typed_symbol = convert_function_symbol(untyped_symbol)?;

                let canonical_id = format!("{}::{}", module_path, typed_symbol.name());

                if map.contains_key(&canonical_id) {
                    return Err(format!(
                        "Semantic Error: Duplicate function definition '{}'.",
                        canonical_id
                    ));
                }

                map.insert(canonical_id, typed_symbol);
            }
        }
    }

    Ok(map)
}*/

/// Converts an untyped function symbol (strings) into a typed symbol (enums).
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
    use ast::data_type::DataType;
    use ast::{ASTNode, AST};
    use crate::expression_sa::sample_codearea;

    /// Creates a dummy AST containing a single function declaration with an empty body.
    /// This is sufficient for testing Stage 2, as it only reads the function signature.
    fn create_dummy_ast(func_name: &str, ret_type: Option<&str>) -> AST<UntypedAST> {
        todo!()
        /*let func_symbol = Rc::new(FunctionSymbol::new(
            func_name.to_string(),
            ret_type.map(|s| s.to_string()),
            Vec::new(),
        ));

        use ast::statement::Statement;
        use ast::block::CodeBlock;
        use ast::top_level::Function;

        let impl_block = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(Vec::new())),
            sample_codearea()
        );

        let func = Function::new(func_symbol, impl_block);

        AST::new(vec![
            ASTNode::new(TopLevelElement::Function(func), sample_codearea())
        ])*/
    }

    /// Tests the "happy path" where functions from different modules are collected successfully.
    ///
    /// Scenario:
    /// - Module "std::math" defines function "add".
    /// - Module "app::main" defines function "start".
    ///
    /// Expected Result:
    /// - The returned map contains keys "std::math::add" and "app::main::start".
    /// - The return types are correctly converted from strings to `DataType` enums.
    #[test]
    fn collect_symbols_from_multiple_modules() {
        todo!()
        /*let ast_math = create_dummy_ast("add", Some("s32"));
        let ast_main = create_dummy_ast("start", None);

        let modules = vec![
            ("std::math".to_string(), &ast_math),
            ("app::main".to_string(), &ast_main),
        ];

        let result = collect_global_symbols(modules);

        assert!(result.is_ok(), "Should successfully collect symbols from valid modules");
        let map = result.unwrap();

        assert!(map.contains_key("std::math::add"));
        let add_func = map.get("std::math::add").unwrap();
        assert_eq!(add_func.return_type(), Some(&DataType::S32));

        assert!(map.contains_key("app::main::start"));
        let start_func = map.get("app::main::start").unwrap();
        assert!(start_func.return_type().is_none());*/
    }

    /// Tests that the collector correctly identifies and rejects duplicate function definitions.
    ///
    /// Scenario:
    /// - Two different ASTs are provided for the *same* module path ("pkg::file").
    /// - Both define a function named "dup".
    ///
    /// Expected Result:
    /// - The collector returns an `Err` because "pkg::file::dup" would be overwritten.
    #[test]
    fn collect_symbols_detects_duplicates() {
        todo!()
        /*let ast1 = create_dummy_ast("dup", None);
        let ast2 = create_dummy_ast("dup", Some("s32"));

        let modules = vec![
            ("pkg::file".to_string(), &ast1),
            ("pkg::file".to_string(), &ast2), 
        ];

        let result = collect_global_symbols(modules);

        assert!(result.is_err(), "Should fail due to duplicate symbol definition");
        let err = result.unwrap_err();
        assert!(err.contains("Duplicate function definition"));
        assert!(err.contains("pkg::file::dup"));*/
    }
}