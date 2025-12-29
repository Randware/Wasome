use crate::file_symbol_mapper::GlobalFunctionMap;
use crate::mics_sa::analyze_data_type;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::TopLevelElement;
use ast::{AST, TypedAST, UntypedAST};
use std::rc::Rc;

/// Collects all function symbols from a list of parsed modules.
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
}

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
