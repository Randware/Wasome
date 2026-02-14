use crate::error_sa::SemanticError;
use crate::expression_sa::analyze_expression;
use crate::symbol::SyntaxContext;
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use crate::symbol_by_name;
use ast::data_type::{DataType, Typed, UntypedDataType};
use ast::expression::{Expression, FunctionCall, MethodCall};
use ast::symbol::{
    DirectlyAvailableSymbol, EnumSymbol, FunctionSymbol, StructSymbol, SymbolWithTypeParameter,
};
use ast::traversal::HasSymbols;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::type_parameter::{TypedTypeParameter, UntypedTypeParameter};
use ast::{ASTNode, TypedAST, UntypedAST};
use source::types::FileID;
use std::rc::Rc;

/// A helper function that resolves the type names into the right types.
///
/// # Resolution Order
/// 1.  **Primitives**: Checks if the name matches a primitive type (e.g., `s32`, `bool`).
/// 2.  **Type Parameters**: Checks if the name matches a generic type parameter in the current context.
/// 3.  **Structs**: Checks if the name corresponds to a known struct.
/// 4.  **Enums**: Checks if the name corresponds to a known enum.
///
/// # Parameters
/// * `to_analyze` - The untyped data type to analyze (`&UntypedDataType`).
/// * `context` - The syntax context containing available symbols and type parameters.
///
/// # Returns
/// * `Ok(DataType)` if the string matches a known type.
/// * `Err(SemanticError)` otherwise.
pub(crate) fn analyze_data_type<'a, T: Clone + HasSymbols<'a, UntypedAST>>(
    to_analyze: &UntypedDataType,
    context: &SyntaxContext<&T>,
) -> Result<DataType, SemanticError> {
    let resolved_type = analyze_primitive_data_type(to_analyze)
        .or_else(|| {
            analyze_type_parameter(to_analyze.name(), context).map(|tp| tp.data_type().clone())
        })
        .or_else(|| {
            analyze_struct_usage(to_analyze.name(), to_analyze.type_parameters(), context)
                .ok()
                .map(DataType::Struct)
        })
        .or_else(|| {
            analyze_enum_usage(to_analyze.name(), to_analyze.type_parameters(), context)
                .ok()
                .map(DataType::Enum)
        });

    resolved_type.ok_or_else(|| SemanticError::UnknownType {
        name: to_analyze.name().to_string(),
        span: FileID::from(0).span(0, 0),
    })
}

fn analyze_primitive_data_type(to_analyze: &UntypedDataType) -> Option<DataType> {
    if !to_analyze.type_parameters().is_empty() {
        return None;
    }
    Some(match to_analyze.name() {
        "char" => DataType::Char,
        "u8" => DataType::U8,
        "s8" => DataType::S8,
        "u16" => DataType::U16,
        "s16" => DataType::S16,
        "u32" => DataType::U32,
        "s32" => DataType::S32,
        "u64" => DataType::U64,
        "s64" => DataType::S64,
        "bool" => DataType::Bool,
        "f32" => DataType::F32,
        "f64" => DataType::F64,
        _ => return None,
    })
}

pub(crate) fn analyze_struct_usage<'a, T: Clone + HasSymbols<'a, UntypedAST>>(
    to_analyze: &str,
    type_parameters: &[UntypedDataType],
    context: &SyntaxContext<&T>,
) -> Result<Rc<StructSymbol<TypedAST>>, SemanticError> {
    let untyped_symbol =
        symbol_by_name(to_analyze, context.ast_reference.symbols()).ok_or_else(|| {
            SemanticError::UnknownSymbol {
                name: to_analyze.to_string(),
                span: FileID::from(0).span(0, 0),
            }
        })?;

    let untyped_symbol = match untyped_symbol {
        DirectlyAvailableSymbol::Struct(st) => st,
        _ => {
            return Err(SemanticError::Custom {
                message: format!("'{}' is not a struct", to_analyze),
                span: FileID::from(0).span(0, 0),
            });
        }
    };

    let type_parameters = analyze_type_parameters_providing(
        untyped_symbol.type_parameters(),
        type_parameters,
        context,
    )?;

    context
        .global_elements
        .get_typed_struct_symbol(Rc::new(untyped_symbol.clone()), &type_parameters)
        .ok_or_else(|| SemanticError::Custom {
            message: "Critical: Struct found in AST but missing in global element map.".to_string(),
            span: FileID::from(0).span(0, 0),
        })
}

pub(crate) fn analyze_struct_usage_from_typed_type_parameters<
    'a,
    T: Clone + HasSymbols<'a, UntypedAST>,
>(
    to_analyze: &str,
    type_parameters: &[TypedTypeParameter],
    context: &SyntaxContext<&T>,
) -> Option<Rc<StructSymbol<TypedAST>>> {
    let untyped_symbol = symbol_by_name(to_analyze, context.ast_reference.symbols())?;
    let untyped_symbol = match untyped_symbol {
        DirectlyAvailableSymbol::Struct(st) => st,
        _ => return None,
    };
    context
        .global_elements
        .get_typed_struct_symbol(Rc::new(untyped_symbol.clone()), type_parameters)
}

pub(crate) fn analyze_enum_usage<'a, T: Clone + HasSymbols<'a, UntypedAST>>(
    to_analyze: &str,
    type_parameters: &[UntypedDataType],
    context: &SyntaxContext<&T>,
) -> Result<Rc<EnumSymbol<TypedAST>>, SemanticError> {
    let untyped_symbol =
        symbol_by_name(to_analyze, context.ast_reference.symbols()).ok_or_else(|| {
            SemanticError::UnknownSymbol {
                name: to_analyze.to_string(),
                span: FileID::from(0).span(0, 0),
            }
        })?;

    let untyped_symbol = match untyped_symbol {
        DirectlyAvailableSymbol::Enum(st) => st,
        _ => {
            return Err(SemanticError::Custom {
                message: format!("'{}' is not an enum", to_analyze),
                span: FileID::from(0).span(0, 0),
            });
        }
    };

    let type_parameters = analyze_type_parameters_providing(
        untyped_symbol.type_parameters(),
        type_parameters,
        context,
    )?;

    context
        .global_elements
        .get_typed_enum_symbol(Rc::new(untyped_symbol.clone()), &type_parameters)
        .ok_or_else(|| SemanticError::Custom {
            message: "Critical: Enum found in AST but missing in global element map.".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        })
}

pub(crate) fn analyze_type_parameter_full<'a, T: Clone>(
    to_analyze: &UntypedTypeParameter,
    context: &'a SyntaxContext<&T>,
) -> Option<&'a TypedTypeParameter> {
    analyze_type_parameter(to_analyze.inner().name(), context)
}

pub(crate) fn analyze_type_parameter<'a, T: Clone>(
    to_analyze: &str,
    context: &'a SyntaxContext<&T>,
) -> Option<&'a TypedTypeParameter> {
    context
        .type_parameter_context
        .lookup_typed_type_parameter(to_analyze)
}

pub(crate) fn analyze_type_parameters_declaration<'a, T: Clone + HasSymbols<'a, UntypedAST>>(
    context: &SyntaxContext<&T>,
    to_analyze: impl Iterator<Item = &'a UntypedTypeParameter>,
) -> Result<Vec<TypedTypeParameter>, String> {
    to_analyze
        .map(|tp| analyze_type_parameter_full(tp, context).cloned())
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| "Unknown type parameter".to_string())
}

pub(crate) fn analyze_type_parameter_providing<'a, T: Clone + HasSymbols<'a, UntypedAST>>(
    to_analyze: &UntypedTypeParameter,
    with: &UntypedDataType,
    context: &SyntaxContext<&T>,
) -> Result<TypedTypeParameter, SemanticError> {
    Ok(TypedTypeParameter::new(
        to_analyze.inner().name().to_owned(),
        analyze_data_type(with, context)?,
    ))
}

pub(crate) fn analyze_type_parameters_providing<'a, T: Clone + HasSymbols<'a, UntypedAST>>(
    type_parameters: &[UntypedTypeParameter],
    fillings: &[UntypedDataType],
    context: &SyntaxContext<&T>,
) -> Result<Vec<TypedTypeParameter>, SemanticError> {
    fillings
        .iter()
        .zip(type_parameters.iter())
        .map(|(filling, param)| analyze_type_parameter_providing(param, filling, context))
        .collect::<Result<Vec<_>, _>>()
}

/// Analyzes a function call
///
/// Does not care if the function call is void or not
///
/// # Panics
/// Panics if the function symbol exists in the AST but is missing from the global element map.
/// This indicates a failure in the "Stage 2" symbol collection pass.
pub(crate) fn analyze_function_call(
    to_analyze: &FunctionCall<UntypedAST>,
    mapper: &mut FunctionSymbolMapper,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
) -> Result<FunctionCall<TypedAST>, SemanticError> {
    let call_name = to_analyze.function();
    let name = &call_name.0;

    let untyped_func_symbol =
        analyze_function_usage(context, name).ok_or_else(|| SemanticError::UnknownSymbol {
            name: name.clone(),
            span: FileID::from(0).span(0, 0), // Placeholder span
        })?;

    let type_parameters = analyze_type_parameters_providing(
        untyped_func_symbol.type_parameters(),
        &to_analyze.function().1,
        context,
    )?;

    let typed_func_symbol = context
        .global_elements
        .get_or_insert_typed_function_symbol(Rc::new(untyped_func_symbol.clone()), &type_parameters)
        .expect("Critical: Symbol found in AST but missing in map. Stage 2 failed?");

    let mut typed_args: Vec<ASTNode<Expression<TypedAST>>> = Vec::new();
    for untyped_arg_node in to_analyze.args().iter() {
        let position = *untyped_arg_node.position();

        let typed_expr =
            analyze_expression(untyped_arg_node, context, mapper).ok_or_else(|| {
                SemanticError::Custom {
                    message: "Failed to analyze argument expression".to_string(),
                    span: position,
                }
            })?;

        typed_args.push(ASTNode::new(typed_expr, position));
    }

    FunctionCall::<TypedAST>::new(typed_func_symbol, typed_args).ok_or_else(|| {
        SemanticError::Custom {
            message: format!("Arguments do not match signature of function '{}'", name),
            span: FileID::from(0).span(0, 0),
        }
    })
}

fn analyze_function_usage<'a>(
    context: &SyntaxContext<&'a StatementTraversalHelper<UntypedAST>>,
    name: &str,
) -> Option<&'a FunctionSymbol<UntypedAST>> {
    let found_symbol = symbol_by_name(name, context.ast_reference.symbols_available_at())?;

    let untyped_func_symbol = match found_symbol {
        DirectlyAvailableSymbol::Function(f) => f,
        _ => return None,
    };
    Some(untyped_func_symbol)
}

pub(crate) fn analyze_method_call(
    to_analyze: &MethodCall,
    mapper: &mut FunctionSymbolMapper,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
) -> Result<FunctionCall<TypedAST>, SemanticError> {
    let struct_expr =
        analyze_expression(to_analyze.struct_source(), context, mapper).ok_or_else(|| {
            SemanticError::Custom {
                message: "Failed to analyze struct source of method call".to_string(),
                span: *to_analyze.struct_source().position(),
            }
        })?;

    let untyped_function_symbol = symbol_by_name(
        &to_analyze.function().0,
        context.ast_reference.symbols_available_at(),
    )
    .ok_or_else(|| SemanticError::UnknownSymbol {
        name: to_analyze.function().0.clone(),
        span: FileID::from(0).span(0, 0),
    })?;

    let function_symbol = if let DirectlyAvailableSymbol::Function(func) = untyped_function_symbol {
        func
    } else {
        return Err(SemanticError::Custom {
            message: format!("'{}' is not a function", to_analyze.function().0),
            span: FileID::from(0).span(0, 0),
        });
    };

    let struct_symbol = if let DataType::Struct(st) = struct_expr.data_type() {
        st
    } else {
        return Err(SemanticError::TypeMismatch {
            expected: "Struct".to_string(),
            found: format!("{:?}", struct_expr.data_type()),
            span: *to_analyze.struct_source().position(),
        });
    };

    let untyped_struct_symbol = context
        .global_elements
        .untyped_struct_symbol_from_typed(&struct_symbol)
        .ok_or_else(|| SemanticError::Custom {
            message: "Internal Error: Could not find untyped struct symbol".to_string(),
            span: FileID::from(0).span(0, 0),
        })?;

    let typed_type_parameters = analyze_type_parameters_providing(
        untyped_struct_symbol.type_parameters(),
        &to_analyze.function().1,
        context,
    )?;

    let function_symbol = context
        .global_elements
        .get_typed_method_symbol(
            &untyped_struct_symbol,
            struct_symbol.type_parameters(),
            Rc::new(function_symbol.clone()),
            &typed_type_parameters,
        )
        .ok_or_else(|| SemanticError::Custom {
            message: format!("Method '{}' not found on struct", to_analyze.function().0),
            span: FileID::from(0).span(0, 0),
        })?;

    let mut args = vec![ASTNode::new(
        struct_expr,
        *to_analyze.struct_source().position(),
    )];

    for param in to_analyze.args().iter() {
        let typed_param =
            analyze_expression(param, context, mapper).ok_or_else(|| SemanticError::Custom {
                message: "Failed to analyze method argument".to_string(),
                span: *param.position(),
            })?;
        args.push(ASTNode::new(typed_param, *param.position()));
    }

    FunctionCall::<TypedAST>::new(function_symbol, args).ok_or_else(|| SemanticError::Custom {
        message: "Arguments do not match method signature".to_string(),
        span: FileID::from(0).span(0, 0),
    })
}
