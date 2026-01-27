use crate::expression_sa::analyze_expression;
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use crate::symbol::{SyntaxContext, TypeParameterContext};
use crate::symbol_by_name;
use ast::data_type::{DataType, UntypedDataType};
use ast::expression::{Expression, FunctionCall};
use ast::symbol::{DirectlyAvailableSymbol, FunctionSymbol, SymbolWithTypeParameter};
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::type_parameter::{TypedTypeParameter, UntypedTypeParameter};
use ast::{ASTNode, TypedAST, UntypedAST};

/// A helper function that resolves the type names into the right types.
///
/// # Parameters
/// * `to_analyze` - The string representation of the data type (e.g., "s32", "bool").
///
/// # Returns
/// * `Some(DataType)` if the string matches a known type.
/// * `None` otherwise.
pub(crate) fn analyze_data_type<T: Clone>(
    to_analyze: &UntypedDataType,
    context: &SyntaxContext<T>,
) -> Option<DataType> {
    // TODO: Add Composites
    analyze_primitive_data_type(to_analyze).or_else(|| {
        analyze_type_parameter(to_analyze.name(), context).map(|tp| tp.data_type().clone())
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

pub(crate) fn analyze_type_parameter_full<'a, T: Clone>(
    to_analyze: &UntypedTypeParameter,
    context: &'a SyntaxContext<T>,
) -> Option<&'a TypedTypeParameter> {
    analyze_type_parameter(to_analyze.inner().name(), context)
}

pub(crate) fn analyze_type_parameter<'a, T: Clone>(
    to_analyze: &str,
    context: &'a SyntaxContext<T>,
) -> Option<&'a TypedTypeParameter> {
    context
        .type_parameter_context
        .lookup_typed_type_parameter(to_analyze)
}

pub(crate) fn analyze_type_parameters_declaration<'a>(
    context: &SyntaxContext<impl Clone>,
    to_analyze: impl Iterator<Item = &'a UntypedTypeParameter>,
) -> Result<Vec<TypedTypeParameter>, String> {
    to_analyze
        .map(|tp| analyze_type_parameter_full(tp, context).cloned())
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| "Unknown type parameter".to_string())
}

pub(crate) fn analyze_type_parameter_providing<T: Clone>(
    to_analyze: &UntypedTypeParameter,
    with: &UntypedDataType,
    context: &SyntaxContext<T>,
) -> Option<TypedTypeParameter> {
    Some(TypedTypeParameter::new(
        to_analyze.inner().name().to_owned(),
        analyze_data_type(with, context)?,
    ))
}

fn analyze_type_parameters_providing<ASTReference: Clone>(
    type_parameters: &[UntypedTypeParameter],
    fillings: &[UntypedDataType],
    context: &SyntaxContext<ASTReference>,
) -> Option<Vec<TypedTypeParameter>> {
    fillings
        .iter()
        .zip(type_parameters.iter())
        .map(|(filling, param)| analyze_type_parameter_providing(param, filling, context))
        .collect::<Option<Vec<_>>>()
}

/// Analyzes a function call
///
/// Does not care if the function call is void or not
pub(crate) fn analyze_function_call(
    to_analyze: &FunctionCall<UntypedAST>,
    mapper: &mut FunctionSymbolMapper,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
) -> Option<FunctionCall<TypedAST>> {
    let call_name = to_analyze.function();

    // TODO: Methods
    let found_symbol = symbol_by_name(&call_name.0, context.ast_reference.symbols_available_at())?;

    let untyped_func_symbol = match found_symbol {
        DirectlyAvailableSymbol::Function(f) => f,
        _ => return None,
    };

    let typed_func_symbol = context
        .global_elements
        .get_or_insert_typed_function_symbol(
            untyped_func_symbol,
            &to_analyze.function().1,
            |from| {
                let mut context = SyntaxContext::new(
                    from,
                    context.type_parameter_context.clone(),
                    context.ast_reference.clone(),
                );
                analyze_type_parameters_providing(
                    untyped_func_symbol.type_parameters(),
                    &to_analyze.function().1,
                    &mut context,
                )
            },
        )
        .expect("Critical: Symbol found in AST but missing in map. Stage 2 failed?");

    let mut typed_args: Vec<ASTNode<Expression<TypedAST>>> = Vec::new();
    for untyped_arg_node in to_analyze.args().iter() {
        let position = untyped_arg_node.position().clone();

        // Rekursiv analyze_expression aufrufen (mit allen Parametern!)
        let typed_expr = analyze_expression(untyped_arg_node, context, mapper)?;

        typed_args.push(ASTNode::new(typed_expr, position));
    }
    FunctionCall::<TypedAST>::new(typed_func_symbol, typed_args)
}
