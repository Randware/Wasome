use std::any::Any;
use crate::expression_sa::analyze_expression;
use crate::{symbol_by_name};
use crate::symbol_translation::function_symbol_mapper::FunctionSymbolMapper;
use ast::data_type::{DataType, UntypedDataType};
use ast::expression::{Expression, FunctionCall};
use ast::symbol::DirectlyAvailableSymbol;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use ast::type_parameter::{TypedTypeParameter, UntypedTypeParameter};
use crate::symbol_translation::{AnalyzableSyntaxElementWithTypeParameter, SyntaxContext, TypeParameterContext};

/// A helper function that resolves the type names into the right types.
///
/// # Parameters
/// * `to_analyze` - The string representation of the data type (e.g., "s32", "bool").
///
/// # Returns
/// * `Some(DataType)` if the string matches a known type.
/// * `None` otherwise.
pub(crate) fn analyze_data_type<T>(to_analyze: &UntypedDataType, context: &mut SyntaxContext<impl TypeParameterContext, T>) -> Option<DataType> {
    // TODO: Add Composites
    analyze_primitive_data_type(to_analyze)
        .or_else(|| analyze_type_parameter(to_analyze.name(), context).map(|tp| tp.data_type().clone()))
}

fn analyze_primitive_data_type(to_analyze: &UntypedDataType) -> Option<DataType> {
    if to_analyze.type_parameters().len() != 0 {
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

pub(crate) fn analyze_type_parameter_full<'a, T>(to_analyze: &UntypedTypeParameter, context: &'a mut SyntaxContext<impl TypeParameterContext, T>) -> Option<&'a TypedTypeParameter> {
    analyze_type_parameter(to_analyze.inner().name(), context)
}

pub(crate) fn analyze_type_parameter<'a, T>(to_analyze: &str, context: &'a mut SyntaxContext<impl TypeParameterContext, T>) -> Option<&'a TypedTypeParameter> {
    context .type_parameter_context.lookup_type_parameter(to_analyze)
}

/// Analyzes a function call
///
/// Does not care if the function call is void or not
pub(crate) fn analyze_function_call(
    to_analyze: &FunctionCall<UntypedAST>,
    mapper: &mut FunctionSymbolMapper,
    context: &mut SyntaxContext<impl TypeParameterContext, StatementTraversalHelper<UntypedAST>>,
) -> Option<FunctionCall<TypedAST>> {
    let call_name = to_analyze.function();

    // TODO
    let found_symbol = symbol_by_name(&call_name.0, context.ast_reference.symbols_available_at())?;

    let untyped_func_symbol = match found_symbol {
        DirectlyAvailableSymbol::Function(f) => f,
        _ => return None,
    };

    let typed_func_symbol = context
        .global_elements
        .get_typed_function_symbol(untyped_func_symbol, &to_analyze.function().1)
        .expect("Critical: Symbol found in AST but missing in map. Stage 2 failed?");

    let mut typed_args: Vec<ASTNode<Expression<TypedAST>>> = Vec::new();
    for untyped_arg_node in to_analyze.args().iter() {
        let position = untyped_arg_node.position().clone();

        // Rekursiv analyze_expression aufrufen (mit allen Parametern!)
        let typed_expr = analyze_expression(untyped_arg_node, mapper, helper)?;

        typed_args.push(ASTNode::new(typed_expr, position));
    }
    FunctionCall::<TypedAST>::new(typed_func_symbol, typed_args)
}
