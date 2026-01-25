use std::rc::Rc;
use ast::data_type::DataType;
use ast::expression::{Expression, FunctionCall};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use ast::symbol::{FunctionSymbol, Symbol};
use crate::expression_sa::analyze_expression;
use crate::symbol_by_name;
use crate::symbol_translation::function_symbol_mapper::FunctionSymbolMapper;

/// A helper function that resolves the type names into the right types.
///
/// # Parameters
/// * `to_analyze` - The string representation of the data type (e.g., "s32", "bool").
///
/// # Returns
/// * `Some(DataType)` if the string matches a known type.
/// * `None` otherwise.
pub(crate) fn analyze_data_type(to_analyze: &str) -> Option<DataType> {
    match to_analyze {
        "char" => Some(DataType::Char),
        "u8" => Some(DataType::U8),
        "s8" => Some(DataType::S8),
        "u16" => Some(DataType::U16),
        "s16" => Some(DataType::S16),
        "u32" => Some(DataType::U32),
        "s32" => Some(DataType::S32),
        "u64" => Some(DataType::U64),
        "s64" => Some(DataType::S64),
        "bool" => Some(DataType::Bool),
        "f32" => Some(DataType::F32),
        "f64" => Some(DataType::F64),
        _ => None,
    }
}

/// Analyzes a function call
/// 
/// Does not care if the function call is void or not
pub(crate) fn analyze_function_call(to_analyze: &FunctionCall<UntypedAST>, mapper: &mut FunctionSymbolMapper, helper: &StatementTraversalHelper<UntypedAST>) -> Option<FunctionCall<TypedAST>> {
    let call_name = to_analyze.function();

    let found_symbol = symbol_by_name(call_name, helper.symbols_available_at())?;

    let untyped_func_symbol = match found_symbol {
        Symbol::Function(f) => f,
        _ => return None,
    };

    let typed_func_symbol = mapper
        .lookup_function(untyped_func_symbol)
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
