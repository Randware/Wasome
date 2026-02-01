use crate::mics_sa::{
    analyze_data_type, analyze_enum_usage, analyze_function_call, analyze_method_call,
    analyze_struct_usage,
};
use crate::symbol::SyntaxContext;
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use crate::symbol_by_name;
use ast::data_type::{DataType, Typed};
use ast::expression::{
    BinaryOp, Expression, FunctionCall, Literal, NewEnum, NewStruct, StructFieldAccess, Typecast,
    UnaryOp, UnaryOpType,
};
use ast::symbol::{DirectlyAvailableSymbol, SymbolWithTypeParameter, VariableSymbol};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::ops::Deref;
use std::rc::Rc;

/// Analyzes an untyped expression and converts it into a typed `Expression`.
///
/// # Parameters
/// * `to_analyze` - The expression to be analyzed (`Expression<UntypedAST>`).
/// * `symbol_mapper` - Mutable reference to `FunctionSymbolMapper` used for resolving symbols and tracking type information during analysis.
///
/// # Returns
/// * `Some(Expression<TypedAST>)` if the expression and all nested sub-expressions can be successfully analyzed and typed.
/// * `None` if analysis or conversion fails for the expression or any of its sub-expressions. Failure context is not provided.
pub(crate) fn analyze_expression(
    to_analyze: &Expression<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Expression<TypedAST>> {
    Some(match to_analyze {
        Expression::FunctionCall(inner) => {
            let typed_call =
                analyze_non_void_function_call(inner, context, function_symbol_mapper)?;
            Expression::FunctionCall(typed_call)
        }
        Expression::MethodCall(inner) => {
            let typed_call = analyze_method_call(inner, function_symbol_mapper, context)?;
            typed_call.function().return_type()?;
            Expression::FunctionCall(typed_call)
        }
        Expression::Variable(inner) => analyze_variable_use(inner, function_symbol_mapper)?,
        Expression::Literal(inner) => Expression::Literal(analyze_literal(inner)?),
        Expression::UnaryOp(inner) => {
            Expression::UnaryOp(analyze_unary_op(inner, context, function_symbol_mapper)?)
        }
        Expression::BinaryOp(inner) => {
            Expression::BinaryOp(analyze_binary_op(inner, context, function_symbol_mapper)?)
        }
        Expression::NewStruct(nstr) => {
            Expression::NewStruct(analyze_new_struct(nstr, context, function_symbol_mapper)?)
        }
        Expression::NewEnum(ne) => {
            Expression::NewEnum(analyze_new_enum(ne, context, function_symbol_mapper)?)
        }
        Expression::StructFieldAccess(sfa) => Expression::StructFieldAccess(
            analyze_struct_field_access(sfa, context, function_symbol_mapper)?,
        ),
    })
}

/// Analyzes an untyped `FunctionCall`, resolves the function symbol, recursively analyzes all arguments,
/// and delegates the final argument count and type checking to the `FunctionCall::new` constructor.
///
/// This function also enforces that the called function must have a return type.
///
/// # Parameters
/// * `to_analyze` - The untyped `FunctionCall` structure.
/// * `function_symbol_mapper` - The mapper for resolving the function symbol and analyzing nested expressions.
///
/// # Returns
/// * `Some(FunctionCall<TypedAST>)` on success.
/// * `None` on semantic error (undeclared function, argument mismatch, argument analysis failure, or if the function returns `void`).
pub(crate) fn analyze_non_void_function_call(
    to_analyze: &FunctionCall<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<FunctionCall<TypedAST>> {
    let typed_call = analyze_function_call(to_analyze, function_symbol_mapper, context)?;

    // Non-void function calls must return something
    typed_call.function().return_type()?;
    Some(typed_call)
}

/// Analyzes the use of a variable within an expression.
///
/// This function looks up the variable by name in the symbol mapper to ensure it has been declared
/// and retrieves its typed symbol.
///
/// # Parameters
/// * `variable_name` - The name of the variable (`&str`) to look up.
/// * `function_symbol_mapper` - The mapper for scope and symbol resolution.
///
/// # Returns
/// * `Some(Expression<TypedAST>)` if the variable is found.
/// * `None` otherwise (semantic error).
fn analyze_variable_use(
    variable_name: &str,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Expression<TypedAST>> {
    let typed_symbol: Rc<VariableSymbol<TypedAST>> =
        function_symbol_mapper.lookup_variable(variable_name)?;

    Some(Expression::Variable(typed_symbol))
}

/// Analyzes a literal string and converts it into a `Literal` type.
///
/// # Parsing Limits
/// *   **F64**: Uses `f64::parse`. Accepts standard float representations. Special values like `inf` or `NaN` are accepted.
/// *   **S32**: Uses `i32::parse`. Returns `None` if the literal exceeds `i32` bounds.
/// *   **Char**: Expects the string to be wrapped in single quotes (e.g., `'c'`).
///     - It does *not* support escape sequences
///         - (e.g., `'\n'` passed as a 2-char string will fail) and strictly expects the inner content to be parseable as a single `char`.
///         - The lexer already resolves escape sequences, so we don't need to handle them here
///
///
/// # Parameters
/// * `to_analyze` - The literal string to be analyzed.
///
/// # Returns
/// * `Some(Literal)` if the string can be successfully recognized and converted as a literal.
/// * `None` if the analysis or conversion fails.
fn analyze_literal(to_analyze: &str) -> Option<Literal> {
    if to_analyze == "true" {
        return Some(Literal::Bool(true));
    }
    if to_analyze == "false" {
        return Some(Literal::Bool(false));
    }

    if to_analyze.starts_with('\'') && to_analyze.ends_with('\'') {
        let inner = &to_analyze[1..to_analyze.len() - 1];
        if let Ok(c) = inner.parse::<char>() {
            return Some(Literal::Char(c as u32));
        }
    }

    if to_analyze.contains('.')
        && let Ok(f64_val) = to_analyze.parse::<f64>()
    {
        return Some(Literal::F64(f64_val));
    }

    if let Ok(s32_val) = to_analyze.parse::<i32>() {
        return Some(Literal::S32(s32_val));
    }

    None
}

/// Creates a new instance of `UnaryOp`.
///
/// # Parameters
/// * `to_analyze` - The unary operation to be analyzed.
/// * `symbol_mapper` - The mapper for resolving symbols.
///
/// # Returns
/// * `Some(Box<UnaryOp<TypedAST>>)` if the unary operation and its operand can be analyzed and converted to a typed form.
/// * `None` if analysis or conversion fails.
fn analyze_unary_op(
    to_analyze: &UnaryOp<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Box<UnaryOp<TypedAST>>> {
    let (op_type, expression) = (to_analyze.op_type(), to_analyze.input());

    let converted_input = analyze_expression(expression, context, function_symbol_mapper)?;

    let converted_unary_op_type = match op_type {
        UnaryOpType::Typecast(inner) => {
            let data_type = inner.target();
            let analyzed_data_type = analyze_data_type(data_type, context)?;
            let typed_typecast = Typecast::<TypedAST>::new(analyzed_data_type);
            UnaryOpType::Typecast(typed_typecast)
        }
        UnaryOpType::Negative => UnaryOpType::Negative,
        UnaryOpType::Not => UnaryOpType::Not,
    };

    let postion = expression.position().clone();

    let analyzed = UnaryOp::<TypedAST>::new(
        converted_unary_op_type,
        ASTNode::new(converted_input, postion),
    )?;
    Some(Box::new(analyzed))
}

/// Creates a new instance of `BinaryOp`.
///
/// # Parameters
/// * `to_analyze` - The Binary operation to be analyzed.
/// * `symbol_mapper` - The mapper for resolving symbols.
///
/// # Returns
/// * `Some(Box<BinaryOp<TypedAST>>)` if the Binary operation and its operand can be analyzed and converted to a typed form.
/// * `None` if analysis or conversion fails.
fn analyze_binary_op(
    to_analyze: &BinaryOp<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Box<BinaryOp<TypedAST>>> {
    let (op_type, left_expr, right_expr) =
        (to_analyze.op_type(), to_analyze.left(), to_analyze.right());

    let converted_left = analyze_expression(left_expr, context, function_symbol_mapper)?;
    let converted_right = analyze_expression(right_expr, context, function_symbol_mapper)?;

    let left_position = left_expr.position().clone();
    let right_position = right_expr.position().clone();

    let typed_left_node = ASTNode::new(converted_left, left_position);
    let typed_right_node = ASTNode::new(converted_right, right_position);

    let analyzed = BinaryOp::<TypedAST>::new(op_type, typed_left_node, typed_right_node)?;

    Some(Box::new(analyzed))
}

fn analyze_new_struct(
    to_analyze: &NewStruct<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Box<NewStruct<TypedAST>>> {
    let struct_use = analyze_struct_usage(&to_analyze.symbol().0, &to_analyze.symbol().1, context)?;
    let untyped_struct_symbol = if let DirectlyAvailableSymbol::Struct(st) = symbol_by_name(
        &to_analyze.symbol().0,
        context.ast_reference.symbols_available_at(),
    )? {
        st
    } else {
        return None;
    };

    let struct_fields = context
        .global_elements
        .get_struct_fields(untyped_struct_symbol, struct_use.type_parameters())?;
    let parameter = to_analyze
        .parameters()
        .iter()
        .map(|param| {
            Some((
                ASTNode::new(
                    struct_fields
                        .iter()
                        .find(|field| param.0.deref() == field.name())?
                        .clone(),
                    param.0.position().clone(),
                ),
                ASTNode::new(
                    analyze_expression(&param.1, context, function_symbol_mapper)?,
                    param.1.position().clone(),
                ),
            ))
        })
        .collect::<Option<Vec<_>>>()?;

    let all_struct_fields_exist_dt_match = struct_fields.iter().all(|field| {
        parameter
            .iter()
            .find(|param| param.0.name() == field.name())
            .is_some_and(|val| &val.1.data_type() == field.data_type())
    });
    let types_ok = all_struct_fields_exist_dt_match && parameter.len() == struct_fields.len();
    if !types_ok {
        return None;
    }
    let analyzed = NewStruct::<TypedAST>::new(struct_use, parameter);

    Some(Box::new(analyzed))
}

fn analyze_new_enum(
    to_analyze: &NewEnum<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Box<NewEnum<TypedAST>>> {
    let enum_use = analyze_enum_usage(
        &to_analyze.to_create().0,
        &to_analyze.to_create().1,
        context,
    )?;
    let untyped_enum_symbol = if let DirectlyAvailableSymbol::Enum(en) = symbol_by_name(
        &to_analyze.to_create().0,
        context.ast_reference.symbols_available_at(),
    )? {
        en
    } else {
        return None;
    };

    let enum_variants = context
        .global_elements
        .get_enum_variants(untyped_enum_symbol, enum_use.type_parameters())?;
    let enum_variant = enum_variants
        .iter()
        .find(|var| var.name() == to_analyze.variant())?;
    let parameter = to_analyze
        .parameters()
        .iter()
        .map(|param| {
            Some(ASTNode::new(
                analyze_expression(param, context, function_symbol_mapper)?,
                param.position().clone(),
            ))
        })
        .collect::<Option<Vec<_>>>()?;

    if !parameter
        .iter()
        .zip(enum_variant.fields().iter())
        .all(|(found, expected)| &found.data_type() == expected)
    {
        return None;
    }
    let analyzed = NewEnum::<TypedAST>::new(enum_use, enum_variant.clone(), parameter)?;

    Some(Box::new(analyzed))
}

fn analyze_struct_field_access(
    to_analyze: &StructFieldAccess<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Box<StructFieldAccess<TypedAST>>> {
    let source_expr = analyze_expression(to_analyze.of(), context, function_symbol_mapper)?;

    let source_symbol = if let DataType::Struct(st) = source_expr.data_type() {
        st
    } else {
        return None;
    };
    let sfs = context.global_elements.get_struct_fields(
        context
            .global_elements
            .untyped_struct_symbol_from_typed(&source_symbol).as_deref()?,
        source_symbol.type_parameters(),
    )?;
    let sf = sfs
        .iter()
        .find(|sf| sf.name() == to_analyze.field())?
        .clone();

    Some(Box::new(StructFieldAccess::<TypedAST>::new(
        ASTNode::new(source_expr, to_analyze.of().position().clone()),
        sf,
    )?))
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::expression::Literal;

    /// Tests the helper function `analyze_literal`.
    /// It ensures that string representations of booleans, chars, floats, and integers are correctly parsed into their `Literal` enum variants.
    #[test]
    fn analyze_literal_recognizes_values() {
        assert_eq!(analyze_literal("true"), Some(Literal::Bool(true)));
        assert_eq!(analyze_literal("false"), Some(Literal::Bool(false)));
        assert_eq!(analyze_literal("'\n'"), Some(Literal::Char('\n' as u32)));
        assert_eq!(analyze_literal("3.14"), Some(Literal::F64(3.14)));
        assert_eq!(analyze_literal("42"), Some(Literal::S32(42)));
        assert_eq!(analyze_literal("nope"), None);
    }
    
}
