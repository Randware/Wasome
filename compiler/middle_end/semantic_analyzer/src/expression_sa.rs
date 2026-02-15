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
use crate::error_sa::SemanticError;

/// Analyzes an untyped expression and converts it into a typed `Expression`.
///
/// # Parameters
/// * `to_analyze` - The expression to be analyzed (`Expression<UntypedAST>`).
/// * `symbol_mapper` - Mutable reference to `FunctionSymbolMapper` used for resolving symbols and tracking type information during analysis.
///
/// # Returns
/// * `Ok(Expression<TypedAST>)` if the expression and all nested sub-expressions can be successfully analyzed and typed.
/// * `Err(SemanticError)` if analysis or conversion fails.
pub(crate) fn analyze_expression(
    to_analyze: &Expression<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Expression<TypedAST>, SemanticError> {
    Ok(match to_analyze {
        Expression::FunctionCall(inner) => {
            let typed_call =
                analyze_non_void_function_call(inner, context, function_symbol_mapper)?;
            Expression::FunctionCall(typed_call)
        }
        Expression::MethodCall(inner) => {
            let typed_call = analyze_method_call(inner, function_symbol_mapper, context)?;
            typed_call.function().return_type().ok_or_else(|| SemanticError::Custom {
                message: format!("Method '{}' does not return a value", inner.function().0),
                span: source::types::FileID::from(0).span(0, 0),
            })?;
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
/// * `Ok(FunctionCall<TypedAST>)` on success.
/// * `Err(SemanticError)` on semantic error (undeclared function, argument mismatch, etc.).
pub(crate) fn analyze_non_void_function_call(
    to_analyze: &FunctionCall<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<FunctionCall<TypedAST>, SemanticError> {
    let typed_call = analyze_function_call(to_analyze, function_symbol_mapper, context)?;

    // Non-void function calls must return something
    typed_call.function().return_type().ok_or_else(|| SemanticError::Custom {
        message: format!("Function '{}' does not return a value", to_analyze.function().0),
        span: source::types::FileID::from(0).span(0, 0),
    })?;
    Ok(typed_call)
}

/// Analyzes the use of a variable within an expression.
fn analyze_variable_use(
    variable_name: &str,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Expression<TypedAST>, SemanticError> {
    let typed_symbol: Rc<VariableSymbol<TypedAST>> =
        function_symbol_mapper.lookup_variable(variable_name).ok_or_else(|| SemanticError::UnknownSymbol {
            name: variable_name.to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        })?;

    Ok(Expression::Variable(typed_symbol))
}

/// Analyzes a literal string and converts it into a `Literal` type.
fn analyze_literal(to_analyze: &str) -> Result<Literal, SemanticError> {
    if to_analyze == "true" {
        return Ok(Literal::Bool(true));
    }
    if to_analyze == "false" {
        return Ok(Literal::Bool(false));
    }

    if to_analyze.starts_with('\'') && to_analyze.ends_with('\'') {
        let inner = &to_analyze[1..to_analyze.len() - 1];
        if let Ok(c) = inner.parse::<char>() {
            return Ok(Literal::Char(c as u32));
        }
    }

    if to_analyze.contains('.')
        && let Ok(f64_val) = to_analyze.parse::<f64>()
    {
        return Ok(Literal::F64(f64_val));
    }

    if let Ok(s32_val) = to_analyze.parse::<i32>() {
        return Ok(Literal::S32(s32_val));
    }

    Err(SemanticError::Custom {
        message: format!("Invalid literal '{}'", to_analyze),
        span: source::types::FileID::from(0).span(0, 0),
    })
}

/// Creates a new instance of `UnaryOp`.
fn analyze_unary_op(
    to_analyze: &UnaryOp<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Box<UnaryOp<TypedAST>>, SemanticError> {
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

    let postion = *expression.position();

    let analyzed = UnaryOp::<TypedAST>::new(
        converted_unary_op_type,
        ASTNode::new(converted_input, postion),
    ).ok_or_else(|| SemanticError::Custom {
        message: "Invalid unary operation".to_string(),
        span: postion,
    })?;

    Ok(Box::new(analyzed))
}

/// Creates a new instance of `BinaryOp`.
fn analyze_binary_op(
    to_analyze: &BinaryOp<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Box<BinaryOp<TypedAST>>, SemanticError> {
    let (op_type, left_expr, right_expr) =
        (to_analyze.op_type(), to_analyze.left(), to_analyze.right());

    let converted_left = analyze_expression(left_expr, context, function_symbol_mapper)?;
    let converted_right = analyze_expression(right_expr, context, function_symbol_mapper)?;

    let left_position = *left_expr.position();
    let right_position = *right_expr.position();

    let typed_left_node = ASTNode::new(converted_left, left_position);
    let typed_right_node = ASTNode::new(converted_right, right_position);

    let analyzed = BinaryOp::<TypedAST>::new(op_type, typed_left_node, typed_right_node)
        .ok_or_else(|| SemanticError::Custom {
            message: "Invalid binary operation".to_string(),
            span: left_position,
        })?;

    Ok(Box::new(analyzed))
}

fn analyze_new_struct(
    to_analyze: &NewStruct<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Box<NewStruct<TypedAST>>, SemanticError> {
    let struct_use = analyze_struct_usage(&to_analyze.symbol().0, &to_analyze.symbol().1, context)?;

    let untyped_struct_symbol = if let DirectlyAvailableSymbol::Struct(st) = symbol_by_name(
        &to_analyze.symbol().0,
        context.ast_reference.symbols_available_at(),
    ).ok_or_else(|| SemanticError::UnknownSymbol {
        name: to_analyze.symbol().0.clone(),
        span: source::types::FileID::from(0).span(0, 0),
    })? {
        st
    } else {
        return Err(SemanticError::Custom {
            message: format!("'{}' is not a struct", to_analyze.symbol().0),
            span: source::types::FileID::from(0).span(0, 0),
        });
    };

    let struct_fields = context
        .global_elements
        .get_struct_fields(untyped_struct_symbol, struct_use.type_parameters())
        .ok_or_else(|| SemanticError::Custom {
            message: "Failed to get struct fields".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        })?;

    let parameter = to_analyze
        .parameters()
        .iter()
        .map(|param| {
            let field = struct_fields
                .iter()
                .find(|field| param.0.deref() == field.name())
                .ok_or_else(|| SemanticError::Custom {
                    message: format!("Field '{}' not found in struct", param.0.deref()),
                    span: *param.0.position(),
                })?
                .clone();

            let expr = analyze_expression(&param.1, context, function_symbol_mapper)?;

            Ok((
                ASTNode::new(field, *param.0.position()),
                ASTNode::new(expr, *param.1.position()),
            ))
        })
        .collect::<Result<Vec<_>, SemanticError>>()?;

    let all_struct_fields_exist_dt_match = struct_fields.iter().all(|field| {
        parameter
            .iter()
            .find(|param| param.0.name() == field.name())
            .is_some_and(|val| &val.1.data_type() == field.data_type())
    });

    let types_ok = all_struct_fields_exist_dt_match && parameter.len() == struct_fields.len();
    if !types_ok {
        return Err(SemanticError::Custom {
            message: "Struct initialization parameters do not match fields".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        });
    }

    let analyzed = NewStruct::<TypedAST>::new(struct_use, parameter);
    Ok(Box::new(analyzed))
}

fn analyze_new_enum(
    to_analyze: &NewEnum<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Box<NewEnum<TypedAST>>, SemanticError> {
    let enum_use = analyze_enum_usage(
        &to_analyze.to_create().0,
        &to_analyze.to_create().1,
        context,
    )?;

    let untyped_enum_symbol = if let DirectlyAvailableSymbol::Enum(en) = symbol_by_name(
        &to_analyze.to_create().0,
        context.ast_reference.symbols_available_at(),
    ).ok_or_else(|| SemanticError::UnknownSymbol {
        name: to_analyze.to_create().0.clone(),
        span: source::types::FileID::from(0).span(0, 0),
    })? {
        en
    } else {
        return Err(SemanticError::Custom {
            message: format!("'{}' is not an enum", to_analyze.to_create().0),
            span: source::types::FileID::from(0).span(0, 0),
        });
    };

    let enum_variants = context
        .global_elements
        .get_enum_variants(untyped_enum_symbol, enum_use.type_parameters())
        .ok_or_else(|| SemanticError::Custom {
            message: "Failed to get enum variants".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        })?;

    let enum_variant = enum_variants
        .iter()
        .find(|var| var.name() == to_analyze.variant())
        .ok_or_else(|| SemanticError::Custom {
            message: format!("Variant '{}' not found in enum", to_analyze.variant()),
            span: source::types::FileID::from(0).span(0, 0),
        })?;

    let parameter = to_analyze
        .parameters()
        .iter()
        .map(|param| {
            let expr = analyze_expression(param, context, function_symbol_mapper)?;
            Ok(ASTNode::new(expr, *param.position()))
        })
        .collect::<Result<Vec<_>, SemanticError>>()?;

    if !parameter
        .iter()
        .zip(enum_variant.fields().iter())
        .all(|(found, expected)| &found.data_type() == expected)
    {
        return Err(SemanticError::Custom {
            message: "Enum variant initialization parameters do not match fields".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        });
    }

    let analyzed = NewEnum::<TypedAST>::new(enum_use, enum_variant.clone(), parameter)
        .ok_or_else(|| SemanticError::Custom {
            message: "Failed to create new enum".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        })?;

    Ok(Box::new(analyzed))
}

fn analyze_struct_field_access(
    to_analyze: &StructFieldAccess<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Box<StructFieldAccess<TypedAST>>, SemanticError> {
    let source_expr = analyze_expression(to_analyze.of(), context, function_symbol_mapper)?;

    let source_symbol = if let DataType::Struct(st) = source_expr.data_type() {
        st
    } else {
        return Err(SemanticError::Custom {
            message: "Field access on non-struct type".to_string(),
            span: *to_analyze.of().position(),
        });
    };

    let untyped_symbol_opt = context
        .global_elements
        .untyped_struct_symbol_from_typed(&source_symbol);

    let untyped_symbol = untyped_symbol_opt.ok_or_else(|| SemanticError::Custom {
        message: "Internal Error: Could not find untyped struct symbol".to_string(),
        span: *to_analyze.of().position(),
    })?;

    let sfs = context.global_elements.get_struct_fields(
        &untyped_symbol,
        source_symbol.type_parameters(),
    ).ok_or_else(|| SemanticError::Custom {
        message: "Failed to get struct fields".to_string(),
        span: *to_analyze.of().position(),
    })?;

    let sf = sfs
        .iter()
        .find(|sf| sf.name() == to_analyze.field())
        .ok_or_else(|| SemanticError::Custom {
            message: format!("Field '{}' not found", to_analyze.field()),
            span: source::types::FileID::from(0).span(0, 0),
        })?
        .clone();

    let analyzed = StructFieldAccess::<TypedAST>::new(
        ASTNode::new(source_expr, *to_analyze.of().position()),
        sf,
    ).ok_or_else(|| SemanticError::Custom {
        message: "Failed to create struct field access".to_string(),
        span: source::types::FileID::from(0).span(0, 0),
    })?;

    Ok(Box::new(analyzed))
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::expression::Literal;
    
    /// Tests the helper function `analyze_literal`.
    /// It ensures that string representations of booleans, chars, floats, and integers are correctly parsed into their `Literal` enum variants.
    #[test]
    fn analyze_literal_recognizes_values() {
        assert_eq!(analyze_literal("true").unwrap(), Literal::Bool(true));
        assert_eq!(analyze_literal("false").unwrap(), Literal::Bool(false));
        assert_eq!(analyze_literal("'\n'").unwrap(), Literal::Char('\n' as u32));
        assert_eq!(analyze_literal("3.14").unwrap(), Literal::F64(3.14));
        assert_eq!(analyze_literal("42").unwrap(), Literal::S32(42));
        assert!(analyze_literal("nope").is_err());
    }
}

