use crate::error_sa::SemanticError;
use crate::expression_sa::analyze_expression;
use crate::mics_sa::{
    analyze_data_type, analyze_enum_usage, analyze_function_call, analyze_method_call,
};
use crate::symbol::SyntaxContext;
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use crate::symbol_by_name;
use ast::data_type::{DataType, Typed};
use ast::expression::{Expression, FunctionCall};
use ast::statement::{
    CodeBlock, Conditional, ControlStructure, IfEnumVariant, Loop, LoopType, Return, Statement,
    StructFieldAssignment, VariableAssignment, VariableDeclaration,
};
use ast::symbol::{DirectlyAvailableSymbol, SymbolWithTypeParameter, VariableSymbol};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::ops::Deref;
use std::rc::Rc;

/// Analyzes a statement referenced by a traversal helper and converts it into a typed statement node.
///
/// This function acts as the main dispatcher for statement analysis. It delegates to specific
/// handler functions based on the statement type. It ensures that global symbols (functions)
/// are resolved via the `global_map` and local variables via the `function_symbol_mapper`.
///
/// # Panics
/// * Panics if it encounters a `Statement::VoidFunctionCall`. It assumes the untyped AST will never produce a `VoidFunctionCall` directly (these are generated during this analysis phase).
///
/// # Parameters
/// * `context` - The syntax context containing the traversal helper and available symbols.
/// * `function_symbol_mapper` - Provides current function context (return type, local scopes) (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Ok(Statement<TypedAST>)` if the statement and its children were successfully analyzed.
/// * `Err(SemanticError)` if a semantic error occurs (e.g., type mismatch, unknown variable).
pub(crate) fn analyze_statement(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Statement<TypedAST>, SemanticError> {
    let to_analyze = context.ast_reference.clone();
    let statement_to_analyze = to_analyze.inner();
    match statement_to_analyze.deref() {
        Statement::VariableAssignment(inner) => {
            let assigned = analyze_variable_assignment(inner, function_symbol_mapper, context)?;
            Ok(Statement::VariableAssignment(assigned))
        }
        Statement::VariableDeclaration(inner) => {
            let declared = analyze_variable_declaration(inner, context, function_symbol_mapper)?;
            Ok(Statement::VariableDeclaration(declared))
        }
        Statement::Expression(inner) => {
            if let Some(void_call) =
                try_analyze_void_function_call(context, function_symbol_mapper)?
            {
                Ok(Statement::VoidFunctionCall(void_call))
            } else if let Some(void_method) =
                try_analyze_void_method_call(context, function_symbol_mapper)?
            {
                Ok(Statement::VoidFunctionCall(void_method))
            } else {
                let typed_expr = analyze_expression(inner, context, function_symbol_mapper)?;
                Ok(Statement::Expression(ASTNode::new(
                    typed_expr,
                    *inner.position(),
                )))
            }
        }
        Statement::Return(inner) => {
            let typed_ret = analyze_return(inner, context, function_symbol_mapper)?;
            Ok(Statement::Return(typed_ret))
        }
        Statement::ControlStructure(crtl) => {
            let typed_cs = Box::new(analyze_control_structure(
                crtl.as_ref(),
                context,
                function_symbol_mapper,
            )?);
            Ok(Statement::ControlStructure(typed_cs))
        }
        Statement::Codeblock(_) => {
            let analyzed_cb = analyze_codeblock(context, function_symbol_mapper)?;
            Ok(Statement::Codeblock(analyzed_cb))
        }
        Statement::VoidFunctionCall(_) => {
            panic!("Void function calls are not allowed in the untyped AST")
        }
        Statement::Break => analyze_break(context),
        Statement::StructFieldAssignment(sfa) => Ok(Statement::StructFieldAssignment(
            analyze_struct_field_assignment(sfa, context, function_symbol_mapper)?,
        )),
    }
}

fn try_analyze_void_function_call(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Option<FunctionCall<TypedAST>>, SemanticError> {
    let to_analyze = &context.ast_reference;
    let statement_to_analyze = to_analyze.inner();
    let expr = if let Statement::Expression(inner) = statement_to_analyze.deref() {
        inner
    } else {
        return Ok(None);
    };
    let call = match expr.deref() {
        Expression::FunctionCall(call) => call,
        _ => return Ok(None),
    };

    let call = analyze_function_call(call, function_symbol_mapper, context)?;
    if call.function().return_type().is_some() {
        return Ok(None);
    }
    Ok(Some(call))
}

fn try_analyze_void_method_call(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Option<FunctionCall<TypedAST>>, SemanticError> {
    let to_analyze = &context.ast_reference;
    let statement_to_analyze = to_analyze.inner();
    let expr = if let Statement::Expression(inner) = statement_to_analyze.deref() {
        inner
    } else {
        return Ok(None);
    };
    let call = match expr.deref() {
        Expression::MethodCall(call) => call,
        _ => return Ok(None),
    };

    let symbol = match symbol_by_name(&call.function().0, to_analyze.symbols_available_at()) {
        Some(s) => s,
        None => return Ok(None),
    };

    if let DirectlyAvailableSymbol::Function(func) = symbol {
        if func.return_type().is_none() {
            return Ok(None);
        }
    } else {
        return Ok(None);
    };

    let typed_call = analyze_method_call(call, function_symbol_mapper, context)?;
    Ok(Some(typed_call))
}

/// Analyzes a variable assignment (re-assignment of an existing variable).
///
/// It checks if the variable exists in the current scope and if the type of the assigned value matches.
///
/// # Type Checking
/// Enforces strict type equality. Implicit casting (e.g., `s32` to `s64`) is **not** supported.
///
/// # Parameters
/// * `to_analyze` - The untyped assignment node.
/// * `function_symbol_mapper` - Used to look up the existing variable in the current scope.
/// * `context` - The syntax context providing the scope and symbol resolution.
///
/// # Returns
/// * `Ok(VariableAssignment<TypedAST>)` if the variable exists and types match.
/// * `Err(SemanticError)` if the variable is not found or types mismatch.
fn analyze_variable_assignment(
    to_analyze: &VariableAssignment<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
) -> Result<VariableAssignment<TypedAST>, SemanticError> {
    let var_name = to_analyze.variable();
    let untyped_val = to_analyze.value();

    let typed_variable_symbol = function_symbol_mapper
        .lookup_variable(var_name)
        .ok_or_else(|| SemanticError::UnknownSymbol {
            name: var_name.to_string(),
            span: *untyped_val.position(),
        })?;

    let typed_value_expr = analyze_expression(untyped_val, context, function_symbol_mapper)?;

    if typed_variable_symbol.data_type() != &typed_value_expr.data_type() {
        return Err(SemanticError::Custom {
            message: format!(
                "Type mismatch in assignment. Expected {:?}, got {:?}",
                typed_variable_symbol.data_type(),
                typed_value_expr.data_type()
            ),
            span: *untyped_val.position(),
        });
    }

    let typed_node = ASTNode::new(typed_value_expr, *untyped_val.position());

    VariableAssignment::<TypedAST>::new(typed_variable_symbol, typed_node).ok_or_else(|| {
        SemanticError::Custom {
            message: "Failed to create variable assignment (internal validation failed)"
                .to_string(),
            span: *untyped_val.position(),
        }
    })
}

/// Analyzes a variable declaration (creation of a new local variable).
///
/// It registers the new variable in the current scope and ensures the type of the
/// initializer matches the declared type.
///
/// # Shadowing
/// Allows shadowing of variables defined in outer scopes, but forbids defining a variable
/// with the same name multiple times within the *same* scope.
///
/// # Parameters
/// * `to_analyze` - The untyped declaration node.
/// * `context` - The syntax context providing symbol resolution for types and expressions.
/// * `function_symbol_mapper` - Used to register the new variable in the current scope.
///
/// # Returns
/// * `Ok(VariableDeclaration<TypedAST>)` if the variable is successfully declared.
/// * `Err(SemanticError)` if the type cannot be inferred or resolved, or if registration fails.
fn analyze_variable_declaration(
    to_analyze: &VariableDeclaration<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<VariableDeclaration<TypedAST>, SemanticError> {
    let untyped_val = to_analyze.value();
    let typed_value_expr = analyze_expression(untyped_val, context, function_symbol_mapper)?;

    let declared_type_name = to_analyze.variable().data_type();
    let resolved_declared_type = analyze_data_type(declared_type_name, context)?;

    if resolved_declared_type != typed_value_expr.data_type() {
        return Err(SemanticError::Custom {
            message: format!(
                "Type mismatch in declaration. Expected {:?}, got {:?}",
                resolved_declared_type,
                typed_value_expr.data_type()
            ),
            span: *untyped_val.position(),
        });
    }

    let var_name = to_analyze.variable().name().to_string();
    let typed_variable_symbol = Rc::new(VariableSymbol::new(var_name, resolved_declared_type));

    if function_symbol_mapper
        .add_variable(typed_variable_symbol.clone())
        .is_err()
    {
        return Err(SemanticError::Custom {
            message: format!(
                "Variable '{}' is already declared in this scope",
                to_analyze.variable().name()
            ),
            span: *untyped_val.position(),
        });
    }

    let typed_node = ASTNode::new(typed_value_expr, *untyped_val.position());

    VariableDeclaration::<TypedAST>::new(typed_variable_symbol, typed_node).ok_or_else(|| {
        SemanticError::Custom {
            message: "Failed to create variable declaration (internal validation failed)"
                .to_string(),
            span: *untyped_val.position(),
        }
    })
}

/// Analyzes a return statement.
///
/// # Parameters
/// * `to_analyze` - The untyped return node.
/// * `context` - The syntax context providing symbol resolution for the returned expression.
/// * `function_symbol_mapper` - Used to check against the function's expected return type.
///
/// # Returns
/// * `Ok(Return<TypedAST>)` if the return value matches the function signature.
/// * `Err(SemanticError)` if types mismatch or the return value is invalid.
fn analyze_return(
    to_analyze: &Return<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Return<TypedAST>, SemanticError> {
    let expected_return_type = function_symbol_mapper
        .get_current_function_return_type()
        .cloned();
    let untyped_return_value = to_analyze.to_return();

    match (expected_return_type, untyped_return_value) {
        (None, None) => Ok(Return::new(None)),

        (Some(expected), Some(expr_node)) => {
            let typed_expr = analyze_expression(expr_node, context, function_symbol_mapper)?;

            if typed_expr.data_type() != expected {
                return Err(SemanticError::Custom {
                    message: format!(
                        "Return type mismatch. Expected {:?}, got {:?}",
                        expected,
                        typed_expr.data_type()
                    ),
                    span: *expr_node.position(),
                });
            }

            let typed_node = ASTNode::new(typed_expr, *expr_node.position());
            Ok(Return::new(Some(typed_node)))
        }

        (None, Some(expr_node)) => Err(SemanticError::Custom {
            message: "Void function must not return a value".to_string(),
            span: *expr_node.position(),
        }),

        (Some(_), None) => Err(SemanticError::Custom {
            message: "Missing return value".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        }),
    }
}

/// Analyzes a control structure (conditional or loop).
///
/// Delegates to `analyze_conditional` or `analyze_loop` respectively.
///
/// # Scoping
/// Creates a new scope for the control structure's body (loops and conditionals).
///
/// # Parameters
/// * `to_analyze` - The untyped control structure.
/// * `context` - The syntax context providing access to children blocks and symbol resolution.
/// * `function_symbol_mapper` - Context for scope and variable management.
///
/// # Returns
/// * `Ok(ControlStructure<TypedAST>)` if the structure and its blocks are valid.
/// * `Err(SemanticError)` if analysis fails.
fn analyze_control_structure(
    to_analyze: &ControlStructure<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<ControlStructure<TypedAST>, SemanticError> {
    match to_analyze {
        ControlStructure::Conditional(cond) => {
            analyze_conditional(cond, context, function_symbol_mapper)
                .map(ControlStructure::Conditional)
        }
        ControlStructure::Loop(lp) => {
            analyze_loop(lp, context, function_symbol_mapper).map(ControlStructure::Loop)
        }
        ControlStructure::IfEnumVariant(iev) => {
            analyze_if_enum_variant(iev, context, function_symbol_mapper)
                .map(ControlStructure::IfEnumVariant)
        }
    }
}

/// Analyzes a conditional statement (if/else).
///
/// Recursively analyzes the condition expression and the 'then' and 'else' blocks.
///
/// # Parameters
/// * `to_analyze` - The untyped conditional (if/else) structure.
/// * `context` - The syntax context for resolving expressions and traversing blocks.
/// * `function_symbol_mapper` - Manages scopes for the then/else blocks.
///
/// # Returns
/// * `Ok(Conditional<TypedAST>)` if the condition is boolean and blocks are valid.
/// * `Err(SemanticError)` if analysis fails.
fn analyze_conditional(
    to_analyze: &Conditional<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Conditional<TypedAST>, SemanticError> {
    let untyped_condition = to_analyze.condition();

    let typed_condition_expr =
        analyze_expression(untyped_condition, context, function_symbol_mapper)?;

    let typed_condition = ASTNode::new(typed_condition_expr, *untyped_condition.position());

    if typed_condition.data_type() != DataType::Bool {
        return Err(SemanticError::Custom {
            message: "Condition must evaluate to a boolean".to_string(),
            span: *untyped_condition.position(),
        });
    }

    function_symbol_mapper.enter_scope();

    let sth = context.ast_reference.get_child(0).unwrap();
    let then_context = context.with_ast_reference(&sth);
    let then_position = *then_context.ast_reference.inner().position();

    let typed_then_statement = match analyze_statement(&then_context, function_symbol_mapper) {
        Ok(stmt) => stmt,
        Err(e) => {
            let _ = function_symbol_mapper.exit_scope();
            return Err(e);
        }
    };

    let typed_then_node = ASTNode::new(typed_then_statement, then_position);
    let _ = function_symbol_mapper.exit_scope();

    let typed_else_statement = if to_analyze.else_statement().is_some() {
        function_symbol_mapper.enter_scope();

        let sth = context.ast_reference.get_child(1).unwrap();
        let else_context = context.with_ast_reference(&sth);
        let else_position = *else_context.ast_reference.inner().position();

        let typed_block = match analyze_statement(&else_context, function_symbol_mapper) {
            Ok(stmt) => stmt,
            Err(e) => {
                let _ = function_symbol_mapper.exit_scope();
                return Err(e);
            }
        };
        let typed_block_node = ASTNode::new(typed_block, else_position);
        let _ = function_symbol_mapper.exit_scope();
        Some(typed_block_node)
    } else {
        None
    };

    Ok(Conditional::new(
        typed_condition,
        typed_then_node,
        typed_else_statement,
    ))
}

/// Analyzes a loop statement (While, For, Infinite).
///
/// Handles the specific child indexing defined in `statement.rs` for loops.
///
/// # Parameters
/// * `to_analyze` - The untyped loop structure.
/// * `context` - The syntax context for resolving expressions and traversing the loop body.
/// * `function_symbol_mapper` - Manages scopes for the loop body.
///
/// # Returns
/// * `Ok(Loop<TypedAST>)` if the loop structure and body are valid.
/// * `Err(SemanticError)` if analysis fails.
fn analyze_loop(
    to_analyze: &Loop<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<Loop<TypedAST>, SemanticError> {
    function_symbol_mapper.enter_scope();

    let inner_res = (|| -> Result<Loop<TypedAST>, SemanticError> {
        let typed_loop_type = match to_analyze.loop_type() {
            LoopType::Infinite => LoopType::Infinite,
            LoopType::While(condition) => {
                let typed_condition_expr =
                    analyze_expression(condition, context, function_symbol_mapper)?;
                let typed_condition = ASTNode::new(typed_condition_expr, *condition.position());

                if typed_condition.data_type() != DataType::Bool {
                    return Err(SemanticError::Custom {
                        message: "While condition must evaluate to a boolean".to_string(),
                        span: *condition.position(),
                    });
                }
                LoopType::While(typed_condition)
            }
            LoopType::For { cond, .. } => {
                let sth = context.ast_reference.get_child(0).unwrap();
                let start_context = context.with_ast_reference(&sth);
                let start_position = *start_context.ast_reference.inner().position();

                let typed_start_stmt = analyze_statement(&start_context, function_symbol_mapper)?;
                let typed_start_node = ASTNode::new(typed_start_stmt, start_position);

                let typed_cond_expr = analyze_expression(cond, context, function_symbol_mapper)?;
                let typed_cond_node = ASTNode::new(typed_cond_expr, *cond.position());

                if typed_cond_node.data_type() != DataType::Bool {
                    return Err(SemanticError::Custom {
                        message: "For condition must evaluate to a boolean".to_string(),
                        span: *cond.position(),
                    });
                }

                let sth = context.ast_reference.get_child(2).unwrap();
                let after_each_context = context.with_ast_reference(&sth);
                let after_each_position = *after_each_context.ast_reference.inner().position();

                let typed_after_each_stmt =
                    analyze_statement(&after_each_context, function_symbol_mapper)?;
                let typed_after_each_node =
                    ASTNode::new(typed_after_each_stmt, after_each_position);

                LoopType::For {
                    start: typed_start_node,
                    cond: typed_cond_node,
                    after_each: typed_after_each_node,
                }
            }
        };

        let body_index = if matches!(to_analyze.loop_type(), LoopType::For { .. }) {
            1
        } else {
            to_analyze.loop_type().len() - 1
        };

        let sth = context.ast_reference.get_child(body_index).unwrap();
        let to_loop_on_context = context.with_ast_reference(&sth);
        let to_loop_on_position = *to_loop_on_context.ast_reference.inner().position();

        let typed_to_loop_on_stmt = analyze_statement(&to_loop_on_context, function_symbol_mapper)?;
        let typed_to_loop_on = ASTNode::new(typed_to_loop_on_stmt, to_loop_on_position);

        Ok(Loop::new(typed_to_loop_on, typed_loop_type))
    })();

    let _ = function_symbol_mapper.exit_scope(); // Always executed!
    inner_res
}

fn analyze_if_enum_variant(
    to_analyze: &IfEnumVariant<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<IfEnumVariant<TypedAST>, SemanticError> {
    let condition_enum = analyze_enum_usage(
        &to_analyze.condition_enum().0,
        &to_analyze.condition_enum().1,
        context,
    )?;

    let untyped_enum_symbol = if let DirectlyAvailableSymbol::Enum(en) = symbol_by_name(
        &to_analyze.condition_enum().0,
        context.ast_reference.symbols_available_at(),
    )
    .ok_or_else(|| SemanticError::UnknownSymbol {
        name: to_analyze.condition_enum().0.clone(),
        span: source::types::FileID::from(0).span(0, 0),
    })? {
        en
    } else {
        return Err(SemanticError::Custom {
            message: "Not an enum type".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        });
    };

    let enum_variants = context
        .global_elements
        .get_enum_variants(untyped_enum_symbol, condition_enum.type_parameters())
        .ok_or_else(|| SemanticError::Custom {
            message: "Could not retrieve enum variants".to_string(),
            span: source::types::FileID::from(0).span(0, 0),
        })?;

    let enum_variant = enum_variants
        .iter()
        .find(|variant| variant.name() == to_analyze.condition_enum_variant())
        .ok_or_else(|| SemanticError::Custom {
            message: format!(
                "Enum variant '{}' not found",
                to_analyze.condition_enum_variant()
            ),
            span: source::types::FileID::from(0).span(0, 0),
        })?
        .clone();

    let typed_condition_expr = analyze_expression(
        to_analyze.assignment_expression(),
        context,
        function_symbol_mapper,
    )?;

    let typed_condition = ASTNode::new(
        typed_condition_expr,
        *to_analyze.assignment_expression().position(),
    );

    if typed_condition.data_type() != DataType::Bool {
        return Err(SemanticError::Custom {
            message: "Condition must evaluate to a boolean".to_string(),
            span: *to_analyze.assignment_expression().position(),
        });
    }

    function_symbol_mapper.enter_scope();
    let inner_res1 = (|| -> Result<_, SemanticError> {
        let variables = to_analyze
            .variables()
            .iter()
            .map(|var| {
                let declared_type_name = var.data_type();
                let resolved_declared_type = analyze_data_type(declared_type_name, context)?;

                let var_name = var.name().to_string();
                Ok(Rc::new(VariableSymbol::new(
                    var_name,
                    resolved_declared_type,
                )))
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        for var in &variables {
            if function_symbol_mapper.add_variable(var.clone()).is_err() {
                return Err(SemanticError::Custom {
                    message: "Variable already declared".to_string(),
                    span: source::types::FileID::from(0).span(0, 0),
                });
            }
        }

        function_symbol_mapper.enter_scope();
        let inner_res2 = (|| -> Result<_, SemanticError> {
            let sth = context.ast_reference.get_child(0).unwrap();
            let then_context = context.with_ast_reference(&sth);
            let then_position = *then_context.ast_reference.inner().position();

            let typed_then_statement = analyze_statement(&then_context, function_symbol_mapper)?;
            let typed_then_node = ASTNode::new(typed_then_statement, then_position);
            Ok((variables, typed_then_node))
        })();
        let _ = function_symbol_mapper.exit_scope(); // Exit inner
        inner_res2
    })();
    let _ = function_symbol_mapper.exit_scope(); // Exit outer

    let (variables, typed_then_node) = inner_res1?;

    IfEnumVariant::<TypedAST>::new(
        condition_enum,
        enum_variant,
        typed_condition,
        variables,
        typed_then_node,
    )
    .ok_or_else(|| SemanticError::Custom {
        message: "Failed to create if-enum-variant structure".to_string(),
        span: source::types::FileID::from(0).span(0, 0),
    })
}

/// Analyzes a code block (a list of statements).
///
/// Iterates through all statements in the block and recursively analyzes them.
/// Creates a new scope for the duration of the block.
///
/// # Parameters
/// * `context` - The syntax context pointing to the code block traversal helper.
/// * `function_symbol_mapper` - Context used to create a new scope for the block.
///
/// # Returns
/// * `Ok(CodeBlock<TypedAST>)` if all statements in the block are valid.
/// * `Err(SemanticError)` if any statement fails analysis.
fn analyze_codeblock(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<CodeBlock<TypedAST>, SemanticError> {
    function_symbol_mapper.enter_scope();

    let inner_res = (|| -> Result<CodeBlock<TypedAST>, SemanticError> {
        let mut typed_statements = Vec::new();
        let count = context.ast_reference.amount_children();

        for i in 0..count {
            // Unwrap:
            // This can never panic as we never reach or exceed the length of child statements
            let sth = context.ast_reference.get_child(i).unwrap();
            let child_context = context.with_ast_reference(&sth);
            let child_position = *child_context.ast_reference.inner().position();
            // Recursively analyze each statement
            let stmt = analyze_statement(&child_context, function_symbol_mapper)?;
            typed_statements.push(ASTNode::new(stmt, child_position));
        }
        Ok(CodeBlock::new(typed_statements))
    })();

    let _ = function_symbol_mapper.exit_scope();
    inner_res
}

/// Analyzes a break statement.
///
/// Traverses up the AST using the helper to ensure the break statement is inside a loop.
///
/// # Validation
/// Implicitly validates that the `break` statement occurs within a `ControlStructure::Loop`.
/// If the statement is nested within other structures (like conditionals) but eventually enclosed by a loop, it is valid.
/// If no enclosing loop is found, analysis returns `None`.
///
/// # Parameters
/// * `context` - The syntax context (used to check validity context by traversing parents).
///
/// # Returns
/// * `Ok(Statement::Break)` if inside a loop.
/// * `Err(SemanticError)` if used outside a loop.
fn analyze_break(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
) -> Result<Statement<TypedAST>, SemanticError> {
    let to_analyze = &context.ast_reference;
    let mut current_loc_opt = Some(to_analyze.location());

    while let Some(current_loc) = current_loc_opt {
        if let Statement::ControlStructure(crtl) = current_loc.referenced_statement()
            && let ControlStructure::Loop(_) = crtl.as_ref()
        {
            return Ok(Statement::Break);
        }
        current_loc_opt = current_loc.parent_statement();
    }

    Err(SemanticError::Custom {
        message: "Break statement used outside of a loop".to_string(),
        span: source::types::FileID::from(0).span(0, 0),
    })
}

fn analyze_struct_field_assignment(
    to_analyze: &StructFieldAssignment<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Result<StructFieldAssignment<TypedAST>, SemanticError> {
    let struct_source =
        analyze_expression(to_analyze.struct_source(), context, function_symbol_mapper)?;
    let struct_source = ASTNode::new(struct_source, *to_analyze.struct_source().position());

    let to_assign_to = if let DataType::Struct(st) = struct_source.data_type() {
        st
    } else {
        return Err(SemanticError::Custom {
            message: "Field assignment target must be a struct".to_string(),
            span: *to_analyze.struct_source().position(),
        });
    };

    let untyped_symbol = context
        .global_elements
        .untyped_struct_symbol_from_typed(&to_assign_to)
        .ok_or_else(|| SemanticError::Custom {
            message: "Internal Error: Untyped struct symbol missing".to_string(),
            span: *to_analyze.struct_source().position(),
        })?;

    let fields = context
        .global_elements
        .get_struct_fields(&untyped_symbol, to_assign_to.type_parameters())
        .ok_or_else(|| SemanticError::Custom {
            message: "Could not retrieve struct fields".to_string(),
            span: *to_analyze.struct_source().position(),
        })?;

    let field = fields
        .iter()
        .find(|field| field.name() == to_analyze.struct_field())
        .ok_or_else(|| SemanticError::Custom {
            message: format!("Field '{}' not found in struct", to_analyze.struct_field()),
            span: *to_analyze.struct_source().position(),
        })?
        .clone();

    let value = analyze_expression(to_analyze.value(), context, function_symbol_mapper)?;
    let value = ASTNode::new(value, *to_analyze.struct_source().position());

    StructFieldAssignment::<TypedAST>::new(struct_source, field, value).ok_or_else(|| {
        SemanticError::Custom {
            message: "Failed to create struct field assignment".to_string(),
            span: *to_analyze.struct_source().position(),
        }
    })
}
