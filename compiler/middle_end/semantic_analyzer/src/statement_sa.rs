use crate::expression_sa::analyze_expression;
use crate::mics_sa::{analyze_data_type, analyze_function_call};
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use crate::symbol::{SyntaxContext, TypeParameterContext};
use crate::symbol_by_name;
use ast::data_type::{DataType, Typed};
use ast::expression::{Expression, FunctionCall};
use ast::statement::{
    CodeBlock, Conditional, ControlStructure, Loop, LoopType, Return, Statement,
    VariableAssignment, VariableDeclaration,
};
use ast::symbol::{DirectlyAvailableSymbol, VariableSymbol};
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
/// # Parameters
/// * `to_analyze` - Traversal helper pointing to the statement to analyze (`StatementTraversalHelper<UntypedAST>`).
/// * `function_symbol_mapper` - Provides current function context (return type, local scopes) (`&mut FunctionSymbolMapper`).
/// * `global_map` - The global registry of typed function signatures (`&GlobalSymbolMap`).
///
/// # Returns
/// * `Some(Statement<TypedAST>)` if the statement and its children were successfully analyzed.
/// * `None` if a semantic error occurs (e.g., type mismatch, unknown variable).
pub(crate) fn analyze_statement(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Statement<TypedAST>> {
    let to_analyze = context.ast_reference.clone();
    let statement_to_analyze = to_analyze.inner();
    match statement_to_analyze.deref() {
        Statement::VariableAssignment(inner) => {
            let assigned = analyze_variable_assignment(inner, function_symbol_mapper, context)?;
            Some(Statement::VariableAssignment(assigned))
        }
        Statement::VariableDeclaration(inner) => {
            let declared = analyze_variable_declaration(inner, context, function_symbol_mapper)?;
            Some(Statement::VariableDeclaration(declared))
        }
        Statement::Expression(inner) => {
            let void_call = try_analyze_void_function_call(context, function_symbol_mapper);
            if let Some(inner) = void_call {
                Some(Statement::VoidFunctionCall(inner))
            } else {
                // We pass the helper as context so the expression can resolve symbols valid at this location
                let typed_expr = analyze_expression(inner, context, function_symbol_mapper)?;
                Some(Statement::Expression(ASTNode::new(
                    typed_expr,
                    inner.position().clone(),
                )))
            }
        }
        Statement::Return(inner) => {
            let typed_ret = analyze_return(inner, context, function_symbol_mapper)?;
            Some(Statement::Return(typed_ret))
        }
        Statement::ControlStructure(crtl) => {
            let typed_cs = Box::new(analyze_control_structure(
                crtl.as_ref(),
                context,
                function_symbol_mapper,
            )?);
            Some(Statement::ControlStructure(typed_cs))
        }
        Statement::Codeblock(_) => {
            let analyzed_cb = analyze_codeblock(context, function_symbol_mapper)?;
            Some(Statement::Codeblock(analyzed_cb))
        }
        Statement::VoidFunctionCall(_) => {
            panic!("Void function calls are not allowed in the untyped AST")
        }
        Statement::Break => analyze_break(context),
        _ => todo!(),
    }
}

fn try_analyze_void_function_call(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<FunctionCall<TypedAST>> {
    let to_analyze = &context.ast_reference;
    let statement_to_analyze = to_analyze.inner();
    let expr = if let Statement::Expression(inner) = statement_to_analyze.deref() {
        inner
    } else {
        return None;
    };
    let call = match expr.deref() {
        Expression::FunctionCall(call) => call,
        _ => return None,
    };

    let symbol = symbol_by_name(&call.function().0, to_analyze.symbols_available_at())?;

    if let DirectlyAvailableSymbol::Function(_) = symbol {
    } else {
        return None;
    };
    analyze_function_call(call, function_symbol_mapper, context)
}
/// Analyzes a variable assignment (re-assignment of an existing variable).
///
/// It checks if the variable exists in the current scope and if the type of the assigned value matches.
///
/// # Parameters
/// * `to_analyze` - The untyped assignment node.
/// * `function_symbol_mapper` - Used to look up the existing variable in the current scope.
/// * `helper` - The traversal helper providing the scope context for the value expression.
/// * `global_map` - Used for function lookups within the assigned expression.
///
/// # Returns
/// * `Some(VariableAssignment<TypedAST>)` if the variable exists and types match.
/// * `None` if the variable is not found or types mismatch.
fn analyze_variable_assignment(
    to_analyze: &VariableAssignment<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
) -> Option<VariableAssignment<TypedAST>> {
    let var_name = to_analyze.variable();
    let typed_variable_symbol = function_symbol_mapper.lookup_variable(var_name)?;

    let untyped_val = to_analyze.value();
    let typed_value_expr = analyze_expression(untyped_val, context, function_symbol_mapper)?;

    if typed_variable_symbol.data_type() != &typed_value_expr.data_type() {
        return None;
    }

    let typed_node = ASTNode::new(typed_value_expr, untyped_val.position().clone());
    VariableAssignment::<TypedAST>::new(typed_variable_symbol, typed_node)
}

/// Analyzes a variable declaration (creation of a new local variable).
///
/// It registers the new variable in the current scope and ensures the type of the
/// initializer matches the declared type.
///
/// # Parameters
/// * `to_analyze` - The untyped declaration node.
/// * `function_symbol_mapper` - Used to register the new variable in the current scope.
/// * `helper` - The traversal helper providing context for the value expression.
/// * `global_map` - Used for function lookups within the value expression.
///
/// # Returns
/// * `Some(VariableDeclaration<TypedAST>)` if the variable is successfully declared.
/// * `None` if the type cannot be inferred or resolved, or if registration fails.
fn analyze_variable_declaration(
    to_analyze: &VariableDeclaration<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<VariableDeclaration<TypedAST>> {
    let untyped_val = to_analyze.value();

    let typed_value_expr = analyze_expression(untyped_val, context, function_symbol_mapper)?;

    let declared_type_name = to_analyze.variable().data_type();
    let resolved_declared_type = analyze_data_type(declared_type_name, context)?;

    // Type Check
    if resolved_declared_type != typed_value_expr.data_type() {
        return None;
    }

    let var_name = to_analyze.variable().name().to_string();
    let typed_variable_symbol = Rc::new(VariableSymbol::new(var_name, resolved_declared_type));

    if function_symbol_mapper
        .add_variable(typed_variable_symbol.clone())
        .is_err()
    {
        return None;
    }

    let typed_node = ASTNode::new(typed_value_expr, untyped_val.position().clone());

    VariableDeclaration::<TypedAST>::new(typed_variable_symbol, typed_node)
}

/// Analyzes a return statement.
///
/// # Parameters
/// * `to_analyze` - The untyped return node.
/// * `function_symbol_mapper` - Used to check against the function's expected return type.
/// * `helper` - The traversal helper providing context for the returned expression.
/// * `global_map` - Used for function lookups within the returned expression.
///
/// # Returns
/// * `Some(Return<TypedAST>)` if the return value matches the function signature.
/// * `None` if types mismatch or the return value is invalid.
fn analyze_return(
    to_analyze: &Return<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Return<TypedAST>> {
    let expected_return_type = function_symbol_mapper
        .get_current_function_return_type()
        .cloned();
    let untyped_return_value = to_analyze.to_return();

    match (expected_return_type, untyped_return_value) {
        (None, None) => Some(Return::new(None)),

        (Some(expected), Some(expr_node)) => {
            let typed_expr = analyze_expression(expr_node, context, function_symbol_mapper)?;

            if typed_expr.data_type() != expected {
                return None;
            }

            let typed_node = ASTNode::new(typed_expr, expr_node.position().clone());
            Some(Return::new(Some(typed_node)))
        }

        _ => None,
    }
}

/// Analyzes a control structure (conditional or loop).
///
/// Delegates to `analyze_conditional` or `analyze_loop` respectively.
///
/// # Parameters
/// * `to_analyze_helper` - Traversal helper for the control structure, providing access to children blocks.
/// * `function_symbol_mapper` - Context for scope and variable management.
/// * `global_map` - Context for global function resolution.
///
/// # Returns
/// * `Some(ControlStructure<TypedAST>)` if the structure and its blocks are valid.
/// * `None` if analysis fails.
fn analyze_control_structure(
    to_analyze: &ControlStructure<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<ControlStructure<TypedAST>> {
    match to_analyze {
        ControlStructure::Conditional(cond) => {
            analyze_conditional(cond, context, function_symbol_mapper)
                .map(ControlStructure::Conditional)
        }
        ControlStructure::Loop(lp) => {
            analyze_loop(lp, context, function_symbol_mapper).map(ControlStructure::Loop)
        }
        _ => todo!(),
    }
}

/// Analyzes a conditional statement (if/else).
///
/// Recursively analyzes the condition expression and the 'then' and 'else' blocks.
///
/// # Parameters
/// * `to_analyze_helper` - Traversal helper for the if-statement.
/// * `function_symbol_mapper` - Manages scopes for the then/else blocks.
/// * `global_map` - Passed down for expression and statement analysis.
///
/// # Returns
/// * `Some(Conditional<TypedAST>)` if the condition is boolean and blocks are valid.
/// * `None` if analysis fails.
fn analyze_conditional(
    to_analyze: &Conditional<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Conditional<TypedAST>> {
    let untyped_condition = to_analyze.condition();

    let typed_condition_expr =
        analyze_expression(untyped_condition, context, function_symbol_mapper)?;

    let typed_condition = ASTNode::new(typed_condition_expr, untyped_condition.position().clone());

    if typed_condition.data_type() != DataType::Bool {
        return None;
    }

    function_symbol_mapper.enter_scope();
    // Unwrap:
    // A conditional always has a 0th statement (then-statement)
    let sth = context.ast_reference.get_child(0).unwrap();
    let mut then_context = context.with_ast_reference(&sth);
    let then_position = then_context.ast_reference.inner().position().clone();

    let typed_then_statement = analyze_statement(&mut then_context, function_symbol_mapper)?;
    let typed_then_node = ASTNode::new(typed_then_statement, then_position);
    let _ = function_symbol_mapper.exit_scope();

    let typed_else_statement = if to_analyze.else_statement().is_some() {
        function_symbol_mapper.enter_scope();
        // Unwrap:
        // We checked that the conditional has a 1st statement (else-statement)
        let sth = context.ast_reference.get_child(1).unwrap();
        let mut else_context = context.with_ast_reference(&sth);
        let else_position = else_context.ast_reference.inner().position().clone();

        let typed_block = analyze_statement(&mut else_context, function_symbol_mapper)?;
        let typed_block_node = ASTNode::new(typed_block, else_position);
        let _ = function_symbol_mapper.exit_scope();
        Some(typed_block_node)
    } else {
        None
    };

    Some(Conditional::new(
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
/// * `to_analyze_helper` - Traversal helper for the loop structure.
/// * `function_symbol_mapper` - Manages scopes for the loop body.
/// * `global_map` - Passed down for expression and statement analysis.
///
/// # Returns
/// * `Some(Loop<TypedAST>)` if the loop structure and body are valid.
/// * `None` if analysis fails.
fn analyze_loop(
    to_analyze: &Loop<UntypedAST>,
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Loop<TypedAST>> {
    function_symbol_mapper.enter_scope();

    let typed_loop_type = match to_analyze.loop_type() {
        LoopType::Infinite => LoopType::Infinite,

        LoopType::While(condition) => {
            let typed_condition_expr =
                analyze_expression(condition, context, function_symbol_mapper)?;
            let typed_condition = ASTNode::new(typed_condition_expr, condition.position().clone());

            if typed_condition.data_type() != DataType::Bool {
                let _ = function_symbol_mapper.exit_scope();
                return None;
            }
            LoopType::While(typed_condition)
        }

        LoopType::For {
            // We get start and after each separately to have a traversal helper
            start: _,
            cond,
            after_each: _,
        } => {
            // Unwrap:
            // A for loop always has a 0th substatement (before)
            let sth = context.ast_reference.get_child(0).unwrap();
            let mut start_context = context.with_ast_reference(&sth);
            let start_position = start_context.ast_reference.inner().position().clone();

            let typed_start_stmt = analyze_statement(&mut start_context, function_symbol_mapper)?;
            let typed_start_node = ASTNode::new(typed_start_stmt, start_position);

            let typed_cond_expr = analyze_expression(cond, context, function_symbol_mapper)?;
            let typed_cond_node = ASTNode::new(typed_cond_expr, cond.position().clone());

            if typed_cond_node.data_type() != DataType::Bool {
                let _ = function_symbol_mapper.exit_scope();
                return None;
            }

            // Unwrap:
            // A for loop always has a 2nd substatement (after)
            let sth = context.ast_reference.get_child(2).unwrap();
            let mut after_each_context = context.with_ast_reference(&sth);
            let after_each_position = after_each_context.ast_reference.inner().position().clone();

            let typed_after_each_stmt =
                analyze_statement(&mut after_each_context, function_symbol_mapper)?;
            let typed_after_each_node = ASTNode::new(typed_after_each_stmt, after_each_position);

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

    // Unwrap:
    // A for loop always has a statement at `body_index` as it is either:
    // 1. The statement is at position 1 of a foor loop
    // 2. The statement is the last child statement
    // Both of these must exist
    let sth = context.ast_reference.get_child(body_index).unwrap();
    let mut to_loop_on_context = context.with_ast_reference(&sth);
    let to_loop_on_position = to_loop_on_context.ast_reference.inner().position().clone();

    let typed_to_loop_on_stmt = analyze_statement(&mut to_loop_on_context, function_symbol_mapper)?;
    let typed_to_loop_on = ASTNode::new(typed_to_loop_on_stmt, to_loop_on_position);

    let _ = function_symbol_mapper.exit_scope();

    Some(Loop::new(typed_to_loop_on, typed_loop_type))
}

/// Analyzes a code block (a list of statements).
///
/// Iterates through all statements in the block and recursively analyzes them.
/// Creates a new scope for the duration of the block.
///
/// # Parameters
/// * `to_analyze_helper` - Traversal helper for the code block.
/// * `function_symbol_mapper` - Context used to create a new scope for the block.
/// * `global_map` - Passed down for nested analysis.
///
/// # Returns
/// * `Some(CodeBlock<TypedAST>)` if all statements in the block are valid.
/// * `None` if any statement fails analysis.
fn analyze_codeblock(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<CodeBlock<TypedAST>> {
    function_symbol_mapper.enter_scope();
    let mut typed_statements = Vec::new();
    let count = context.ast_reference.amount_children();

    for i in 0..count {
        // Unwrap:
        // This can never panic as we never reach or exceed the length of child statements
        let sth = context.ast_reference.get_child(i).unwrap();
        let mut child_context = context.with_ast_reference(&sth);
        let child_position = child_context.ast_reference.inner().position().clone();
        // Recursively analyze each statement
        if let Some(stmt) = analyze_statement(&mut child_context, function_symbol_mapper) {
            typed_statements.push(ASTNode::new(stmt, child_position));
        } else {
            let _ = function_symbol_mapper.exit_scope();
            return None;
        }
    }
    let _ = function_symbol_mapper.exit_scope();
    Some(CodeBlock::new(typed_statements))
}

/// Analyzes a break statement.
///
/// Traverses up the AST using the helper to ensure the break statement is inside a loop.
///
/// # Parameters
/// * `to_analyze` - Traversal helper (used to check validity context).
///
/// # Returns
/// * `Some(Statement::Break)` if inside a loop.
/// * `None` if used outside a loop.
fn analyze_break(
    context: &SyntaxContext<&StatementTraversalHelper<UntypedAST>>,
) -> Option<Statement<TypedAST>> {
    let to_analyze = &context.ast_reference;
    let mut current_loc_opt = Some(to_analyze.location());

    while let Some(current_loc) = current_loc_opt {
        if let Statement::ControlStructure(crtl) = current_loc.referenced_statement()
            && let ControlStructure::Loop(_) = crtl.as_ref()
        {
            return Some(Statement::Break);
        }
        current_loc_opt = current_loc.parent_statement();
    }
    None
}

#[cfg(test)]
mod tests {
    /*/// Tests that a return statement with no value (void) is successfully analyzed.
    /// It verifies that the analyzer accepts `return;` when the function signature expects void.
    #[test]
    fn analyze_return_ok_matching_void() {
        let ret_inner = Return::new(None);
        let stmt = Statement::Return(ret_inner);
        let stmt_node = ASTNode::new(stmt, sample_codearea());

        let body_block = CodeBlock::new(vec![stmt_node]);
        let body_stmt = Statement::Codeblock(body_block);
        let body_node = ASTNode::new(body_stmt, sample_codearea());

        let func_symbol_raw = FunctionSymbol::new("test_func".to_string(), None, Vec::new());
        let func_symbol = Rc::new(func_symbol_raw);

        let func = Function::new(func_symbol, body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let mock_context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &mock_context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let ret_ref = body_ref.get_child(0).unwrap();

        let inner_ret = match &**ret_ref.inner() {
            Statement::Return(r) => r,
            _ => panic!("Expected Return"),
        };

        let analyzed = analyze_return(inner_ret, &mut mapper, &ret_ref, &global_map);

        assert!(
            analyzed.is_some(),
            "Expected Some for void return without expression"
        );
        let analyzed_ret = analyzed.unwrap();
        assert!(analyzed_ret.to_return().is_none());
    }

    /// Tests that a return statement with a value matching the function signature is valid.
    /// It ensures that `return 42;` is accepted if the function expects `s32`.
    #[test]
    fn analyze_return_ok_matching_types() {
        let untyped_literal = Expression::Literal(String::from("42"));
        let untyped_literal_node = ASTNode::new(untyped_literal, sample_codearea());
        let untyped_return = Return::new(Some(untyped_literal_node));
        let stmt = Statement::Return(untyped_return);

        let stmt_node = ASTNode::new(stmt, sample_codearea());
        let body_node = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );

        let func_symbol_raw =
            FunctionSymbol::new("test_func".to_string(), Some("s32".to_string()), Vec::new());
        let func = Function::new(Rc::new(func_symbol_raw), body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let mock_context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &mock_context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        mapper.set_current_function_return_type(Some(DataType::S32));

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let ret_ref = body_ref.get_child(0).unwrap();

        let inner_ret = match &**ret_ref.inner() {
            Statement::Return(r) => r,
            _ => panic!("Expected Return"),
        };

        let result = analyze_return(inner_ret, &mut mapper, &ret_ref, &global_map);

        assert!(result.is_some(), "Should be successful and return");
        let typed_return = result.unwrap();

        // Fix E0716 by binding the option
        let ret_val = typed_return.to_return();
        let returned_expr = ret_val.as_ref().expect("Should contain a return type.");

        assert_eq!(DataType::S32, returned_expr.data_type());
    }

    /// Tests successful analysis of a conditional (if) statement with a valid boolean condition.
    /// It ensures the condition expression is typed as Bool and the block is analyzed.
    #[test]
    fn analyze_control_structure_conditional_ok() {
        let stmt_to_test = {
            let condition_expr = Expression::Literal(String::from("true"));
            let condition_node = ASTNode::new(condition_expr, sample_codearea());
            let then_block_node = ASTNode::new(
                Statement::Codeblock(CodeBlock::new(Vec::new())),
                sample_codearea(),
            );
            let conditional = Conditional::new(condition_node, then_block_node, None);
            Statement::ControlStructure(Box::new(ControlStructure::Conditional(conditional)))
        };

        let stmt_node = ASTNode::new(stmt_to_test, sample_codearea());
        let body_node = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("t".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let mock_context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &mock_context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let cs_helper = body_ref.get_child(0).unwrap();

        let analyzed = analyze_control_structure(cs_helper, &mut mapper, &global_map);
        assert!(analyzed.is_some());

        if let Some(ControlStructure::Conditional(c)) = analyzed {
            assert_eq!(c.condition().data_type(), DataType::Bool);
        } else {
            panic!("Expected Conditional variant");
        }
    }

    /// Tests successful analysis of a loop statement.
    /// It verifies that a `while(true)` loop is correctly accepted because the condition is a boolean.
    #[test]
    fn analyze_control_structure_loop_ok() {
        let stmt_to_test = {
            let condition_expr = Expression::Literal(String::from("true"));
            let condition_node = ASTNode::new(condition_expr, sample_codearea());
            let loop_body_node = ASTNode::new(
                Statement::Codeblock(CodeBlock::new(Vec::new())),
                sample_codearea(),
            );
            let loop_node = Loop::new(loop_body_node, LoopType::While(condition_node));
            Statement::ControlStructure(Box::new(ControlStructure::Loop(loop_node)))
        };

        let stmt_node = ASTNode::new(stmt_to_test, sample_codearea());
        let body_node = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("t".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let mock_context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &mock_context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let loop_helper = body_ref.get_child(0).unwrap();

        let analyzed = analyze_control_structure(loop_helper, &mut mapper, &global_map);
        assert!(analyzed.is_some());

        if let Some(ControlStructure::Loop(l)) = analyzed {
            match l.loop_type() {
                LoopType::While(cond_node) => {
                    assert_eq!(cond_node.data_type(), DataType::Bool);
                }
                _ => panic!("Expected While loop type"),
            }
        } else {
            panic!("Expected Loop variant");
        }
    }

    /// Validates that a code block containing valid statements is correctly analyzed.
    /// It creates a block with a single return statement and checks if the block is processed correctly.
    #[test]
    fn analyze_codeblock_ok() {
        let ret_stmt_inner: Statement<UntypedAST> = Statement::Return(Return::new(None));
        let ret_stmt_node = ASTNode::new(ret_stmt_inner, sample_codearea());
        let codeblock_inner = CodeBlock::new(vec![ret_stmt_node]);
        let stmt_to_test: Statement<UntypedAST> = Statement::Codeblock(codeblock_inner);

        let stmt_node = ASTNode::new(stmt_to_test, sample_codearea());
        let body_node = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("t".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let mock_context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &mock_context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);
        mapper.set_current_function_return_type(None);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let block_helper = body_ref.get_child(0).unwrap();

        let analyzed = analyze_codeblock(block_helper, &mut mapper, &global_map);
        assert!(analyzed.is_some());

        let cb = analyzed.unwrap();
        assert_eq!(cb.len(), 1);
        assert!(matches!(*cb[0], Statement::Return(_)));
    }

    /// Tests valid variable declaration where the initializer type matches the declared type.
    /// Example: `let x: s32 = 5;` should succeed.
    #[test]
    fn analyze_variable_declaration_basic_ok() {
        let untyped_literal_expr = Expression::Literal("5".to_string());
        let untyped_literal_node = ASTNode::new(untyped_literal_expr, sample_codearea());
        let untyped_var_decl = VariableDeclaration::<UntypedAST>::new(
            Rc::new(VariableSymbol::new("x".to_string(), "s32".to_string())),
            untyped_literal_node,
        );
        let stmt_to_analyze = Statement::VariableDeclaration(untyped_var_decl);

        let stmt_node = ASTNode::new(stmt_to_analyze, sample_codearea());
        let body_node = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("t".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let mock_context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &mock_context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let decl_helper = body_ref.get_child(0).unwrap();

        let analyzed_stmt = analyze_statement(decl_helper, &mut mapper, &global_map);
        assert!(analyzed_stmt.is_some());
    }

    /// Tests valid variable assignment, ensuring the variable exists and the value type matches.
    /// Example: `x = 10;` should succeed if x is declared as s32.
    #[test]
    fn analyze_variable_assignment_basic_ok() {
        let var_name = "x".to_string();
        let untyped_value_expr = Expression::Literal("10".to_string());
        let untyped_value_node = ASTNode::new(untyped_value_expr, sample_codearea());
        let untyped_assignment =
            VariableAssignment::<UntypedAST>::new(var_name.clone(), untyped_value_node);
        let stmt_to_analyze = Statement::VariableAssignment(untyped_assignment);

        let stmt_node = ASTNode::new(stmt_to_analyze, sample_codearea());
        let body_node = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("t".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body_node, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let mock_context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &mock_context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let x_symbol = Rc::new(VariableSymbol::new(var_name, DataType::S32));
        mapper
            .add_variable(x_symbol.clone())
            .expect("Could not add variable.");

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let assign_helper = body_ref.get_child(0).unwrap();

        let analyzed_stmt = analyze_statement(assign_helper, &mut mapper, &global_map);
        assert!(analyzed_stmt.is_some());

        if let Some(Statement::VariableAssignment(typed_assignment)) = analyzed_stmt {
            assert_eq!(typed_assignment.variable().name(), x_symbol.name());
            assert_eq!(typed_assignment.value().data_type(), DataType::S32);
        } else {
            panic!("Wrong statement type");
        }
    }

    /// Verifies that a call to a void function is correctly accepted as a statement.
    /// It mocks a global map containing a void function 'log_message' and checks if calling it works.
    #[test]
    fn analyze_void_function_call_ok() {
        let func_name = "log_message";
        let param_name = "msg";

        let param = Rc::new(VariableSymbol::new(
            param_name.to_string(),
            "s32".to_string(),
        ));

        let func_symbol_log_raw = FunctionSymbol::new(func_name.to_string(), None, vec![param]);
        let func_symbol_log_rc = Rc::new(func_symbol_log_raw.clone());

        let body_log = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![])),
            sample_codearea(),
        );
        let func_log = Function::new(func_symbol_log_rc, body_log, Visibility::Private);

        let arg = ASTNode::new(Expression::Literal("5".to_string()), sample_codearea());
        let untyped_call = FunctionCall::<UntypedAST>::new(func_name.to_string(), vec![arg]);
        let stmt_call = Statement::VoidFunctionCall(untyped_call);
        let stmt_node = ASTNode::new(stmt_call, sample_codearea());

        let body_main = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );
        let func_main_raw = FunctionSymbol::new("main".into(), None, vec![]);
        let func_main = Function::new(Rc::new(func_main_raw), body_main, Visibility::Private);

        let ast = functions_into_ast(vec![
            ASTNode::new(func_log, sample_codearea()),
            ASTNode::new(func_main, sample_codearea()),
        ]);

        // Global Map: Raw Key -> Typed Value
        let mut global_map = GlobalSymbolMap::new();
        let typed_param = Rc::new(VariableSymbol::new(param_name.to_string(), DataType::S32));
        let typed_func_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            func_name.to_string(),
            None,
            vec![typed_param],
        ));
        global_map.insert(func_symbol_log_raw, typed_func_symbol);

        let context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();

        let func_ref = file_ref.index_function(1);
        let body_ref = func_ref.ref_to_implementation();
        let call_helper = body_ref.get_child(0).unwrap();

        let inner_call = match &**call_helper.inner() {
            Statement::VoidFunctionCall(c) => c,
            _ => panic!("Expected VoidFunctionCall"),
        };

        let analyzed_stmt =
            analyze_void_function_call(inner_call, &mut mapper, &call_helper, &global_map);

        assert!(
            analyzed_stmt.is_some(),
            "Expected void function call analysis to succeed."
        );
    }

    /// Ensures that calling a non-void function as a standalone statement fails analysis.
    /// This prevents discarding return values implicitly, e.g., using `add(1, 2)` as a statement.
    #[test]
    fn analyze_void_function_call_must_be_void_fail() {
        let func_name = "add";

        let func_symbol_add_raw =
            FunctionSymbol::new(func_name.to_string(), Some("s32".to_string()), vec![]);
        let func_symbol_add_rc = Rc::new(func_symbol_add_raw.clone());
        let func_add = Function::new(
            func_symbol_add_rc,
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![])),
                sample_codearea(),
            ),
            Visibility::Private,
        );

        let untyped_call = FunctionCall::<UntypedAST>::new(func_name.to_string(), Vec::new());
        let stmt_call = Statement::VoidFunctionCall(untyped_call);
        let stmt_node = ASTNode::new(stmt_call, sample_codearea());

        let func_main_raw = FunctionSymbol::new("main".to_string(), None, vec![]);
        let func_main = Function::new(
            Rc::new(func_main_raw),
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
                sample_codearea(),
            ),
            Visibility::Private,
        );

        let ast = functions_into_ast(vec![
            ASTNode::new(func_add, sample_codearea()),
            ASTNode::new(func_main, sample_codearea()),
        ]);

        let mut global_map = GlobalSymbolMap::new();
        let typed_func_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            func_name.to_string(),
            Some(DataType::S32),
            Vec::new(),
        ));
        global_map.insert(func_symbol_add_raw, typed_func_symbol);

        let context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(1);
        let body_ref = func_ref.ref_to_implementation();
        let call_helper = body_ref.get_child(0).unwrap();

        let inner_call = match &**call_helper.inner() {
            Statement::VoidFunctionCall(c) => c,
            _ => panic!("Expected Call"),
        };

        let analyzed_stmt =
            analyze_void_function_call(inner_call, &mut mapper, &call_helper, &global_map);

        assert!(
            analyzed_stmt.is_none(),
            "Expected analysis to fail because 'add' returns S32, but was used as a Void-Statement."
        );
    }

    /// Checks that a break statement is valid when placed inside a loop.
    /// It traverses the AST to ensure the `StatementTraversalHelper` can correctly identify the parent loop.
    #[test]
    fn analyze_break_ok_inside_loop() {
        let break_node = ASTNode::new(Statement::Break, sample_codearea());

        let loop_body_stmt = Statement::Codeblock(CodeBlock::new(vec![break_node]));
        let loop_body_node = ASTNode::new(loop_body_stmt, sample_codearea());

        let condition_node =
            ASTNode::new(Expression::Literal("true".to_string()), sample_codearea());
        let loop_struct = Loop::new(loop_body_node, LoopType::While(condition_node));
        let loop_stmt = Statement::ControlStructure(Box::new(ControlStructure::Loop(loop_struct)));
        let loop_node = ASTNode::new(loop_stmt, sample_codearea());

        let func_body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![loop_node])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("t".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), func_body, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let func_body_ref = func_ref.ref_to_implementation();
        let loop_ref = func_body_ref.get_child(0).unwrap();

        let loop_body_ref = loop_ref.get_child(0).unwrap();
        let break_ref = loop_body_ref.get_child(0).unwrap();

        let analyzed = analyze_statement(break_ref, &mut mapper, &global_map);

        assert!(
            analyzed.is_some(),
            "Expected loop containing break to analyze successfully"
        );
    }*/
}
