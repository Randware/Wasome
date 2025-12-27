use crate::expression_sa::{analyze_expression, analyze_function_call};
use crate::function_symbol_mapper::FunctionSymbolMapper;
use crate::mics_sa::analyze_data_type;
use ast::block::CodeBlock;
use ast::data_type::{DataType, Typed};
use ast::statement::{
    Conditional, ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
    VariableDeclaration,
};
use ast::symbol::{FunctionCall, VariableSymbol};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::rc::Rc;

/// Analyzes a statement referenced by a traversal helper and converts it into a typed statement node.
///
/// # Parameters
/// * `to_analyze` - Traversal helper pointing to the statement to analyze (`StatementTraversalHelper<UntypedAST>`).
/// * `function_symbol_mapper` - Provides current function return type and scope context for validation (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Some(Statement<TypedAST>)` if the statement (and any nested expressions/statements) could be successfully analyzed.
/// * `None` if analysis fails (type errors, invalid constructs, or other semantic errors).
pub(crate) fn analyze_statement(
    to_analyze: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Statement<TypedAST>> {
    match &**to_analyze.get_inner() {
        Statement::VariableAssignment(inner) => {
            let assigned_variable = analyze_variable_assignment(inner, function_symbol_mapper)?;
            Some(Statement::VariableAssignment(assigned_variable))
        }
        Statement::VariableDeclaration(inner) => {
            let declared_variable = analyze_variable_declaration(inner, function_symbol_mapper)?;
            Some(Statement::VariableDeclaration(declared_variable))
        }
        Statement::Expression(inner) => {
            let typed_expr = analyze_expression(inner, function_symbol_mapper)?;
            let pos = inner.position().clone();
            Some(Statement::Expression(ASTNode::new(typed_expr, pos)))
        }
        Statement::Return(inner) => {
            let typed_return = analyze_return(inner, function_symbol_mapper)?;
            Some(Statement::Return(typed_return))
        }
        Statement::ControlStructure(_) => {
            let typed_cs = Box::new(analyze_control_structure(
                to_analyze,
                function_symbol_mapper,
            )?);
            Some(Statement::ControlStructure(typed_cs))
        }
        Statement::Codeblock(_) => {
            let analyzed_cb = analyze_codeblock(to_analyze, function_symbol_mapper)?;
            Some(Statement::Codeblock(analyzed_cb))
        }
        Statement::VoidFunctionCall(inner) => {
            analyze_void_function_call(inner, function_symbol_mapper)
        }
        Statement::Break => analyze_break(to_analyze),
    }
}

/// Analyzes an untyped `VariableAssignment` statement and converts it into a typed statement.
///
/// This function resolves the target variable, analyzes the assigned expression value,
/// and performs type checking to ensure the expression's return type matches the variable's declared type.
///
/// # Parameters
/// * `to_analyze` - The untyped `VariableAssignment` statement reference.
/// * `function_symbol_mapper` - The mapper used for resolving the variable symbol, managing scope, and analyzing the value expression.
///
/// # Returns
/// * `Some(VariableAssignment<TypedAST>)` on success.
/// * `None` if the target variable is undeclared or a type mismatch occurs.
fn analyze_variable_assignment<'a>(
    to_analyze: &VariableAssignment<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper<'a>,
) -> Option<VariableAssignment<TypedAST>> {
    let var_name = to_analyze.variable();

    let typed_variable_symbol = function_symbol_mapper.lookup_variable(var_name)?;

    let untyped_value_node_ref = to_analyze.value();

    let position = untyped_value_node_ref.position().clone();

    let typed_value_expr = analyze_expression(untyped_value_node_ref, function_symbol_mapper)?;

    let typed_value_node = ASTNode::new(typed_value_expr, position);

    VariableAssignment::<TypedAST>::new(typed_variable_symbol, typed_value_node)
}

/// Analyzes a `VariableDeclaration` and converts it into a typed statement node.
///
/// # Parameters
/// * `to_analyze` - The untyped `VariableDeclaration` statement.
/// * `function_symbol_mapper` - The mapper for scope and type registration.
///
/// # Returns
/// * `Some(VariableDeclaration<TypedAST>)` on success.
/// * `None` on semantic error.
fn analyze_variable_declaration<'a>(
    to_analyze: &VariableDeclaration<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper<'a>,
) -> Option<VariableDeclaration<TypedAST>> {
    let untyped_initializer_node_ref = to_analyze.value();

    let position = untyped_initializer_node_ref.position().clone();

    let typed_initializer_expr =
        analyze_expression(untyped_initializer_node_ref, function_symbol_mapper)?;

    let typed_initializer_node = ASTNode::new(typed_initializer_expr, position);

    let declared_type_name = to_analyze.variable().data_type();
    let resolved_declared_type = analyze_data_type(declared_type_name)?;

    let var_name = to_analyze.variable().name().to_string();
    let typed_variable_symbol = Rc::new(VariableSymbol::new(
        var_name.clone(),
        resolved_declared_type.clone(),
    ));

    function_symbol_mapper
        .add_variable(typed_variable_symbol.clone())
        .ok()?;

    let typed_declaration =
        VariableDeclaration::<TypedAST>::new(typed_variable_symbol, typed_initializer_node)?;

    Some(typed_declaration)
}

/// Analyzes a `Return` statement and converts it into a typed `Return` node.
///
/// # Parameters
/// * `to_analyze` - The untyped `Return` statement to analyze (`&Return<UntypedAST>`).
/// * `function_symbol_mapper` - Provides the current function return type and scope context for validation (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Some(Return<TypedAST>)` if the `Return` (and its inner expression, if present) can be analyzed and types match.
/// * `None` if analysis fails, a type mismatch occurs, or an invalid return is found (e.g., value returned from void function or missing value for non-void function).
fn analyze_return(
    to_analyze: &Return<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Return<TypedAST>> {
    let expected_type = function_symbol_mapper.get_current_function_return_type();

    let untyped_expr_option = to_analyze.to_return();

    match (expected_type, untyped_expr_option) {
        (None, Some(_untyped_expr_ref)) => None,

        (Some(_expected), None) => None,

        (None, None) => Some(Return::new(None)),

        (Some(expected), Some(untyped_expr_ref)) => {
            let typed_expr = analyze_expression(untyped_expr_ref, function_symbol_mapper)?;
            let typed_node = ASTNode::new(typed_expr, untyped_expr_ref.position().clone());
            let actual_type = typed_node.data_type();

            if actual_type != expected {
                return None;
            }

            Some(Return::new(Some(typed_node)))
        }
    }
}

/// Analyzes a control-structure statement referenced by a traversal helper and returns a typed control structure.
///
/// # Parameters
/// * `to_analyze_helper` - Traversal helper pointing to the control-structure statement (`StatementTraversalHelper<UntypedAST>`).
/// * `function_symbol_mapper` - Provides scope context and supports nested statement analysis (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Some(ControlStructure<TypedAST>)` if the contained control structure (conditional or loop) was successfully analyzed.
/// * `None` if the helper does not point to a control structure or if nested analysis fails.
fn analyze_control_structure(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<ControlStructure<TypedAST>> {
    let inner_control_structure = match &**to_analyze_helper.get_inner() {
        Statement::ControlStructure(cs) => cs,
        _ => return None, //"Expected a ControlStructure statement.",
    };

    match **inner_control_structure {
        ControlStructure::Conditional(_) => {
            analyze_conditional(to_analyze_helper, function_symbol_mapper)
                .map(ControlStructure::Conditional)
        }
        ControlStructure::Loop(_) => {
            analyze_loop(to_analyze_helper, function_symbol_mapper).map(ControlStructure::Loop)
        }
    }
}

/// Analyzes a `Conditional` control structure (if/then/else) and converts it into a typed `Conditional` node.
///
/// # Parameters
/// * `to_analyze_helper` - Traversal helper positioned at the `Conditional` statement (`StatementTraversalHelper<UntypedAST>`).
/// * `function_symbol_mapper` - Used to validate the condition type and manage scopes for then/else branches (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Some(Conditional<TypedAST>)` if the condition and both branches (when present) are semantically valid and typed.
/// * `None` if the helper is not a `Conditional`, the condition is not boolean, or nested branch analysis fails.
fn analyze_conditional(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Conditional<TypedAST>> {
    let inner_box_ref = match &**to_analyze_helper.get_inner() {
        Statement::ControlStructure(b) => b,
        _ => return None,
    };

    let untyped_conditional_ref: &Conditional<UntypedAST> = match **inner_box_ref {
        ControlStructure::Conditional(ref c) => c,
        ControlStructure::Loop(_) => return None,
    };

    let untyped_condition = untyped_conditional_ref.condition();
    let typed_condition_expr = analyze_expression(untyped_condition, function_symbol_mapper)?;
    let typed_condition = ASTNode::new(typed_condition_expr, untyped_condition.position().clone());

    if typed_condition.data_type() != DataType::Bool {
        return None;
    }

    function_symbol_mapper.enter_scope();
    let then_helper = to_analyze_helper.index(0);
    let then_position = then_helper.get_inner().position().clone();
    let typed_then_statement = analyze_statement(then_helper, function_symbol_mapper)?;
    let typed_then_node = ASTNode::new(typed_then_statement, then_position);
    function_symbol_mapper.exit_scope();

    let typed_else_statement = if untyped_conditional_ref.else_statement().is_some() {
        function_symbol_mapper.enter_scope();
        let else_helper = to_analyze_helper.index(1);
        let else_position = else_helper.get_inner().position().clone();
        let typed_block = analyze_statement(else_helper, function_symbol_mapper)?;
        let typed_block_node = ASTNode::new(typed_block, else_position);
        function_symbol_mapper.exit_scope();
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

/// Analyzes a `Loop` control structure and converts it into a typed `Loop` node.
///
/// # Parameters
/// * `to_analyze_helper` - Traversal helper positioned at the `Loop` statement (`StatementTraversalHelper<UntypedAST>`).
/// * `function_symbol_mapper` - Used to validate loop components and manage a new scope for loop body and headers (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Some(Loop<TypedAST>)` if the loop header (while/for) and body are semantically valid and typed.
/// * `None` if the helper is not a `Loop`, a condition is not boolean, or nested analysis fails.
fn analyze_loop(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Loop<TypedAST>> {
    let inner_box_ref = match &**to_analyze_helper.get_inner() {
        Statement::ControlStructure(b) => b,
        _ => return None,
    };
    let untyped_loop_ref: &Loop<UntypedAST> = match **inner_box_ref {
        ControlStructure::Loop(ref l) => l,
        _ => return None,
    };

    function_symbol_mapper.enter_scope();

    let typed_loop_type = match untyped_loop_ref.loop_type() {
        LoopType::Infinite => LoopType::Infinite,

        LoopType::While(condition) => {
            let typed_condition_expr = analyze_expression(condition, function_symbol_mapper)?;
            let typed_condition = ASTNode::new(typed_condition_expr, condition.position().clone());

            if typed_condition.data_type() != DataType::Bool {
                function_symbol_mapper.exit_scope();
                return None;
            }
            LoopType::While(typed_condition)
        }

        LoopType::For {
            start,
            cond,
            after_each,
        } => {
            let start_helper = to_analyze_helper.index(0);
            let start_position = start_helper.get_inner().position().clone();
            let typed_start_stmt = analyze_statement(start_helper, function_symbol_mapper)?;
            let typed_start_node = ASTNode::new(typed_start_stmt, start_position);

            let typed_cond_expr = analyze_expression(cond, function_symbol_mapper)?;
            let typed_cond_node = ASTNode::new(typed_cond_expr, cond.position().clone());
            if typed_cond_node.data_type() != DataType::Bool {
                function_symbol_mapper.exit_scope();
                return None;
            }

            let after_each_helper = to_analyze_helper.index(1);
            let after_each_position = after_each_helper.get_inner().position().clone();
            let typed_after_each_stmt =
                analyze_statement(after_each_helper, function_symbol_mapper)?;
            let typed_after_each_node = ASTNode::new(typed_after_each_stmt, after_each_position);

            LoopType::For {
                start: typed_start_node,
                cond: typed_cond_node,
                after_each: typed_after_each_node,
            }
        }
    };

    let to_loop_on_index = untyped_loop_ref.loop_type().len();
    let to_loop_on_helper = to_analyze_helper.index(to_loop_on_index);
    let loop_body_position = to_loop_on_helper.get_inner().position().clone();
    let typed_to_loop_on_stmt = analyze_statement(to_loop_on_helper, function_symbol_mapper)?;
    let typed_to_loop_on = ASTNode::new(typed_to_loop_on_stmt, loop_body_position);

    function_symbol_mapper.exit_scope();

    Some(Loop::new(typed_to_loop_on, typed_loop_type))
}

/// Analyzes a code block referenced by a traversal helper and converts it into a typed `CodeBlock` node.
///
/// # Parameters
/// * `to_analyze_helper` - Traversal helper positioned at the code block to analyze (`StatementTraversalHelper<UntypedAST>`).
/// * `function_symbol_mapper` - Provides scope management and context for nested statement analysis (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Some(CodeBlock<TypedAST>)` containing typed statements if all child statements were successfully analyzed.
/// * `None` if any child statement fails semantic analysis or a nested error occurs.
fn analyze_codeblock(
    to_analyze_helper: StatementTraversalHelper<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<CodeBlock<TypedAST>> {
    function_symbol_mapper.enter_scope();

    let mut typed_statements: Vec<ASTNode<Statement<TypedAST>>> = Vec::new();
    let statement_count = to_analyze_helper.child_len();

    for i in 0..statement_count {
        let child_helper = to_analyze_helper.index(i);

        let position = child_helper.get_inner().position().clone();

        if let Some(typed_statement) = analyze_statement(child_helper, function_symbol_mapper) {
            let node = ASTNode::new(typed_statement, position);
            typed_statements.push(node);
        } else {
            function_symbol_mapper
                .exit_scope()
                .expect("Internal Compiler Error: Scope stack imbalance during error recovery.");
            return None;
        }
    }

    function_symbol_mapper
        .exit_scope()
        .expect("Internal Compiler Error: Scope stack imbalance after successful block analysis.");

    Some(CodeBlock::new(typed_statements))
}

/// Analyzes an untyped function call intended to be used as a standalone statement (for side effects).
///
/// This function resolves the function symbol, recursively analyzes the arguments, and critically validates
/// that the called function explicitly returns **no value (void)**.
///
/// # Parameters
/// * `to_analyze` - The untyped function call structure (`&FunctionCall<UntypedAST>`).
/// * `function_symbol_mapper` - Provides symbol resolution for the function and context for argument analysis (`&mut FunctionSymbolMapper`).
///
/// # Returns
/// * `Some(Statement<TypedAST>)` wrapping a VoidFunctionCall if the function is found, arguments are valid, and the return type is None (void).
/// * `None` if the function is undeclared, arguments fail semantic analysis, or the function has a return type (Non-Void).
fn analyze_void_function_call(
    to_analyze: &FunctionCall<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
) -> Option<Statement<TypedAST>> {
    let typed_call = analyze_function_call(to_analyze, function_symbol_mapper)?;

    if typed_call.function().return_type().is_some() {
        return None;
    }
    Some(Statement::VoidFunctionCall(typed_call))
}

/// Analyzes a `Break` statement.
///
/// Validates that the break is used inside a loop context by traversing the AST ancestry
/// via the StatementLocation chain.
fn analyze_break(to_analyze: StatementTraversalHelper<UntypedAST>) -> Option<Statement<TypedAST>> {
    let root = to_analyze.root_helper();

    let mut current_loc_opt = to_analyze.location();

    while let Some(current_loc) = current_loc_opt {
        if let Some(parent_loc) = current_loc.prev() {
            let parent_node = root.index_implementation(parent_loc);

            let statement = parent_node;

            if let Statement::ControlStructure(cs_box) = statement {
                if matches!(cs_box.as_ref(), ControlStructure::Loop(_)) {
                    return Some(Statement::Break);
                }
            }

            current_loc_opt = Some(parent_loc);
        } else {
            return None;
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression_sa::sample_codearea;
    use crate::file_symbol_mapper::FileSymbolMapper;
    use ast::data_type::DataType;
    use ast::expression::{Expression, Literal};
    use ast::statement::Return;
    use ast::symbol::FunctionSymbol;
    use ast::top_level::{Function, TopLevelElement};
    use ast::traversal::function_traversal::FunctionTraversalHelper;
    use ast::{AST, UntypedAST};
    use std::rc::Rc;

    /** Tests the successful semantic analysis of a return statement without an expression,
     * ensuring it is correctly typed as void.
     */
    #[test]
    fn analyze_return_ok_matching_void() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let ret: Return<UntypedAST> = Return::new(None);
        let analyzed = analyze_return(&ret, &mut mapper);
        assert!(
            analyzed.is_some(),
            "Expected Some for void return without expression"
        );
        let analyzed_ret = analyzed.unwrap();
        assert!(
            analyzed_ret.to_return().is_none(),
            "Expected returned Return to contain no expression"
        );
    }

    /** Tests the successful semantic analysis of a return statement containing a literal expression,
     * verifying that the expression is analyzed and typed (S32) and matches the function's required return type.
     */
    #[test]
    fn analyze_return_ok_matching_types() {
        let expected_type = DataType::S32;
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        mapper.set_current_function_return_type(Some(expected_type));

        let untyped_literal = Expression::Literal(String::from("42"));

        let untyped_literal_node = ASTNode::new(untyped_literal, sample_codearea());

        let untyped_return = Return::new(Some(untyped_literal_node));

        let result = analyze_return(&untyped_return, &mut mapper);

        assert!(result.is_some(), "Should be successful and return");

        let typed_return = result.unwrap();

        let actual_returned_type = typed_return
            .return_type()
            .expect("Should contain a return type.");

        assert_eq!(
            expected_type, actual_returned_type,
            "The actual returned type of the statement should be S32."
        );

        assert!(
            matches!(**typed_return.to_return().unwrap(), Expression::Literal(_)),
            "The returned expression should be a typed S32 literal."
        );
    }

    /** Tests the successful semantic analysis of a basic Conditional (if statement),
     * ensuring the literal condition expression ("true") is correctly analyzed and typed as Bool.
     */
    #[test]
    fn analyze_control_structure_conditional_ok() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let stmt_to_test = {
            let condition_expr = Expression::Literal(String::from("true"));
            let condition_node = ASTNode::new(condition_expr, sample_codearea());

            let then_block_inner = CodeBlock::new(Vec::new());
            let then_block_stmt = Statement::Codeblock(then_block_inner);
            let then_block_node = ASTNode::new(then_block_stmt, sample_codearea());

            let conditional = Conditional::new(condition_node, then_block_node, None);

            let cs_inner = ControlStructure::Conditional(conditional);
            let cs_box = Box::new(cs_inner);

            Statement::ControlStructure(cs_box)
        };

        let stmt_to_test_node = ASTNode::new(stmt_to_test, sample_codearea());

        let func_symbol = Rc::new(FunctionSymbol::new(
            "test_conditional".to_string(),
            None,
            Vec::new(),
        ));
        let func = Function::new(func_symbol, stmt_to_test_node);
        let ast = AST::new(vec![ASTNode::new(
            TopLevelElement::Function(func),
            sample_codearea(),
        )]);

        let func_ref = FunctionTraversalHelper::new(ast.functions().next().unwrap(), &ast);
        let helper = func_ref.ref_to_implementation();

        let analyzed = analyze_control_structure(helper, &mut mapper);
        assert!(
            analyzed.is_some(),
            "Expected conditional to analyze successfully"
        );

        if let Some(ControlStructure::Conditional(c)) = analyzed {
            assert_eq!(c.condition().data_type(), DataType::Bool);
        } else {
            panic!("Expected Conditional variant");
        }
    }

    /** Tests the successful semantic analysis of a basic While Loop,
     * verifying that the literal condition expression ("true") is correctly analyzed and typed as Bool.
     */
    #[test]
    fn analyze_control_structure_loop_ok() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let stmt_to_test = {
            let condition_expr = Expression::Literal(String::from("true"));
            let condition_node = ASTNode::new(condition_expr, sample_codearea());

            let loop_body_inner = CodeBlock::new(Vec::new());
            let loop_body_stmt = Statement::Codeblock(loop_body_inner);
            let loop_body_node = ASTNode::new(loop_body_stmt, sample_codearea());

            let loop_node = Loop::new(loop_body_node, LoopType::While(condition_node));

            let cs_inner = ControlStructure::Loop(loop_node);
            let cs_box = Box::new(cs_inner);

            Statement::ControlStructure(cs_box)
        };

        let stmt_to_test_node = ASTNode::new(stmt_to_test, sample_codearea());

        let func_symbol = Rc::new(FunctionSymbol::new(
            "test_loop".to_string(),
            None,
            Vec::new(),
        ));
        let func = Function::new(func_symbol, stmt_to_test_node);
        let ast = AST::new(vec![ASTNode::new(
            TopLevelElement::Function(func),
            sample_codearea(),
        )]);

        let func_ref = FunctionTraversalHelper::new(ast.functions().next().unwrap(), &ast);
        let helper = func_ref.ref_to_implementation();

        let analyzed = analyze_control_structure(helper, &mut mapper);
        assert!(analyzed.is_some(), "Expected loop to analyze successfully");

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

    /** Tests the successful semantic analysis of a CodeBlock containing a single void Return statement,
     * ensuring the code block structure is preserved and its internal statements are analyzed.
     */
    #[test]
    fn analyze_codeblock_ok() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        mapper.set_current_function_return_type(None);

        let stmt_to_test = {
            let ret_stmt_inner: Statement<UntypedAST> = Statement::Return(Return::new(None));

            let ret_stmt_node = ASTNode::new(ret_stmt_inner, sample_codearea());

            let codeblock_inner = CodeBlock::new(vec![ret_stmt_node]);

            let codeblock_stmt: Statement<UntypedAST> = Statement::Codeblock(codeblock_inner);

            ASTNode::new(codeblock_stmt, sample_codearea())
        };

        let func_symbol = Rc::new(FunctionSymbol::new(
            "test_codeblock".to_string(),
            None,
            Vec::new(),
        ));

        let func = Function::new(func_symbol, stmt_to_test);
        let ast = AST::new(vec![ASTNode::new(
            TopLevelElement::Function(func),
            sample_codearea(),
        )]);

        let func_ref = FunctionTraversalHelper::new(ast.functions().next().unwrap(), &ast);
        let helper = func_ref.ref_to_implementation();

        let analyzed = analyze_codeblock(helper, &mut mapper);
        assert!(
            analyzed.is_some(),
            "Expected codeblock to analyze successfully"
        );

        let cb = analyzed.unwrap();

        assert_eq!(cb.len(), 1, "Expected one statement in typed codeblock");

        assert!(matches!(*cb[0], Statement::Return(_)));
    }

    /** Tests the successful semantic analysis of a basic VariableDeclaration (e.g., x: s32 = 5),
     * ensuring the assignment expression is successfully typed and the declaration is semantically valid.
     */
    #[test]
    fn analyze_variable_declaration_basic_ok() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let untyped_literal_expr = Expression::Literal("5".to_string());
        let untyped_literal_node = ASTNode::new(untyped_literal_expr, sample_codearea());

        let untyped_var_decl = VariableDeclaration::<UntypedAST>::new(
            Rc::new(VariableSymbol::new("x".to_string(), "s32".to_string())),
            untyped_literal_node,
        );

        let stmt_to_analyze = Statement::VariableDeclaration(untyped_var_decl);

        let stmt_to_analyze_node = ASTNode::new(stmt_to_analyze, sample_codearea());

        let func_symbol = Rc::new(FunctionSymbol::new("main".to_string(), None, Vec::new()));

        let implementation_block_inner = CodeBlock::new(vec![stmt_to_analyze_node]);
        let implementation_block_stmt = Statement::Codeblock(implementation_block_inner);

        let implementation_block_node = ASTNode::new(implementation_block_stmt, sample_codearea());
        let func = Function::new(func_symbol, implementation_block_node);

        let ast = AST::new(vec![ASTNode::new(
            TopLevelElement::Function(func),
            sample_codearea(),
        )]);

        let func_ref = ast.functions().next().unwrap();
        let root_helper = FunctionTraversalHelper::new(func_ref, &ast);
        let helper = root_helper.ref_to_implementation();

        let decl_helper = helper.index(0);

        let analyzed_stmt = analyze_statement(decl_helper, &mut mapper);

        assert!(
            analyzed_stmt.is_some(),
            "Expected variable declaration analysis to succeed, but it failed (returned None)."
        );
    }

    /** Tests the successful semantic analysis of a basic VariableAssignment (e.g., x = 10),
     * verifying that the variable is correctly looked up in the symbol table and the assigned literal expression
     * is correctly analyzed as the matching type (S32).
     */
    #[test]
    fn analyze_variable_assignment_basic_ok() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);
        let var_type = DataType::S32;

        let x_symbol = Rc::new(VariableSymbol::new("x".to_string(), var_type));
        mapper
            .add_variable(x_symbol.clone())
            .expect("Could not add variable.");

        let untyped_value_expr = Expression::Literal("10".to_string());
        let untyped_value_node = ASTNode::new(untyped_value_expr, sample_codearea());

        let untyped_assignment =
            VariableAssignment::<UntypedAST>::new("x".to_string(), untyped_value_node);
        let stmt_to_analyze = Statement::VariableAssignment(untyped_assignment);
        let stmt_to_analyze_node = ASTNode::new(stmt_to_analyze, sample_codearea());

        let func_symbol = Rc::new(FunctionSymbol::new("main".to_string(), None, Vec::new()));
        let implementation_block_inner = CodeBlock::new(vec![stmt_to_analyze_node]);
        let implementation_block_stmt = Statement::Codeblock(implementation_block_inner);
        let implementation_block_node = ASTNode::new(implementation_block_stmt, sample_codearea());
        let func = Function::new(func_symbol, implementation_block_node);
        let ast = AST::new(vec![ASTNode::new(
            TopLevelElement::Function(func),
            sample_codearea(),
        )]);
        let func_ref = ast.functions().next().unwrap();
        let root_helper = FunctionTraversalHelper::new(func_ref, &ast);
        let helper = root_helper.ref_to_implementation();

        let assign_helper = helper.index(0);

        let analyzed_stmt = analyze_statement(assign_helper, &mut mapper);

        assert!(
            analyzed_stmt.is_some(),
            "Variable assignment analysis should succeed."
        );

        if let Some(Statement::VariableAssignment(typed_assignment)) = analyzed_stmt {
            assert_eq!(
                *typed_assignment.variable(),
                x_symbol,
                "The variable symbol should be resolved correctly."
            );

            let assigned_value_node = typed_assignment.value();
            assert_eq!(assigned_value_node.data_type(), var_type);

            assert!(
                matches!(**assigned_value_node, Expression::Literal(Literal::S32(10))),
                "The assigned value should be a typed S32 literal (10)."
            );
        } else {
            panic!("The wrong statement type was returned.");
        }
    }

    /** Tests the successful semantic analysis of a VoidFunctionCall.
     * Ensures that a function declared with return_type=None is correctly analyzed as a statement.
     */
    #[test]
    fn analyze_void_function_call_ok() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);
        let func_name = "log_message".to_string();

        let param = Rc::new(VariableSymbol::new("msg".to_string(), DataType::S32));
        let func_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            func_name.clone(),
            None,
            vec![param],
        ));

        mapper
            .get_file_mapper()
            .add_function_to_file(func_symbol.clone())
            .expect("Failed to add mock function.");

        let arg = ASTNode::new(Expression::Literal("5".to_string()), sample_codearea());
        let untyped_call = FunctionCall::<UntypedAST>::new(func_name, vec![arg]);

        let analyzed_stmt = analyze_void_function_call(&untyped_call, &mut mapper);

        assert!(
            analyzed_stmt.is_some(),
            "Expected void function call analysis to succeed."
        );

        let Statement::VoidFunctionCall(typed_call) = analyzed_stmt.unwrap() else {
            panic!("Result was not a Statement::VoidFunctionCall");
        };

        assert!(typed_call.function().return_type().is_none());
    }

    /** Tests the failure case where a function returning a value (S32) is used as a VoidStatement.
     * This ensures the semantic check inside analyze_void_function_call is working.
     */
    #[test]
    fn analyze_void_function_call_must_be_void_fail() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);
        let func_name = "add".to_string();

        let func_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            func_name.clone(),
            Some(DataType::S32),
            Vec::new(),
        ));

        mapper
            .get_file_mapper()
            .add_function_to_file(func_symbol)
            .expect("Failed to add mock function.");

        let untyped_call = FunctionCall::<UntypedAST>::new(func_name, Vec::new());

        let analyzed_stmt = analyze_void_function_call(&untyped_call, &mut mapper);
        assert!(
            analyzed_stmt.is_none(),
            "Expected analysis to fail because 'add' returns S32, but was used as a Void-Statement."
        );
    }

    /** Tests the successful semantic analysis of a Break statement situated inside a loop.
     * Ensures that the break is recognized as valid because it has a surrounding loop context.
     */
    #[test]
    fn analyze_break_ok_inside_loop() {
        let mut file_mapper = FileSymbolMapper::new();
        let mut mapper = FunctionSymbolMapper::new(&mut file_mapper);

        let break_stmt = Statement::Break;
        let break_node = ASTNode::new(break_stmt, sample_codearea());

        let loop_body_inner = CodeBlock::new(vec![break_node]);
        let loop_body_stmt = Statement::Codeblock(loop_body_inner);
        let loop_body_node = ASTNode::new(loop_body_stmt, sample_codearea());

        let condition_node =
            ASTNode::new(Expression::Literal("true".to_string()), sample_codearea());

        let loop_struct = Loop::new(loop_body_node, LoopType::While(condition_node));
        let loop_stmt = Statement::ControlStructure(Box::new(ControlStructure::Loop(loop_struct)));
        let loop_node = ASTNode::new(loop_stmt, sample_codearea());

        let func_body = CodeBlock::new(vec![loop_node]);
        let func_stmt = Statement::Codeblock(func_body);
        let func_node = ASTNode::new(func_stmt, sample_codearea());

        let func_symbol = Rc::new(FunctionSymbol::new(
            "test_break_loop".to_string(),
            None,
            Vec::new(),
        ));
        let func = Function::new(func_symbol, func_node);

        let ast = AST::new(vec![ASTNode::new(
            TopLevelElement::Function(func),
            sample_codearea(),
        )]);

        let func_ref = ast.functions().next().unwrap();
        let root_helper = FunctionTraversalHelper::new(func_ref, &ast);
        let helper = root_helper.ref_to_implementation();

        let loop_helper = helper.index(0);

        let analyzed = analyze_statement(loop_helper, &mut mapper);

        assert!(
            analyzed.is_some(),
            "Expected loop containing break to analyze successfully"
        );
    }
}
