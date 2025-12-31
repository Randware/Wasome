use crate::function_symbol_mapper::FunctionSymbolMapper;
use crate::global_system_collector::GlobalSymbolMap;
use crate::mics_sa::analyze_data_type;
use ast::expression::{
    BinaryOp, Expression, FunctionCall, Literal, Typecast, UnaryOp, UnaryOpType,
};
use ast::symbol::{FunctionSymbol, Symbol, VariableSymbol};
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::rc::Rc;

/// Analyzes an untyped expression and converts it into a typed `Expression`.
///
/// # Parameters
/// * `to_analyze` - The expression to be analyzed (`Expression<UntypedAST>`).
/// * `symbol_mapper` - Mutable reference to `FunctionSymbolMapper` used for resolving symbols and tracking type information during analysis.
///
/// # Returns
/// * `Some(Expression<TypedAST>)` if the expression and all nested sub-expressions can be successfully analyzed and typed.
/// * `None` if analysis or conversion fails for the expression or any of its sub-expressions.
pub(crate) fn analyze_expression(
    to_analyze: &Expression<UntypedAST>,
    function_symbol_mapper: &mut FunctionSymbolMapper,
    helper: &StatementTraversalHelper<UntypedAST>,
    global_map: &GlobalSymbolMap,
) -> Option<Expression<TypedAST>> {
    Some(match to_analyze {
        Expression::FunctionCall(inner) => {
            let typed_call =
                analyze_function_call(inner, function_symbol_mapper, helper, global_map)?;
            if typed_call.function().return_type().is_none() {
                return None;
            }
            Expression::FunctionCall(typed_call)
        }
        Expression::Variable(inner) => analyze_variable_use(inner, function_symbol_mapper)?,
        Expression::Literal(inner) => Expression::Literal(analyze_literal(&inner)?),
        Expression::UnaryOp(inner) => Expression::UnaryOp(analyze_unary_op(
            inner,
            function_symbol_mapper,
            helper,
            global_map,
        )?),
        Expression::BinaryOp(inner) => Expression::BinaryOp(analyze_binary_op(
            inner,
            function_symbol_mapper,
            helper,
            global_map,
        )?),
    })
}

/// Analyzes an untyped `FunctionCall`, resolves the function symbol, recursively analyzes all arguments,
/// and delegates the final argument count and type checking to the `FunctionCall::new` constructor.
///
/// # Parameters
/// * `to_analyze` - The untyped `FunctionCall` structure.
/// * `function_symbol_mapper` - The mapper for resolving the function symbol and analyzing nested expressions.
///
/// # Returns
/// * `Some(FunctionCall<TypedAST>)` on success.
/// * `None` on semantic error (undeclared function, argument mismatch, or argument analysis failure).
pub(crate) fn analyze_function_call(
    to_analyze: &FunctionCall<UntypedAST>,
    _mapper: &mut FunctionSymbolMapper,
    helper: &StatementTraversalHelper<UntypedAST>,
    global_map: &GlobalSymbolMap,
) -> Option<FunctionCall<TypedAST>> {
    let call_name = to_analyze.function();

    let mut found_symbol = None;

    for (prefix, symbol) in helper.symbols_available_at() {
        let full_name = match prefix {
            Some(p) => format!("{}.{}", p.name(), symbol.name()),
            None => symbol.name().to_string(),
        };

        if full_name == *call_name {
            found_symbol = Some(symbol);
            break;
        }
    }

    let found_symbol = found_symbol?;

    let untyped_func_symbol = match found_symbol {
        Symbol::Function(f) => f,
        _ => return None,
    };

    let typed_func_symbol = global_map
        .get(untyped_func_symbol)
        .expect("Critical: Symbol found in AST but missing in GlobalMap. Stage 2 failed?");

    let mut typed_args: Vec<ASTNode<Expression<TypedAST>>> = Vec::new();
    for untyped_arg_node in to_analyze.args().iter() {
        let position = untyped_arg_node.position().clone();

        // Rekursiv analyze_expression aufrufen (mit allen Parametern!)
        let typed_expr = analyze_expression(untyped_arg_node, _mapper, helper, global_map)?;

        typed_args.push(ASTNode::new(typed_expr, position));
    }

    FunctionCall::<TypedAST>::new(typed_func_symbol.clone(), typed_args)
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
        if let Some(c) = inner.parse::<char>().ok() {
            return Some(Literal::Char(c as u32));
        }
    }

    if to_analyze.contains('.') {
        if let Ok(f64_val) = to_analyze.parse::<f64>() {
            return Some(Literal::F64(f64_val));
        }
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
    to_analyze: &Box<UnaryOp<UntypedAST>>,
    mapper: &mut FunctionSymbolMapper,
    helper: &StatementTraversalHelper<UntypedAST>,
    global_map: &GlobalSymbolMap,
) -> Option<Box<UnaryOp<TypedAST>>> {
    let (op_type, expression) = (to_analyze.op_type(), to_analyze.input());

    let converted_input = analyze_expression(&expression, mapper, helper, global_map)?;

    let converted_unary_op_type = match op_type {
        UnaryOpType::Typecast(inner) => {
            let data_type = inner.target();
            let analyzed_data_type = analyze_data_type(data_type)?;
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
    to_analyze: &Box<BinaryOp<UntypedAST>>,
    symbol_mapper: &mut FunctionSymbolMapper,
    helper: &StatementTraversalHelper<UntypedAST>,
    global_map: &GlobalSymbolMap,
) -> Option<Box<BinaryOp<TypedAST>>> {
    let (op_type, left_expr, right_expr) =
        (to_analyze.op_type(), to_analyze.left(), to_analyze.right());

    let converted_left = analyze_expression(left_expr, symbol_mapper, helper, global_map)?;
    let converted_right = analyze_expression(right_expr, symbol_mapper, helper, global_map)?;

    let left_position = left_expr.position().clone();
    let right_position = right_expr.position().clone();

    let typed_left_node = ASTNode::new(converted_left, left_position);
    let typed_right_node = ASTNode::new(converted_right, right_position);

    let analyzed = BinaryOp::<TypedAST>::new(op_type, typed_left_node, typed_right_node)?;

    Some(Box::new(analyzed))
}

#[cfg(test)]
pub(crate) fn sample_codearea() -> shared::code_reference::CodeArea {
    use shared::code_file::CodeFile;
    use shared::code_reference::{CodeArea, CodeLocation};
    use std::path::PathBuf;

    CodeArea::new(
        CodeLocation::new(0, 0),
        CodeLocation::new(0, 10),
        CodeFile::new(PathBuf::from("test/test")),
    )
    .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_symbol_mapper::{FileContext, FileSymbolMapper, GlobalFunctionMap};
    use crate::function_symbol_mapper::FunctionSymbolMapper;
    use crate::global_system_collector::GlobalSymbolMap;
    use crate::test_shared::functions_into_ast;
    use ast::data_type::{DataType, Typed};
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Literal, UnaryOp, UnaryOpType};
    use ast::statement::{CodeBlock, Statement};
    use ast::symbol::{FunctionSymbol, Symbol, VariableSymbol};
    use ast::top_level::Function;
    use ast::traversal::directory_traversal::DirectoryTraversalHelper;
    use ast::visibility::Visibility;
    use ast::{ASTNode, UntypedAST};
    use std::collections::HashMap;
    use std::rc::Rc;

    struct MockFileContext {
        path: String,
    }
    impl FileContext for MockFileContext {
        fn get_canonical_path(&self) -> &str {
            &self.path
        }
        fn resolve_import(&self, _: &str) -> Option<String> {
            None
        }
    }

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

    /// Tests that untyped literal expressions are correctly analyzed and converted into typed expressions.
    /// It verifies that "42" becomes an S32 literal and "12.2" becomes an F64 literal.
    #[test]
    fn analyze_expression_literal_converts_to_typed_literal() {
        let input = Expression::Literal(String::from("42"));
        let input2 = Expression::Literal(String::from("12.2"));

        let stmt1 = Statement::Expression(ASTNode::new(input, sample_codearea()));
        let stmt2 = Statement::Expression(ASTNode::new(input2, sample_codearea()));
        let body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![
                ASTNode::new(stmt1, sample_codearea()),
                ASTNode::new(stmt2, sample_codearea()),
            ])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("test".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body, Visibility::Private);
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
        let body_ref = func_ref.ref_to_implementation();

        let stmt1_ref = body_ref.get_child(0).unwrap();
        let expr1_ref = match &**stmt1_ref.inner() {
            Statement::Expression(e) => e,
            _ => panic!("Expected expression statement"),
        };

        let stmt2_ref = body_ref.get_child(1).unwrap();
        let expr2_ref = match &**stmt2_ref.inner() {
            Statement::Expression(e) => e,
            _ => panic!("Expected expression statement"),
        };

        let output = analyze_expression(expr1_ref, &mut mapper, &stmt1_ref, &global_map)
            .expect("should convert literal");
        assert_eq!(Expression::Literal(Literal::S32(42)), output);

        let output2 = analyze_expression(expr2_ref, &mut mapper, &stmt2_ref, &global_map)
            .expect("should convert literal (2)");
        assert_eq!(Expression::Literal(Literal::F64(12.2)), output2);
    }

    /// Tests the semantic analysis of a unary operation (e.g., negative number).
    /// It ensures that the inner expression is typed (S32) and the operation type is preserved.
    #[test]
    fn analyze_unary_negative_converts_op() {
        let inner_expr = Expression::Literal(String::from("42"));
        let inner_node = ASTNode::new(inner_expr, sample_codearea());
        let untyped = UnaryOp::<UntypedAST>::new(UnaryOpType::Negative, inner_node);
        let expr_wrapper = Expression::UnaryOp(Box::new(untyped));

        let stmt = Statement::Expression(ASTNode::new(expr_wrapper, sample_codearea()));
        let body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![ASTNode::new(stmt, sample_codearea())])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("test".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body, Visibility::Private);
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
        let body_ref = func_ref.ref_to_implementation();
        let stmt_ref = body_ref.get_child(0).unwrap();

        let expr_ref = match &**stmt_ref.inner() {
            Statement::Expression(e) => e,
            _ => panic!("Expected expression statement"),
        };
        let unary_box = match &**expr_ref {
            Expression::UnaryOp(u) => u,
            _ => panic!("Expected unary op"),
        };

        let result = analyze_unary_op(unary_box, &mut mapper, &stmt_ref, &global_map)
            .expect("should analyze unary op");

        let (op_type, expr) = (result.op_type(), result.input());

        assert_eq!(&UnaryOpType::Negative, op_type);
        assert_eq!(&Expression::Literal(Literal::S32(42)), &**expr);
    }

    /// Tests the semantic analysis of a binary operation (e.g., addition).
    /// It verifies that both left and right operands are analyzed and the operation type remains addition.
    #[test]
    fn analyze_binary_add_converts_op() {
        let left_node = ASTNode::new(Expression::Literal(String::from("17")), sample_codearea());
        let right_node = ASTNode::new(Expression::Literal(String::from("5")), sample_codearea());

        let untyped = BinaryOp::<UntypedAST>::new(BinaryOpType::Addition, left_node, right_node);
        let expr_wrapper = Expression::BinaryOp(Box::new(untyped));

        let stmt = Statement::Expression(ASTNode::new(expr_wrapper, sample_codearea()));
        let body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![ASTNode::new(stmt, sample_codearea())])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("test".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body, Visibility::Private);
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
        let body_ref = func_ref.ref_to_implementation();
        let stmt_ref = body_ref.get_child(0).unwrap();

        let expr_ref = match &**stmt_ref.inner() {
            Statement::Expression(e) => e,
            _ => panic!("Expected expression statement"),
        };
        let binary_box = match &**expr_ref {
            Expression::BinaryOp(b) => b,
            _ => panic!("Expected binary op"),
        };

        let result = analyze_binary_op(binary_box, &mut mapper, &stmt_ref, &global_map)
            .expect("should analyze binary op");

        let (op_type, l_expr, r_expr) = (result.op_type(), result.left(), result.right());
        assert_eq!(BinaryOpType::Addition, op_type);
        assert_eq!(&Expression::Literal(Literal::S32(17)), &**l_expr);
        assert_eq!(&Expression::Literal(Literal::S32(5)), &**r_expr);
    }

    /// Tests the analysis of a variable usage expression.
    /// It ensures that a variable declared in the symbol mapper is correctly resolved and typed.
    #[test]
    fn analyze_expression_variable_use_ok() {
        let untyped_use = Expression::Variable("x".to_string());

        let stmt = Statement::Expression(ASTNode::new(untyped_use, sample_codearea()));
        let body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![ASTNode::new(stmt, sample_codearea())])),
            sample_codearea(),
        );

        let func_symbol_raw = FunctionSymbol::new("test".into(), None, vec![]);
        let func = Function::new(Rc::new(func_symbol_raw), body, Visibility::Private);
        let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

        let global_map = GlobalSymbolMap::new();
        let context = MockFileContext {
            path: "test".to_string(),
        };
        let old_global_map = GlobalFunctionMap::new();
        let file_mapper = FileSymbolMapper::new(&old_global_map, &context);
        let mut mapper = FunctionSymbolMapper::new(&file_mapper);

        let expected_type = DataType::S32;
        let x_symbol = Rc::new(VariableSymbol::new("x".to_string(), expected_type));
        mapper
            .add_variable(x_symbol.clone())
            .expect("Failed to add variable.");

        let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_ref = dir_ref.file_by_name("main.waso").unwrap();
        let func_ref = file_ref.index_function(0);
        let body_ref = func_ref.ref_to_implementation();
        let stmt_ref = body_ref.get_child(0).unwrap();

        let expr_ref = match &**stmt_ref.inner() {
            Statement::Expression(e) => e,
            _ => panic!("Expected expression statement"),
        };

        let analyzed_expr = analyze_expression(expr_ref, &mut mapper, &stmt_ref, &global_map);

        assert!(
            analyzed_expr.is_some(),
            "Expected expression analysis to succeed for declared variable 'x'."
        );

        let typed_expr = analyzed_expr.unwrap();
        assert_eq!(typed_expr.data_type(), expected_type);

        if let Expression::Variable(actual_symbol) = typed_expr {
            assert_eq!(
                actual_symbol.name(),
                x_symbol.name(),
                "The resolved variable symbol name must match."
            );
        } else {
            panic!("Expected Expression::Variable variant.");
        }
    }

    /// Tests valid function call analysis.
    /// It verifies that a function call with arguments matching the parameter types is successfully resolved and typed.
    #[test]
    fn analyze_function_call_ok() {
        let func_name = "add";
        let param_type = DataType::S32;

        let param_a = Rc::new(VariableSymbol::new("a".to_string(), "s32".to_string()));
        let param_b = Rc::new(VariableSymbol::new("b".to_string(), "s32".to_string()));

        let func_symbol_untyped_raw = FunctionSymbol::new(
            func_name.to_string(),
            Some("s32".to_string()),
            vec![param_a, param_b],
        );
        let func_symbol_untyped_rc = Rc::new(func_symbol_untyped_raw.clone());

        let func_node = ASTNode::new(
            Function::new(
                func_symbol_untyped_rc,
                ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(vec![])),
                    sample_codearea(),
                ),
                Visibility::Private,
            ),
            sample_codearea(),
        );

        let typed_param_a = Rc::new(VariableSymbol::new("a".to_string(), param_type));
        let typed_param_b = Rc::new(VariableSymbol::new("b".to_string(), param_type));
        let func_symbol_typed = Rc::new(FunctionSymbol::<TypedAST>::new(
            func_name.to_string(),
            Some(DataType::S32),
            vec![typed_param_a, typed_param_b],
        ));

        let arg1 = ASTNode::new(Expression::Literal("1".to_string()), sample_codearea());
        let arg2 = ASTNode::new(Expression::Literal("2".to_string()), sample_codearea());
        let untyped_call = FunctionCall::<UntypedAST>::new(func_name.to_string(), vec![arg1, arg2]);
        let expr_wrapper = Expression::FunctionCall(untyped_call);

        let call_stmt = Statement::Expression(ASTNode::new(expr_wrapper, sample_codearea()));
        let call_body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![ASTNode::new(
                call_stmt,
                sample_codearea(),
            )])),
            sample_codearea(),
        );

        let func_main_symbol_raw = FunctionSymbol::new("main".into(), None, vec![]);
        let caller_func = Function::new(
            Rc::new(func_main_symbol_raw),
            call_body,
            Visibility::Private,
        );

        let ast = functions_into_ast(vec![
            func_node,
            ASTNode::new(caller_func, sample_codearea()),
        ]);

        let mut global_map = GlobalSymbolMap::new();
        global_map.insert(func_symbol_untyped_raw, func_symbol_typed.clone());

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
        let stmt_ref = body_ref.get_child(0).unwrap();

        let expr_ref = match &**stmt_ref.inner() {
            Statement::Expression(e) => e,
            _ => panic!("Expected expression statement"),
        };
        let call_ref = match &**expr_ref {
            Expression::FunctionCall(c) => c,
            _ => panic!("Expected function call"),
        };

        let analyzed_call = analyze_function_call(call_ref, &mut mapper, &stmt_ref, &global_map);

        assert!(
            analyzed_call.is_some(),
            "Expected function call analysis to succeed."
        );

        let typed_call = analyzed_call.unwrap();

        assert_eq!(
            typed_call.function().name(),
            func_symbol_typed.name(),
            "Resolved function symbol name must match."
        );
        assert_eq!(typed_call.args().len(), 2, "Expected 2 arguments.");
        assert_eq!(typed_call.args()[0].data_type(), DataType::S32);
        assert_eq!(typed_call.args()[1].data_type(), DataType::S32);
    }

    /// Tests detection of argument type mismatches in function calls.
    /// It verifies that passing a Bool where an S32 is expected results in a failed analysis (None).
    #[test]
    fn analyze_function_call_arg_type_mismatch() {
        let func_name = "add";

        let func_symbol_untyped_raw = FunctionSymbol::new(
            func_name.to_string(),
            Some("s32".to_string()),
            vec![
                Rc::new(VariableSymbol::new("a".into(), "s32".into())),
                Rc::new(VariableSymbol::new("b".into(), "s32".into())),
            ],
        );
        let func_symbol_untyped_rc = Rc::new(func_symbol_untyped_raw.clone());
        let func_node = ASTNode::new(
            Function::new(
                func_symbol_untyped_rc,
                ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(vec![])),
                    sample_codearea(),
                ),
                Visibility::Private,
            ),
            sample_codearea(),
        );

        let func_symbol_typed = Rc::new(FunctionSymbol::<TypedAST>::new(
            func_name.to_string(),
            Some(DataType::S32),
            vec![
                Rc::new(VariableSymbol::new("a".into(), DataType::S32)),
                Rc::new(VariableSymbol::new("b".into(), DataType::S32)),
            ],
        ));

        let arg1_fail = ASTNode::new(Expression::Literal("true".to_string()), sample_codearea());
        let arg2_ok = ASTNode::new(Expression::Literal("2".to_string()), sample_codearea());
        let untyped_call =
            FunctionCall::<UntypedAST>::new(func_name.to_string(), vec![arg1_fail, arg2_ok]);

        let expr_wrapper = Expression::FunctionCall(untyped_call);
        let call_stmt = Statement::Expression(ASTNode::new(expr_wrapper, sample_codearea()));
        let call_body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![ASTNode::new(
                call_stmt,
                sample_codearea(),
            )])),
            sample_codearea(),
        );

        let func_main_raw = FunctionSymbol::new("main".into(), None, vec![]);
        let caller_func = Function::new(Rc::new(func_main_raw), call_body, Visibility::Private);

        let ast = functions_into_ast(vec![
            func_node,
            ASTNode::new(caller_func, sample_codearea()),
        ]);

        let mut global_map = GlobalSymbolMap::new();
        global_map.insert(func_symbol_untyped_raw, func_symbol_typed);

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
        let stmt_ref = body_ref.get_child(0).unwrap();

        let expr_ref = match &**stmt_ref.inner() {
            Statement::Expression(e) => e,
            _ => panic!("Expected expression statement"),
        };
        let call_ref = match &**expr_ref {
            Expression::FunctionCall(c) => c,
            _ => panic!("Expected function call"),
        };

        let analyzed_call = analyze_function_call(call_ref, &mut mapper, &stmt_ref, &global_map);

        assert!(
            analyzed_call.is_none(),
            "Expected function call analysis to fail due to argument type mismatch (Bool vs S32)."
        );
    }
}
