use crate::error_sa::SemanticError;
use crate::statement_sa::analyze_statement;
use crate::symbol::SyntaxContext;
use crate::symbol::function_symbol_mapper::FunctionSymbolMapper;
use ast::composite::{Enum, EnumVariant};
use ast::statement::{ControlStructure, Statement};
use ast::symbol::{DirectlyAvailableSymbol, EnumSymbol, EnumVariantSymbol, FunctionSymbol, SymbolWithTypeParameter};
use ast::top_level::Function;
use ast::top_level::FunctionType;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::visibility::Visible;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::ops::Deref;
use std::rc::Rc;

/// Analyzes the implementation (body) of an untyped function.
///
/// Initializes the function's local scope, registers parameters, and validates all contained statements and expressions.
/// Requires the function's signature to be present in the global symbol map prior to execution.
///
/// # Panics
/// * Panics if function parameters conflict (assumes upstream uniqueness guarantees).
///
/// # Control Flow Analysis
/// Checks if the function always returns a value (if a return type is declared) using `always_return`.
///
/// # Parameters
/// * `symbol` - The typed function symbol containing the return type and parameters.
/// * `context` - The syntax context containing the function traversal helper for the body.
///
/// # Returns
/// * `Ok(Function<TypedAST>)` if the body is semantically correct.
/// * `Err(SemanticError)` if analysis fails (e.g., type or scope errors, missing return statement).
pub(crate) fn analyze_function(
    symbol: Rc<FunctionSymbol<TypedAST>>,
    context: &SyntaxContext<&FunctionTraversalHelper<UntypedAST>>,
) -> Result<ASTNode<Function<TypedAST>>, SemanticError> {
    let mut func_mapper = FunctionSymbolMapper::new();
    func_mapper.set_current_function_return_type(symbol.return_type().cloned());

    use std::collections::HashSet;
    let mut duplicate_param_error = None;
    context.ast_reference.symbols().find(|sym| {
        match &sym.1 {
            DirectlyAvailableSymbol::Struct(st) => {
                let mut seen = HashSet::new();
                for tp in st.type_parameters() {
                    let name = tp.inner().name().to_string();
                    if !seen.insert(name.clone()) {
                        duplicate_param_error = Some(SemanticError::DuplicateTypeParameter {
                            name,
                            span: *context.ast_reference.inner().position(),
                        });
                        return true;
                    }
                }
            }
            DirectlyAvailableSymbol::Enum(en) => {
                let mut seen = HashSet::new();
                for tp in en.type_parameters() {
                    let name = tp.inner().name().to_string();
                    if !seen.insert(name.clone()) {
                        duplicate_param_error = Some(SemanticError::DuplicateTypeParameter {
                            name,
                            span: *context.ast_reference.inner().position(),
                        });
                        return true;
                    }
                }
            }
            _ => {}
        }
        false
    });

    if let Some(err) = duplicate_param_error {
        return Err(err);
    }

    // Validate drop/predrop method signature constraints (E3029)
    if symbol.name() == "drop" || symbol.name() == "predrop" {
        if symbol.return_type().is_some()
            || !symbol.type_parameters().is_empty()
            || symbol.params().iter().any(|p| p.name() != "self")
        {
            return Err(SemanticError::InvalidDropSignature {
                message: "Drop methods cannot accept parameters, generics, or explicit return signatures".to_string(),
                span: *context.ast_reference.inner().position(),
            });
        }
    }

    for param_symbol in symbol.params().iter() {
        func_mapper
            .add_variable(
                param_symbol.clone(),
                *context.ast_reference.inner().position(),
            )
            .map_err(|_| SemanticError::AlreadyDeclared {
                name: param_symbol.name().to_string(),
                kind: "Parameter".to_string(),
                span: *context.ast_reference.inner().position(),
            })?;
    }

    let ft = match context.ast_reference.inner().function_type() {
     FunctionType::Regular(implementation) => {
            // Use the traversal helper API on the function helper instead of calling
            // StatementTraversalHelper::new_root directly. This is more ergonomic and
            // avoids lifetime/trait-object issues.
            let sth = context
                .ast_reference
                .ref_to_implementation()
                .map_err(|_| SemanticError::Internal {
                    message: "Failed to initialize statement traversal helper".to_string(),
                    span: *implementation.position(),
                })?;

            let new_context = context.with_ast_reference(&sth);
            let typed_implementation_statement = analyze_statement(&new_context, &mut func_mapper)?;

            if symbol.return_type().is_some() && !always_return(&typed_implementation_statement) {
                return Err(SemanticError::MissingReturn {
                    func_name: symbol.name().to_string(),
                    span: *implementation.position(),
                });
            }

            let code_area = *implementation.position();
            FunctionType::Regular(ASTNode::new(typed_implementation_statement, code_area))
        }
        ast::top_level::FunctionType::External => ast::top_level::FunctionType::External
    };

    let to_analyze = &context.ast_reference;
    Ok(ASTNode::new(
        Function::new(symbol, ft, to_analyze.inner().visibility()),
        *to_analyze.inner().position(),
    ))
}

pub(crate) fn analyze_enum(
    symbol: Rc<EnumSymbol<TypedAST>>,
    variants: Vec<Rc<EnumVariantSymbol<TypedAST>>>,
    context: &SyntaxContext<&EnumTraversalHelper<UntypedAST>>,
) -> ASTNode<Enum<TypedAST>> {
    let untyped_enum = context.ast_reference.inner();
    ASTNode::new(
        Enum::new(
            symbol,
            variants
                .into_iter()
                .zip(untyped_enum.variants().iter())
                .map(|(typed, untyped)| ASTNode::new(EnumVariant::new(typed), *untyped.position()))
                .collect(),
            untyped_enum.visibility(),
        ),
        *untyped_enum.position(),
    )
}

/// Checks wherever a statement will always encounter a return statement before finishing execution
fn always_return(to_check: &Statement<TypedAST>) -> bool {
    match to_check {
        Statement::Return(_) => true,
        Statement::Codeblock(codeblock) => codeblock
            .last()
            .map(|statement| always_return(statement.deref()))
            .unwrap_or(false),
        Statement::ControlStructure(crtl) => match crtl.deref() {
            ControlStructure::Conditional(cond) => {
                always_return(cond.then_statement())
                    && cond
                    .else_statement()
                    .map(|else_statement| always_return(else_statement))
                    .unwrap_or(false)
            }
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    /*/// Tests the successful analysis of a simple void function with no parameters.
/// It verifies that the function is correctly resolved from the global map and its body is processed.
#[test]
fn analyze_function_ok() {
    let func_name = "test".to_string();
    let func_symbol_untyped_raw = FunctionSymbol::new(func_name.clone(), None, Vec::new());
    let func_symbol_untyped_rc = Rc::new(func_symbol_untyped_raw.clone());

    let body_block = CodeBlock::new(Vec::new());
    let body_node = ASTNode::new(Statement::Codeblock(body_block), sample_codearea());

    let func = Function::new(func_symbol_untyped_rc, body_node, Visibility::Private);
    let ast = functions_into_ast(vec![ASTNode::new(func, sample_codearea())]);

    let mut global_map = GlobalSymbolMap::new();
    let func_symbol_typed =
        Rc::new(FunctionSymbol::<TypedAST>::new(func_name, None, Vec::new()));
    global_map.insert(func_symbol_untyped_raw, func_symbol_typed.clone());

    let context = MockFileContext {
        path: "test".to_string(),
    };
    let old_global_map = GlobalFunctionMap::new();
    let mut file_mapper = FileSymbolMapper::new(&old_global_map, &context);

    let dir_ref = DirectoryTraversalHelper::new_from_ast(&ast);
    let file_ref = dir_ref.file_by_name("main.waso").unwrap();
    let func_ref = file_ref.index_function(0);

    let analyzed_func =
        analyze_function(func_ref.inner(), &func_ref, &mut file_mapper, &global_map);

    assert!(analyzed_func.is_some(), "Function analysis should succeed");

    let typed_func = analyzed_func.unwrap();
    assert_eq!(typed_func.declaration().name(), "test");
    assert!(typed_func.declaration().return_type().is_none());
}*/
}