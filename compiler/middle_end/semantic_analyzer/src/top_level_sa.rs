use crate::file_symbol_mapper::FileSymbolMapper;
use crate::function_symbol_mapper::FunctionSymbolMapper;
use crate::statement_sa::analyze_statement;
use ast::symbol::FunctionSymbol;
use ast::top_level::{Function, TopLevelElement};
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::rc::Rc;

/// Analyzes a top-level element (e.g., a Function) and converts it into its typed representation.
///
/// This process ensures semantic correctness for the element's implementation body.
///
/// # Parameters
/// * `to_analyze` - The untyped element to be analyzed (`TopLevelElement<UntypedAST>`).
/// * `file_mapper` - Provides global context for symbol lookup (e.g., function declarations) (`&mut FileSymbolMapper`).
/// * `root_helper` - The traversal context for the function's implementation (`&FunctionTraversalHelper<UntypedAST>`).
///
/// # Returns
/// * `Some(TopLevelElement<TypedAST>)` if the element (including its body) could be successfully analyzed.
/// * `None` if analysis fails (type errors, invalid constructs, or other semantic errors).
pub(crate) fn analyze_top_level(
    to_analyze: TopLevelElement<UntypedAST>,
    file_mapper: &mut FileSymbolMapper,
    root_helper: &FunctionTraversalHelper<UntypedAST>,
) -> Option<TopLevelElement<TypedAST>> {
    match to_analyze {
        TopLevelElement::Function(untyped_function) => {
            analyze_function(&untyped_function, root_helper, file_mapper)
                .map(TopLevelElement::Function)
        }
    }
}

/// Analyzes the implementation (body) of an untyped function.
///
/// Initializes the function's local scope, registers parameters, and validates all contained statements and expressions.
/// Requires the function's signature to be present in the file symbol table prior to execution.
///
/// # Parameters
/// * `untyped_function` - Function element with the body to analyze (`&Function<UntypedAST>`).
/// * `root_helper` - Traversal context for the function's body (`&FunctionTraversalHelper<UntypedAST>`).
/// * `file_mapper` - Global symbol table for declaration lookup and context (`&mut FileSymbolMapper`).
///
/// # Returns
/// * `Some(Function<TypedAST>)` if the body is semantically correct.
/// * `None` if analysis fails (e.g., type or scope errors).
fn analyze_function(
    untyped_function: &Function<UntypedAST>,
    root_helper: &FunctionTraversalHelper<UntypedAST>,
    file_mapper: &mut FileSymbolMapper,
) -> Option<Function<TypedAST>> {
    let func_name = untyped_function.declaration().name();
    let typed_declaration: Rc<FunctionSymbol<TypedAST>> =
        file_mapper.lookup_function_rc(func_name)?;

    let mut func_mapper = FunctionSymbolMapper::new(file_mapper);
    func_mapper.set_current_function_return_type(typed_declaration.return_type().cloned());

    for param_symbol in typed_declaration.params().iter() {
        func_mapper
            .add_variable(param_symbol.clone())
            .expect("Internal error: Function parameters should not conflict.");
    }

    let impl_helper = StatementTraversalHelper::new_root(root_helper);
    let typed_implementation_statement = analyze_statement(impl_helper, &mut func_mapper)?;

    let code_area = untyped_function.implementation().position().clone();

    let implementation_node = ASTNode::new(typed_implementation_statement, code_area);

    Some(Function::new(
        typed_declaration.clone(),
        implementation_node,
    ))
}

//todo Testing
