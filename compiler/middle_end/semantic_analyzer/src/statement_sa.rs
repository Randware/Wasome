use ast::statement::{Return, Statement};
use ast::{TypedAST, UntypedAST};
use ast::expression::Expression;
use ast::traversal::statement_traversal::StatementTraversalHelper;
use crate::expression_sa;
use crate::expression_sa::analyze_expression;
use crate::symbol_mapper::SymbolMapper;

pub(crate) fn analyze_statement(to_analyze: StatementTraversalHelper<UntypedAST>, symbol_mapper: &mut SymbolMapper) -> Option<Statement<TypedAST>> {
    match  to_analyze.get_inner() {
        Statement::VariableAssignment(_) => todo!(),
        Statement::VariableDeclaration(_) => todo!(),
        Statement::Expression(inner) => {
            let typed_expr = analyze_expression(inner, symbol_mapper)?;
            Some(Statement::Expression(typed_expr))
        }
        Statement::Return(inner) => {
        let typed_return = analyze_return(inner,symbol_mapper)?;
        Some(Statement::Return(typed_return))
        },
        Statement::ControlStructure(_) => todo!(), //3
        Statement::Codeblock(_) => todo!(), //4
        Statement::VoidFunctionCall(_) => todo!()
    }
}

fn analyze_return(to_analyze : &Return<UntypedAST>,symbol_mapper: &mut SymbolMapper) -> Option<Return<TypedAST>>{
    todo!()
}
