use std::ops::Deref;
use crate::top_level::Function;

pub mod expression;
pub mod statement;
pub mod block;
pub mod top_level;
pub mod symbol;
pub mod data_type;

#[derive(Debug)]
pub struct AST
{
    functions: Vec<Function>
}

impl AST
{
    pub fn new(functions: Vec<Function>) -> Self
    {
        Self
        {
            functions
        }
    }

    pub fn functions(&self) -> &[Function]
    {
        &self.functions
    }
}

impl Deref for AST
{
    type Target = [Function];

    fn deref(&self) -> &Self::Target
    {
        &self.functions
    }
}


/** This compares two values
This is useful for returning with the ? operator if values are not equal
@params
left, right: The values to compare
@return
None if not equal
Some if equal
*/
fn eq_return_option<T: PartialEq>(left: T, right: T) -> Option<()>
{
    if left == right
    {
        return Some(())
    }
    None
}

#[cfg(test)]
mod tests
{
    use std::rc::Rc;
    use crate::AST;
    use crate::block::CodeBlock;
    use crate::data_type::DataType;
    use crate::expression::{Expression, Literal};
    use crate::statement::{ControlStructure, Loop, LoopType, Statement, StatementRef, VariableAssignment};
    use crate::symbol::{FunctionSymbol, Symbol, VariableSymbol};
    use crate::top_level::{Function, FunctionRef};

    #[test]
    fn statement()
    {
        let symbol = Rc::new(
            VariableSymbol::new("test".to_string(), DataType::F32)
        );
        let statement = Statement::VariableDeclaration(
            basic_test_variable(symbol.clone()).unwrap()
        );


        assert_eq!(Some(Symbol::Variable(&symbol)), statement.get_direct_symbol());

        let function = Function::new(
            Rc::new(
                FunctionSymbol::new(
                    "test".to_string(),
                    None,
                    Vec::new()
                )
            ),
            statement
        );

        let ast = AST::new(vec![function]);

        let function_ref = FunctionRef::new(&ast[0], &ast);

        let statement_ref = StatementRef::new_root(&function_ref);
        assert_eq!(vec![Symbol::Function(function_ref.declaration())], statement_ref.symbols().collect::<Vec<_>>());
    }

    #[test]
    fn statement_2()
    {
        let symbol = Rc::new(
            VariableSymbol::new("test".to_string(), DataType::F32)
        );
        let statement = Statement::Codeblock(
            CodeBlock::new(
                vec![
                    Statement::VariableDeclaration(
                        VariableAssignment::new(
                            symbol.clone(),
                            Expression::Literal(
                                Literal::F32(
                                    10.0
                                )
                            )
                        ).unwrap()
                    ),
                    Statement::ControlStructure(
                        Box::new(
                            ControlStructure::Loop(
                                Loop::new(
                                    Statement::Expression(
                                        Expression::Literal(
                                            Literal::Bool(
                                                false
                                            )
                                        )
                                    ),
                                    LoopType::Infinite
                                )
                            )
                        )
                    )
                ]
            )
        );

        let function = Function::new(
            Rc::new(
                FunctionSymbol::new(
                    "test".to_string(),
                    None,
                    Vec::new()
                )
            ),
            statement
        );

        let ast = AST::new(vec![function]);

        let function_ref = FunctionRef::new(&ast[0], &ast);

        let root = StatementRef::new_root(&function_ref);
        let loop_statement = root.index(1);
        let statement_ref = loop_statement.index(0);

        let actual = statement_ref.symbols().collect::<Vec<_>>();
        let expected = vec![Symbol::Variable(&symbol), Symbol::Function(function_ref.declaration())];
        assert_eq!(actual.len(), 2);
        assert!(expected.iter().all(|val| actual.contains(val)))
    }

    fn basic_test_variable(symbol: Rc<VariableSymbol>) -> Option<VariableAssignment> {
        VariableAssignment::new(
            symbol,
            Expression::Literal(
                Literal::F32(
                    14.0
                )
            )
        )
    }
}
