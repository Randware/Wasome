use chumsky::Parser;
use chumsky::prelude::just;
use ast::{UntypedAST, AST};
use lexer::Token;
use crate::top_level::top_level_parser;
use chumsky::IterParser;
use crate::misc::statement_seperator;

mod expression;
mod statement;
mod misc;
mod top_level;

pub fn parser<'src>() -> impl Parser<'src, &'src [Token], AST<UntypedAST>>
{
    let top_level = top_level_parser();
    top_level
        .separated_by(statement_seperator())
        .collect::<Vec<_>>()
        .map(AST::new)
}

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use ast::block::CodeBlock;
    use ast::expression::{BinaryOp, BinaryOpType, Expression};
    use ast::statement::{ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment, VariableDeclaration};
    use ast::symbol::{FunctionSymbol, VariableSymbol};
    use ast::top_level::{Function, TopLevelElement};
    use super::*;

    #[test]
    fn parse() {
        let tokens = vec![
            Token::Function, Token::Identifier("fibonacci".to_string()),
            Token::OpenParen,
                Token::S32, Token::Identifier("nth".to_string()),
            Token::CloseParen,
            Token::Return, Token::S32,
            Token::OpenScope, Token::StatementSeparator,
                Token::S32, Token::Identifier("current".to_string()), Token::Assign, Token::Integer(1), Token::StatementSeparator,
                Token::S32, Token::Identifier("previous".to_string()), Token::Assign, Token::Integer(0), Token::StatementSeparator,
                Token::Loop,
                Token::OpenParen,
                    Token::Identifier("nth".to_string()), Token::GreaterThan, Token::Integer(1),
                Token::CloseParen,
                Token::OpenScope, Token::StatementSeparator,
                    Token::S32, Token::Identifier("temp".to_string()), Token::Assign, Token::Identifier("current".to_string()), Token::StatementSeparator,
                    Token::Identifier("current".to_string()), Token::Assign,
                        Token::Identifier("current".to_string()), Token::Addition, Token::Identifier("previous".to_string()), Token::StatementSeparator,
                    Token::Identifier("previous".to_string()), Token::Assign, Token::Identifier("temp".to_string()), Token::StatementSeparator,
                    Token::Identifier("nth".to_string()), Token::Assign,
                        Token::Identifier("nth".to_string()), Token::Subtraction, Token::Integer(1), Token::StatementSeparator,
                Token::CloseScope, Token::StatementSeparator,
                Token::Return, Token::Identifier("current".to_string()), Token::StatementSeparator,
            Token::CloseScope
        ];

        // The how manyth fibonacci number we want
        let nth = Rc::new(VariableSymbol::<UntypedAST>::new(
            "nth".to_string(),
            "s32".to_string()
        ));
        let current = Rc::new(VariableSymbol::new(
            "current".to_string(),
            "s32".to_string()
        ));
        let previous = Rc::new(VariableSymbol::new(
            "previous".to_string(),
            "s32".to_string()
        ));
        let temp = Rc::new(VariableSymbol::new(
            "temp".to_string(),
            "s32".to_string()
        ));

        let fibonacci = Rc::new(FunctionSymbol::new(
            "fibonacci".to_string(),
            Some("s32".to_string()),
            vec![nth.clone()]
        ));

        let exprected = AST::new(
            vec![
                TopLevelElement::Function(
                    Function::new(
                        fibonacci.clone(),
                        Statement::Codeblock(
                            CodeBlock::new(
                                vec![
                                    Statement::VariableDeclaration(
                                        VariableDeclaration::<UntypedAST>::new(
                                            current.clone(),
                                            Expression::Literal(
                                                "1".to_string()
                                            )
                                        )
                                    ),
                                    Statement::VariableDeclaration(
                                        VariableDeclaration::<UntypedAST>::new(
                                            previous.clone(),
                                            Expression::Literal(
                                                "0".to_string()
                                            )
                                        )
                                    ),
                                    Statement::ControlStructure(
                                        Box::new(ControlStructure::Loop(
                                            Loop::new(
                                                Statement::Codeblock(
                                                    CodeBlock::new(
                                                        vec![
                                                            Statement::VariableDeclaration(
                                                                VariableDeclaration::<UntypedAST>::new(
                                                                    temp.clone(),
                                                                    Expression::Variable(
                                                                        "current".to_string()
                                                                    )
                                                                )
                                                            ),
                                                            Statement::VariableAssignment(
                                                                VariableAssignment::<UntypedAST>::new(
                                                                    "current".to_string(),
                                                                    Expression::BinaryOp(
                                                                        Box::new(BinaryOp::<UntypedAST>::new(
                                                                            BinaryOpType::Addition,
                                                                            Expression::Variable(
                                                                                "current".to_string()
                                                                            ),
                                                                            Expression::Variable(
                                                                                "previous".to_string()
                                                                            ),
                                                                        ))
                                                                    )
                                                                )
                                                            ),
                                                            Statement::VariableAssignment(
                                                                VariableAssignment::<UntypedAST>::new(
                                                                    "previous".to_string(),
                                                                    Expression::Variable(
                                                                        "temp".to_string()
                                                                    )
                                                                )
                                                            ),
                                                            Statement::VariableAssignment(
                                                                VariableAssignment::<UntypedAST>::new(
                                                                    "nth".to_string(),
                                                                    Expression::BinaryOp(
                                                                        Box::new(BinaryOp::<UntypedAST>::new(
                                                                            BinaryOpType::Subtraction,
                                                                            Expression::Variable(
                                                                                "nth".to_string()
                                                                            ),
                                                                            Expression::Literal(
                                                                                "1".to_string()
                                                                            ),
                                                                        ))
                                                                    )
                                                                )
                                                            ),
                                                        ]
                                                    )
                                                ),
                                                LoopType::While(
                                                    Expression::BinaryOp(
                                                        Box::new(BinaryOp::<UntypedAST>::new(
                                                            BinaryOpType::Greater,
                                                            Expression::Variable(
                                                                "nth".to_string()
                                                            ),
                                                            Expression::Literal(
                                                                "1".to_string() //The fibonacci number of 1 is 1
                                                            )
                                                        ))
                                                    )
                                                )
                                            )
                                        ))
                                    ),
                                    Statement::Return(
                                        Return::new(
                                            Some(Expression::Variable(
                                                "current".to_string()
                                            ))
                                        )
                                    )
                                ]
                            )
                        )
                    )
                )
            ]
        );

        let parser = parser();
        let actual = parser.parse(&tokens).unwrap();
        assert_eq!(exprected, actual);
    }
}
