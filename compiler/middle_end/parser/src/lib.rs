use crate::misc::statement_seperator;
use crate::top_level::top_level_parser;
use ast::{AST, UntypedAST};
use chumsky::IterParser;
use chumsky::Parser;
use lexer::Token;
use shared::code_file::CodeFile;
use shared::code_reference::CodeArea;
use std::fmt::Debug;
use std::ops::Deref;
use std::path::PathBuf;

mod expression;
mod misc;
mod statement;
mod top_level;

pub fn parse(to_parse: Vec<Token>, file: String) -> Option<AST<UntypedAST>> {
    let to_parse_with_file_info = inject_file_information(to_parse, file);
    let parser = parser();
    parser.parse(&to_parse_with_file_info).into_output()
}

fn inject_file_information(
    raw_tokens: Vec<Token>,
    file: String,
) -> Vec<PosInfoWrapper<Token, CodeFile>> {
    let code_file = CodeFile::new(PathBuf::from(file));
    raw_tokens
        .into_iter()
        .map(|token| PosInfoWrapper::new(token, code_file.clone()))
        .collect()
}
/** This parses a slice of tokens into an ast
*/
fn parser<'src>() -> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], AST<UntypedAST>> {
    let top_level = top_level_parser();
    top_level
        .separated_by(statement_seperator())
        .collect::<Vec<_>>()
        .map(|top_level_elements| {
            AST::new(
                top_level_elements
                    .into_iter()
                    .map(|top_level_elem| top_level_elem.inner)
                    .collect(),
            )
        })
}

pub(crate) fn combine_code_areas(a: &CodeArea, b: &CodeArea) -> Option<CodeArea> {
    if a.file() != b.file() {
        return None;
    }
    CodeArea::new(a.start().clone(), b.start().clone(), a.file().clone())
}

pub(crate) fn combine_code_areas_succeeding(a: &CodeArea, b: &CodeArea) -> CodeArea {
    combine_code_areas(a, b).unwrap()
}

#[derive(PartialEq, Debug)]
pub(crate) struct PosInfoWrapper<T: PartialEq + Debug, Pos: PartialEq + Debug = CodeArea> {
    pub inner: T,
    pub pos_info: Pos,
}

impl<T: PartialEq + Debug, Pos: PartialEq + Debug> PosInfoWrapper<T, Pos> {
    pub fn new(inner: T, pos_info: Pos) -> Self {
        Self { inner, pos_info }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn pos_info(&self) -> &Pos {
        &self.pos_info
    }

    pub fn map<O: PartialEq + Debug>(self, mapper: impl FnOnce(T) -> O) -> PosInfoWrapper<O, Pos> {
        PosInfoWrapper::new(mapper(self.inner), self.pos_info)
    }
}

impl<T: PartialEq + Debug, Pos: PartialEq + Debug> Deref for PosInfoWrapper<T, Pos> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T: PartialEq + Debug + Clone, Pos: PartialEq + Debug + Clone> Clone
    for PosInfoWrapper<T, Pos>
{
    fn clone(&self) -> Self {
        Self::new(self.inner.clone(), self.pos_info.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_shared::prepare_token;
    use ast::block::CodeBlock;
    use ast::expression::{BinaryOp, BinaryOpType, Expression};
    use ast::statement::{
        ControlStructure, Loop, LoopType, Return, Statement, VariableAssignment,
        VariableDeclaration,
    };
    use ast::symbol::{FunctionSymbol, VariableSymbol};
    use ast::top_level::{Function, TopLevelElement};
    use lexer::TokenType;
    use std::rc::Rc;

    #[test]
    fn parse() {
        let tokens = [
            TokenType::Function,
            TokenType::Identifier("fibonacci".to_string()),
            TokenType::OpenParen,
            TokenType::S32,
            TokenType::Identifier("nth".to_string()),
            TokenType::CloseParen,
            TokenType::Return,
            TokenType::S32,
            TokenType::OpenScope,
            TokenType::StatementSeparator,
            TokenType::S32,
            TokenType::Identifier("current".to_string()),
            TokenType::Assign,
            TokenType::Integer(1),
            TokenType::StatementSeparator,
            TokenType::S32,
            TokenType::Identifier("previous".to_string()),
            TokenType::Assign,
            TokenType::Integer(0),
            TokenType::StatementSeparator,
            TokenType::Loop,
            TokenType::OpenParen,
            TokenType::Identifier("nth".to_string()),
            TokenType::GreaterThan,
            TokenType::Integer(1),
            TokenType::CloseParen,
            TokenType::OpenScope,
            TokenType::StatementSeparator,
            TokenType::S32,
            TokenType::Identifier("temp".to_string()),
            TokenType::Assign,
            TokenType::Identifier("current".to_string()),
            TokenType::StatementSeparator,
            TokenType::Identifier("current".to_string()),
            TokenType::Assign,
            TokenType::Identifier("current".to_string()),
            TokenType::Addition,
            TokenType::Identifier("previous".to_string()),
            TokenType::StatementSeparator,
            TokenType::Identifier("previous".to_string()),
            TokenType::Assign,
            TokenType::Identifier("temp".to_string()),
            TokenType::StatementSeparator,
            TokenType::Identifier("nth".to_string()),
            TokenType::Assign,
            TokenType::Identifier("nth".to_string()),
            TokenType::Subtraction,
            TokenType::Integer(1),
            TokenType::StatementSeparator,
            TokenType::CloseScope,
            TokenType::StatementSeparator,
            TokenType::Return,
            TokenType::Identifier("current".to_string()),
            TokenType::StatementSeparator,
            TokenType::CloseScope,
        ]
        .map(prepare_token);

        // The how manyth fibonacci number we want
        let nth = Rc::new(VariableSymbol::<UntypedAST>::new(
            "nth".to_string(),
            "s32".to_string(),
        ));
        let current = Rc::new(VariableSymbol::new(
            "current".to_string(),
            "s32".to_string(),
        ));
        let previous = Rc::new(VariableSymbol::new(
            "previous".to_string(),
            "s32".to_string(),
        ));
        let temp = Rc::new(VariableSymbol::new("temp".to_string(), "s32".to_string()));

        let fibonacci = Rc::new(FunctionSymbol::new(
            "fibonacci".to_string(),
            Some("s32".to_string()),
            vec![nth.clone()],
        ));

        let exprected = AST::new(vec![TopLevelElement::Function(Function::new(
            fibonacci.clone(),
            Statement::Codeblock(CodeBlock::new(vec![
                Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                    current.clone(),
                    Expression::Literal("1".to_string()),
                )),
                Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                    previous.clone(),
                    Expression::Literal("0".to_string()),
                )),
                Statement::ControlStructure(Box::new(ControlStructure::Loop(Loop::new(
                    Statement::Codeblock(CodeBlock::new(vec![
                        Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
                            temp.clone(),
                            Expression::Variable("current".to_string()),
                        )),
                        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
                            "current".to_string(),
                            Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                                BinaryOpType::Addition,
                                Expression::Variable("current".to_string()),
                                Expression::Variable("previous".to_string()),
                            ))),
                        )),
                        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
                            "previous".to_string(),
                            Expression::Variable("temp".to_string()),
                        )),
                        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
                            "nth".to_string(),
                            Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                                BinaryOpType::Subtraction,
                                Expression::Variable("nth".to_string()),
                                Expression::Literal("1".to_string()),
                            ))),
                        )),
                    ])),
                    LoopType::While(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Greater,
                        Expression::Variable("nth".to_string()),
                        Expression::Literal(
                            "1".to_string(), //The fibonacci number of 1 is 1
                        ),
                    )))),
                )))),
                Statement::Return(Return::new(Some(Expression::Variable(
                    "current".to_string(),
                )))),
            ])),
        ))]);

        let parser = parser();
        let actual = parser.parse(&tokens).unwrap();
        assert_eq!(exprected, actual);
    }
}

#[cfg(test)]
pub(crate) mod test_shared {
    use crate::PosInfoWrapper;
    use lexer::{Token, TokenType};
    use shared::code_file::CodeFile;
    use std::path::PathBuf;

    pub(crate) fn prepare_token(to_convert: TokenType) -> PosInfoWrapper<Token, CodeFile> {
        PosInfoWrapper::new(
            Token {
                kind: to_convert,
                line: 0,
                span: Default::default(),
            },
            CodeFile::new(PathBuf::from("test/test")),
        )
    }
}
