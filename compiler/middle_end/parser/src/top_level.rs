use crate::misc::{datatype_parser, identifier_parser, just_token};
use crate::statement::statement_parser;
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::UntypedAST;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::{Function, TopLevelElement};
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use std::rc::Rc;

/** This parses a slice of tokens into an arbitiary top-level element
*/
pub(crate) fn top_level_parser<'src>() -> impl Parser<
    'src,
    &'src [PosInfoWrapper<Token, CodeFile>],
    PosInfoWrapper<TopLevelElement<UntypedAST>>,
> {
    // This currently only handles functions
    function_parser().map(|func| func.map(TopLevelElement::Function))
}

/** This parses a slice of tokens into a function
*/
fn function_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], PosInfoWrapper<Function<UntypedAST>>>
{
    let statement = statement_parser();
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let param = data_type
        .clone()
        .then(ident.clone())
        .map(|(data_type, name)| {
            PosInfoWrapper::new(
                Rc::new(VariableSymbol::new(name.inner, data_type.inner)),
                combine_code_areas_succeeding(&data_type.pos_info, &name.pos_info),
            )
        });

    just_token(TokenType::Function)
        .ignore_then(ident)
        .then(
            param
                .clone()
                .separated_by(just_token(TokenType::ArgumentSeparator))
                .collect::<Vec<PosInfoWrapper<Rc<VariableSymbol<UntypedAST>>>>>()
                .delimited_by(
                    just_token(TokenType::OpenParen),
                    just_token(TokenType::CloseParen),
                ),
        )
        .then(
            just_token(TokenType::Return)
                .ignore_then(data_type)
                .or_not(),
        )
        .then(statement)
        .map(|(((name, params), return_type), implementation)| {
            PosInfoWrapper::new(
                Function::new(
                    Rc::new(FunctionSymbol::new(
                        name.inner,
                        return_type.map(|to_map| to_map.inner),
                        params.into_iter().map(|param| param.inner).collect(),
                    )),
                    implementation.inner,
                ),
                combine_code_areas_succeeding(&name.pos_info, &implementation.pos_info),
            )
        })
}
#[cfg(test)]
mod tests {
    use crate::test_shared::prepare_token;
    use crate::top_level::top_level_parser;
    use ast::UntypedAST;
    use ast::block::CodeBlock;
    use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
    use ast::statement::{Statement, VariableDeclaration};
    use ast::symbol::{FunctionCall, FunctionSymbol, VariableSymbol};
    use ast::top_level::{Function, TopLevelElement};
    use chumsky::Parser;
    use lexer::TokenType;
    use std::rc::Rc;

    #[test]
    fn parse() {
        let to_parse = [
            TokenType::Function,
            TokenType::Identifier("func".to_string()),
            TokenType::OpenParen,
            TokenType::CloseParen,
            TokenType::OpenScope,
            TokenType::StatementSeparator,
            TokenType::Bool,
            TokenType::Identifier("var".to_string()),
            TokenType::Assign,
            TokenType::Identifier("test".to_string()),
            TokenType::OpenParen,
            TokenType::Integer(5),
            TokenType::As,
            TokenType::F32,
            TokenType::ArgumentSeparator,
            TokenType::Identifier("test2".to_string()),
            TokenType::NotEqual,
            TokenType::Decimal(5.0),
            TokenType::Multiplication,
            TokenType::Decimal(10.0),
            TokenType::CloseParen,
            TokenType::StatementSeparator,
            TokenType::CloseScope,
        ]
        .map(prepare_token);

        let parser = top_level_parser();

        let parsed = parser.parse(&to_parse).unwrap();
        println!("{:?}", parsed);
        let expected = TopLevelElement::Function(Function::new(
            Rc::new(FunctionSymbol::new("func".to_string(), None, Vec::new())),
            Statement::Codeblock(CodeBlock::new(vec![Statement::VariableDeclaration(
                VariableDeclaration::<UntypedAST>::new(
                    Rc::new(VariableSymbol::new("var".to_string(), "bool".to_string())),
                    Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
                        "test".to_string(),
                        vec![
                            Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                                UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                                Expression::Literal("5".to_string()),
                            ))),
                            Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                                BinaryOpType::NotEquals,
                                Expression::Variable("test2".to_string()),
                                Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                                    BinaryOpType::Multiplication,
                                    Expression::Literal("5".to_string()),
                                    Expression::Literal("10".to_string()),
                                ))),
                            ))),
                        ],
                    )),
                ),
            )])),
        ));
        assert_eq!(parsed.inner(), &expected);
    }
}
