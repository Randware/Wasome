use crate::misc::{datatype_parser, identifier_parser, just_token};
use crate::statement::statement_parser;
use crate::{combine_code_areas_succeeding, PosInfoWrapper};
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::{Function, TopLevelElement};
use ast::{ASTNode, UntypedAST};
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use std::rc::Rc;

/** This parses a slice of tokens into an arbitiary top-level element
*/
pub(crate) fn top_level_parser<'src>() -> impl Parser<
    'src,
    &'src [PosInfoWrapper<Token, CodeFile>],
    ASTNode<TopLevelElement<UntypedAST>>,
> {
    // This currently only handles functions
    function_parser().map(|func| func.map(TopLevelElement::Function).into_ast_node())
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
            let pos = combine_code_areas_succeeding(&name.pos_info, implementation.position());
            PosInfoWrapper::new(
                Function::new(
                    Rc::new(FunctionSymbol::new(
                        name.inner,
                        return_type.map(|to_map| to_map.inner),
                        params.into_iter().map(|param| param.inner).collect(),
                    )),
                    implementation,
                ),
                pos,
            )
        })
}
#[cfg(test)]
mod tests {
    use crate::test_shared::prepare_token;
    use crate::top_level::top_level_parser;
    use ast::top_level::TopLevelElement;
    use chumsky::Parser;
    use lexer::TokenType;
    use std::ops::Deref;

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

        let expected_func_name = "func";
        let func_name =
            {
                let TopLevelElement::Function(function) = parsed.deref();
                function.declaration().name()
            };
        assert_eq!(expected_func_name, func_name);
    }
}
