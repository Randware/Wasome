use crate::misc::{datatype_parser, identifier_parser, just_token, statement_separator};
use crate::statement::statement_parser;
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::file::File;
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::{Function, Import};
use ast::visibility::Visibility;
use ast::{ASTNode, UntypedAST};
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use std::rc::Rc;

/** This parses a slice of tokens into a file
*/
pub(crate) fn top_level_parser<'src>() -> impl Parser<
    'src,
    &'src [PosInfoWrapper<Token, CodeFile>],
    (Vec<ASTNode<Import>>, Vec<ASTNode<Function<UntypedAST>>>),
> {
    // This currently only handles functions
    function_parser()
        .map(|func| func.into_ast_node())
        .separated_by(crate::misc::statement_separator())
        .collect::<Vec<_>>()
        .map(|functions| (Vec::new(), functions))
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

    just_token(TokenType::Public)
        .or_not()
        .then(
            just_token(TokenType::Function).ignore_then(ident).then(
                param
                    .clone()
                    .separated_by(just_token(TokenType::ArgumentSeparator))
                    .collect::<Vec<PosInfoWrapper<Rc<VariableSymbol<UntypedAST>>>>>()
                    .delimited_by(
                        just_token(TokenType::OpenParen),
                        just_token(TokenType::CloseParen),
                    ),
            ),
        )
        .then(
            just_token(TokenType::Return)
                .ignore_then(data_type)
                .or_not(),
        )
        .then(statement)
        .map(
            |(((visibility, (name, params)), return_type), implementation)| {
                let pos = combine_code_areas_succeeding(&name.pos_info, implementation.position());
                PosInfoWrapper::new(
                    Function::new(
                        Rc::new(FunctionSymbol::new(
                            name.inner,
                            return_type.map(|to_map| to_map.inner),
                            params.into_iter().map(|param| param.inner).collect(),
                        )),
                        implementation,
                        visibility
                            .map(|_| Visibility::Public)
                            .unwrap_or(Visibility::Private),
                    ),
                    pos,
                )
            },
        )
}
#[cfg(test)]
mod tests {
    use crate::test_shared::wrap_token;
    use crate::top_level::top_level_parser;
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
        .map(wrap_token);

        let parser = top_level_parser();

        let parsed = parser.parse(&to_parse).unwrap();

        let expected_func_name = "func";
        let func_name = {
            let function = parsed.1.first().unwrap();
            function.declaration().name()
        };
        assert_eq!(expected_func_name, func_name);
    }
}
