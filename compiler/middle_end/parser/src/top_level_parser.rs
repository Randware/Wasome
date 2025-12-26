use crate::misc_parsers::{datatype_parser, identifier_parser, token_parser, statement_separator};
use crate::statement_parser::statement_parser;
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::symbol::{FunctionSymbol, ModuleUsageNameSymbol, VariableSymbol};
use ast::top_level::{Function, Import, ImportRoot};
use ast::visibility::Visibility;
use ast::{ASTNode, UntypedAST, AST};
use chumsky::prelude::*;
use lexer::{Token, TokenType};
use shared::code_file::CodeFile;
use shared::code_reference::CodeArea;
use std::rc::Rc;

/// Parses all Top-Level elements in a file
pub(crate) fn top_level_parser<'src>() -> impl Parser<
    'src,
    &'src [PosInfoWrapper<Token, CodeFile>],
    (Vec<ASTNode<Import>>, Vec<ASTNode<Function<UntypedAST>>>),
> {
    let imports = import_parser()
        .separated_by(statement_separator())
        .collect::<Vec<_>>();

    let functions = function_parser()
        .separated_by(statement_separator())
        .collect::<Vec<_>>();

    imports
        .then(functions)
        .map(|(imports, functions)| (imports, functions))
}

/// Parses a single import
fn import_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Import>> {
    let ident = identifier_parser();
    let path = ident
        .clone()
        .separated_by(token_parser(TokenType::PathSeparator))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|elements| {
            // We specified that the path must be at least one long
            // Therefore the vec may not be empty, last can not return None and we never panic
            let pos = elements.last().unwrap().pos_info.clone();
            (
                elements
                    .into_iter()
                    .map(|elem| elem.inner)
                    .collect::<Vec<_>>(),
                pos,
            )
        });
    // TODO: Change token to import
    // TODO: Change ArgSep to quote
    token_parser(TokenType::Function)
        .then(
            token_parser(TokenType::ArgumentSeparator)
                .ignore_then(path)
                .then_ignore(token_parser(TokenType::Division))
                .then(token_parser(TokenType::As).ignore_then(ident).or_not()),
        )
        .map(|(import, ((mut path, path_end_pos), usage_name))| {
            let start = import.pos_info.start().clone();
            let end = usage_name
                .as_ref()
                .map(|inner| inner.pos_info.clone())
                .unwrap_or(path_end_pos)
                .end()
                .clone();

            let root = if path.first().unwrap() == "." {
                path.remove(0);
                ImportRoot::CurrentModule
            } else {
                ImportRoot::Root
            };

            // This never panics due to the same reason as above
            let use_as = Rc::new(ModuleUsageNameSymbol::new(
                usage_name
                    .map(|un| un.inner)
                    .unwrap_or_else(|| path.last().unwrap().clone()),
            ));
            // The pos info of a later token can never be before that of an earlier token
            // Therefore, this can never panic
            ASTNode::new(
                Import::new(root, path, use_as),
                CodeArea::new(start, end, import.pos_info.file().clone()).unwrap(),
            )
        })
}

/// Parses a single function
fn function_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Function<UntypedAST>>>
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

    token_parser(TokenType::Public)
        .or_not()
        .then(
            token_parser(TokenType::Function).ignore_then(ident).then(
                param
                    .clone()
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .collect::<Vec<PosInfoWrapper<Rc<VariableSymbol<UntypedAST>>>>>()
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    ),
            ),
        )
        .then(
            token_parser(TokenType::Return)
                .ignore_then(data_type)
                .or_not(),
        )
        .then(statement)
        .map(
            |(((visibility, (name, params)), return_type), implementation)| {
                let pos = combine_code_areas_succeeding(&name.pos_info, implementation.position());
                ASTNode::new(
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
    use crate::top_level_parser::top_level_parser;
    use chumsky::Parser;
    use lexer::TokenType;

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
