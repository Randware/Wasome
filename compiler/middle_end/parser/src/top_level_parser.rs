use crate::misc_parsers::{datatype_parser, identifier_parser, statement_separator, token_parser};
use crate::statement_parser::statement_parser;
use crate::top_level_parser::import_parser::import_parser;
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::symbol::{FunctionSymbol, ModuleUsageNameSymbol, VariableSymbol};
use ast::top_level::{Function, Import, ImportRoot};
use ast::visibility::Visibility;
use ast::{ASTNode, UntypedAST};
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

mod import_parser {
    use crate::PosInfoWrapper;
    use crate::misc_parsers::{identifier_parser, string_parser, token_parser};
    use ast::symbol::ModuleUsageNameSymbol;
    use ast::top_level::{Function, Import, ImportRoot};
    use ast::{ASTNode, UntypedAST};
    use chumsky::IterParser;
    use chumsky::Parser;
    use chumsky::container::OrderedSeq;
    use chumsky::error::EmptyErr;
    use chumsky::prelude::{choice, just, todo};
    use chumsky::primitive::Just;
    use chumsky::regex::regex;
    use lexer::{Token, TokenType};
    use shared::code_file::CodeFile;
    use shared::code_reference::CodeArea;
    use std::mem::transmute;
    use std::rc::Rc;

    /// Parses a single import
    pub(super) fn import_parser<'src>()
    -> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Import>> {
        let ident = identifier_parser();
        let path = string_parser();
        token_parser(TokenType::Import)
            .then(path.then(token_parser(TokenType::As).ignore_then(ident).or_not()))
            .try_map(|(import, (path, usage_name)), span| {
                let path_end_pos = path.pos_info;
                let path = path.inner;
                let start = import.pos_info.start().clone();
                let end = usage_name
                    .as_ref()
                    .map(|inner| inner.pos_info.clone())
                    .unwrap_or(path_end_pos)
                    .end()
                    .clone();

                let path = parse_import_path(&path).ok_or(EmptyErr::default())?;
                let use_as = usage_name.map(|inner| inner.inner).unwrap_or_else(|| {
                    path.1
                        .last()
                        .map(|input| input.as_ref())
                        .unwrap_or_else(|| "")
                        .to_owned()
                }); // TODO

                // The pos info of a later token can never be before that of an earlier token
                // Therefore, this can never panic
                Ok(ASTNode::new(
                    Import::new(path.0, path.1, Rc::new(ModuleUsageNameSymbol::new(use_as))),
                    CodeArea::new(start, end, import.pos_info.file().clone()).unwrap(),
                ))
            })
    }

    /// Parses an import path
    ///
    /// An import path looks like this:
    ///
    /// "./math/floating_point/trigonometry"
    ///
    /// The import path starts and ends with a quote.
    /// The remainder is separated by slashes and consists of the following elements:
    /// 1. The import root
    ///     - This can either be:
    ///         - **./**: [`ImportRoot::CurrentModule`]
    ///             - This consumes the first part of the import path
    ///             - Despite being the separator, the slash id mandatory
    ///                 - `.` is not permitted
    ///                 - `./` is
    ///                     - This is the only case where trailing slashes are allowed
    ///         - **Anything else**: [`ImportRoot::Root`]
    ///             - This does not consume anything from the import path
    ///             - If it is empty, the path is invalid (see Returns)
    ///     - This works due to the representation of import paths in the AST
    /// 2. The path
    ///     - All parts not consumed by 1.
    ///         - Each part must match the following regex: `[a-zA-Z0-9][a-zA-Z0-9_-]*[a-zA-Z0-9]|[a-zA-Z0-9]`
    ///     - May be empty if and only if the import root is [`ImportRoot::CurrentModule`]
    ///
    /// # Parameter
    ///
    /// - **path**: The path to parse
    ///
    /// # Returns
    ///
    /// None if there was a parsing failure:
    /// 1. There were invalid characters in the path
    /// 2. If the import path is empty but may not be so
    ///
    /// Some with:
    /// - The import root
    /// - The path
    ///
    /// otherwise
    fn parse_import_path<'a>(path: &'a str) -> Option<(ImportRoot, Vec<String>)> {
        // Performance
        // While creating a new parser for each import is slower than caching, there is no good (safe) alternative
        // And it is very fast regardless (under 50µs per import)
        import_path_parser().parse(path).into_output()
    }

    /// Creates a parser for [`parse_import_path`]
    fn import_path_parser<'src>() -> impl Parser<'src, &'src str, (ImportRoot, Vec<String>)> {
        let current_module = just("./");
        let path_element = regex(r#"[a-zA-Z0-9][a-zA-Z0-9_-]*[a-zA-Z0-9]|[a-zA-Z0-9]"#);

        let path_elements = path_element
            .map(|elem: &str| elem.to_owned())
            .separated_by(just('/'));

        let full_path = choice((
            current_module
                .ignore_then(path_elements.clone().collect::<Vec<_>>())
                .map(|path| (ImportRoot::CurrentModule, path)),
            path_elements
                .clone()
                .at_least(1)
                .collect::<Vec<_>>()
                .map(|path| (ImportRoot::Root, path)),
        ));
        full_path
    }

    #[cfg(test)]
    mod test {
        use crate::top_level_parser::import_parser::parse_import_path;
        use ast::top_level::ImportRoot;

        #[test]
        fn parse_valid_import_path_should_be_ok_current_module() {
            let path = "./model/employee";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(parsed.1, vec!["model".to_owned(), "employee".to_owned()]);
        }

        #[test]
        fn parse_valid_import_path_should_be_ok_just_current_module() {
            let path = "./";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(parsed.1, Vec::<String>::new())
        }

        #[test]
        fn parse_valid_import_path_should_be_ok_underscore_numbers() {
            let path = "./model/employee_123";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(
                parsed.1,
                vec!["model".to_owned(), "employee_123".to_owned()]
            );
        }

        #[test]
        fn parse_valid_import_path_should_be_ok_hyphen_numbers() {
            let path = "./model/employee-123";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(
                parsed.1,
                vec!["model".to_owned(), "employee-123".to_owned()]
            );
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_trailing_slash() {
            let path = "./model/employee-123/";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_empty() {
            let path = "";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_invalid_chars() {
            let path = "./§14";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_starting_with_underscore() {
            let path = "./_123";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_ending_with_underscore() {
            let path = "./123_";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_path_traversal_attack() {
            let path = "./../../../../../etc/shadow";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }
    }
}

/// Parses a single function
fn function_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<Token, CodeFile>], ASTNode<Function<UntypedAST>>> {
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
