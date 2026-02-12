use crate::composite_parser::{enum_parser, struct_parser};
use crate::function_parser::function_parser;
use crate::input::ParserInput;
use crate::misc_parsers::{maybe_statement_separator, statement_separator};
use crate::top_level_parser::import_parser::import_parser;
use crate::{FileInformation, ParserSpan};
use ast::composite::{Enum, Struct};
use ast::top_level::{Function, Import};
use ast::{ASTNode, UntypedAST};
use chumsky::extra::Full;
use chumsky::prelude::*;
use io::FullIO;
use lexer::TokenType;

/// Parses all Top-Level elements in a file.
///
/// The resulting parser should only be used for the file provided with `file_information`
///
/// # Parameter
///
/// - **file_information**: Information about the to be parsed.
pub(crate) fn top_level_parser<'src, Loader: FullIO>(
    file_information: &'src FileInformation<Loader>,
) -> impl Parser<'src, ParserInput<'src>, TopLevelElements, Full<Rich<'src, TokenType, ParserSpan>, (), ()>> {
    let imports = maybe_statement_separator()
        .ignore_then(import_parser(file_information).then_ignore(statement_separator()))
        .repeated()
        .collect::<Vec<_>>();

    let top_level_element = choice((
        function_parser().boxed().map(TopLevelElement::Function),
        enum_parser().boxed().map(TopLevelElement::Enum),
        struct_parser().boxed().map(TopLevelElement::Struct),
    ));
    let top_level_elements = top_level_element
        .separated_by(statement_separator())
        .allow_trailing()
        .allow_leading()
        .collect::<Vec<_>>();

    maybe_statement_separator()
        .ignore_then(imports.then(top_level_elements))
        .map(|(imports, top_level_elements)| {
            let mut functions = Vec::new();
            let mut structs = Vec::new();
            let mut enums = Vec::new();

            top_level_elements.into_iter().for_each(|tle| match tle {
                TopLevelElement::Function(func) => functions.push(func),
                TopLevelElement::Struct(stru) => structs.push(stru),
                TopLevelElement::Enum(en) => enums.push(en),
            });

            (imports, functions, structs, enums)
        })
}
type TopLevelElements = (
    Vec<ASTNode<Import>>,
    Vec<ASTNode<Function<UntypedAST>>>,
    Vec<ASTNode<Struct<UntypedAST>>>,
    Vec<ASTNode<Enum<UntypedAST>>>,
);

/// Enum for temporarily storing data during parsing
enum TopLevelElement {
    Function(ASTNode<Function<UntypedAST>>),
    Struct(ASTNode<Struct<UntypedAST>>),
    Enum(ASTNode<Enum<UntypedAST>>),
}

mod import_parser {
    use crate::misc_parsers::{identifier_parser, string_parser, token_parser};
    use crate::{FileInformation, ParserSpan};
    use ast::symbol::ModuleUsageNameSymbol;
    use ast::top_level::{Import, ImportRoot};
    use ast::ASTNode;
    use chumsky::IterParser;
    use chumsky::Parser;

    use chumsky::error::Rich;
    use chumsky::prelude::{choice, just};

    use chumsky::regex::regex;
    use lexer::TokenType;

    use crate::input::ParserInput;
    use chumsky::extra::Full;
    use chumsky::span::Span as ChumskySpan;
    use io::FullIO;
    use std::rc::Rc;

    /// Parses a single import.
    ///
    /// The resulting parser should only be used for the file provided with `file_information`
    ///
    /// # Parameter
    ///
    /// - **file_information**: Information about the file to be parsed. This is currently only used
    ///   in order to resolve import paths correctly
    pub(super) fn import_parser<'src, Loader: FullIO>(
        file_information: &'src FileInformation<Loader>,
    ) -> impl Parser<'src, ParserInput<'src>, ASTNode<Import>, Full<Rich<'src, TokenType, ParserSpan>, (), ()>> {
        let ident = identifier_parser();
        let path = string_parser();
        token_parser(TokenType::Import)
            .then(path.then(token_parser(TokenType::As).ignore_then(ident).or_not()))
            .try_map(|(import, (path, usage_name)), _span| {
                let path_end_pos = path.span;
                let path = path.inner;
                let start = import.span.0.start();
                let end = usage_name
                    .as_ref()
                    .map(|inner| inner.span)
                    .unwrap_or(path_end_pos)
                    .0.end();
                let pos = import.span.context().span(start.0, end.0);
                let path = parse_import_path(&path).ok_or(Rich::custom(pos.into(), "Invalid import".to_string()))?;
                let use_as = usage_name
                    .map(|inner| inner.inner)
                    .unwrap_or_else(|| file_information.module_name().to_owned());

                // The pos info of a later token can never be before that of an earlier token
                // Therefore, this can never panic
                Ok(ASTNode::new(
                    Import::new(path.0, path.1, Rc::new(ModuleUsageNameSymbol::new(use_as))),
                    pos,
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
    fn parse_import_path(path: &str) -> Option<(ImportRoot, Vec<String>)> {
        if path.len() < 2 || !path.starts_with("\"") || !path.ends_with("\"") {
            return None;
        }
        // Performance
        // While creating a new parser for each import is slower than caching, there is no good (safe) alternative
        // And it is very fast regardless (under 50µs per import)

        // This will never panic as the slice is always valid due to out check above
        import_path_parser()
            .parse(&path[1..path.len() - 1])
            .into_output()
    }

    /// Creates a parser for [`parse_import_path`]
    fn import_path_parser<'src>() -> impl Parser<'src, &'src str, (ImportRoot, Vec<String>)> {
        let current_module = just("./");
        let path_element = regex(r#"[a-zA-Z0-9][a-zA-Z0-9_-]*[a-zA-Z0-9]|[a-zA-Z0-9]"#);

        let path_elements = path_element
            .map(|elem: &str| elem.to_owned())
            .separated_by(just('/'));

        choice((
            current_module
                .ignore_then(path_elements.clone().collect::<Vec<_>>())
                .map(|path| (ImportRoot::CurrentModule, path)),
            path_elements
                .clone()
                .at_least(1)
                .collect::<Vec<_>>()
                .map(|path| (ImportRoot::Root, path)),
        ))
    }

    #[cfg(test)]
    mod test {
        use crate::top_level_parser::import_parser::parse_import_path;
        use ast::top_level::ImportRoot;

        #[test]
        fn parse_valid_import_path_should_be_ok_current_module() {
            let path = "\"./model/employee\"";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(parsed.1, vec!["model".to_owned(), "employee".to_owned()]);
        }

        #[test]
        fn parse_valid_import_path_should_be_ok_just_current_module() {
            let path = "\"./\"";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(parsed.1, Vec::<String>::new())
        }

        #[test]
        fn parse_valid_import_path_should_be_ok_underscore_numbers() {
            let path = "\"./model/employee_123\"";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(
                parsed.1,
                vec!["model".to_owned(), "employee_123".to_owned()]
            );
        }

        #[test]
        fn parse_valid_import_path_should_be_ok_hyphen_numbers() {
            let path = "\"./model/employee-123\"";

            let parsed = parse_import_path(path).unwrap();

            assert_eq!(parsed.0, ImportRoot::CurrentModule);
            assert_eq!(
                parsed.1,
                vec!["model".to_owned(), "employee-123".to_owned()]
            );
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_trailing_slash() {
            let path = "\"./model/employee-123/\"";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_empty() {
            let path = "\"\"";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_invalid_chars() {
            let path = "\"./§14\"";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_starting_with_underscore() {
            let path = "\"./_123\"";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_ending_with_underscore() {
            let path = "\"./123_\"";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_path_traversal_attack() {
            let path = "\"./../../../../../etc/shadow\"";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }

        #[test]
        fn parse_invalid_import_path_should_be_error_no_quotes() {
            let path = "math/trigonimetry";

            let parsed = parse_import_path(path);

            assert!(parsed.is_none());
        }
    }
}
