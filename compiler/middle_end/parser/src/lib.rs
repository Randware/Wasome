use crate::top_level_parser::top_level_parser;
use ast::file::File;
use ast::{ASTNode, UntypedAST};
use chumsky::Parser;
use lexer::{Token, lex};
use shared::code_file::CodeFile;
use shared::code_reference::CodeArea;
use source::SourceFile;
use std::fmt::Debug;
use std::ops::Deref;
use std::path::PathBuf;

mod expression_parser;
mod misc_parsers;
mod statement_parser;
mod top_level_parser;

pub fn parse(to_parse: &SourceFile) -> Option<File<UntypedAST>> {
    let mut tokens = Vec::new();
    let mut all_ok = true;
    lex(to_parse.content()).for_each(|token| match token {
        Ok(inner_token) => tokens.push(inner_token),
        Err(_) => all_ok = false,
    });
    parse_tokens(tokens, filename_without_extension(to_parse)?.to_owned())
}

/// Gets the filename without extension from the provided [`SourceFile`]
///
/// # Failure
///
/// This may fail for a variety of reasons:
/// 1. **Empty Path**: The path of the file is empty
/// 2. **Non-UTF8 Characters**: The name of the file contains non-utf8 characters
/// 3. **Empty Filename**: The filename is empty. This differs from 1. by the fact that having
///    elements before the filepath does not exclude this failure condition
///
/// # Parameter
///
/// **from**: The [`SourceFile`] to extract the name from
///
/// # Return
///
/// The name without file extension
fn filename_without_extension(from: &SourceFile) -> Option<&str> {
    from.path()
        .iter()
        .next_back()?
        .to_str()?
        .split('.')
        .next_back()
}

fn parse_tokens(to_parse: Vec<Token>, file: String) -> Option<File<UntypedAST>> {
    let to_parse_with_file_info = inject_file_information(to_parse, file.clone());
    // It's not a good idea to put this into a static to prevent recreating the parser
    // as it would require a lot of HRTB stuff
    let parser = top_level_parser();
    parser
        .parse(&to_parse_with_file_info)
        .into_output()
        .map(|(imports, funcs)| File::new(file, imports, funcs))
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

pub(crate) fn combine_code_areas(a: &CodeArea, b: &CodeArea) -> Option<CodeArea> {
    if a.file() != b.file() {
        return None;
    }
    CodeArea::new(a.start().clone(), b.end().clone(), a.file().clone())
}

pub(crate) fn combine_code_areas_succeeding(a: &CodeArea, b: &CodeArea) -> CodeArea {
    combine_code_areas(a, b).unwrap()
}

/// Stores positional information and an element
/// 
/// This is like [`ASTNode`], except that it doesn't indicate that this belongs in an AST
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

    pub fn into_ast_node(self) -> ASTNode<T, Pos> {
        ASTNode::new(self.inner, self.pos_info)
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
    use lexer::TokenType;

    #[test]
    fn parse_full() {
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
        .map(prepare_token)
        .to_vec();

        let actual = parse_tokens(tokens, "test".to_string()).unwrap();

        let expected_func_name = "fibonacci";
        let func_name = {
            let function = actual.functions()[0].deref();
            function.declaration().name()
        };
        assert_eq!(expected_func_name, func_name);
    }
}

#[cfg(test)]
pub(crate) mod test_shared {
    use crate::PosInfoWrapper;
    use ast::ASTNode;
    use lexer::{Token, TokenType};
    use shared::code_file::CodeFile;
    use shared::code_reference::{CodeArea, CodeLocation};
    use std::fmt::Debug;
    use std::path::PathBuf;

    pub(crate) fn wrap_token(to_convert: TokenType) -> PosInfoWrapper<Token, CodeFile> {
        PosInfoWrapper::new(
            prepare_token(to_convert),
            CodeFile::new(PathBuf::from("test/test")),
        )
    }

    pub(crate) fn prepare_token(to_convert: TokenType) -> Token {
        Token {
            kind: to_convert,
            line: 0,
            span: Default::default(),
        }
    }

    pub(crate) fn wrap_in_astnode<T: PartialEq + Debug>(to_wrap: T) -> ASTNode<T> {
        ASTNode::new(
            to_wrap,
            CodeArea::new(
                CodeLocation::new(0, 0),
                CodeLocation::new(0, 0),
                CodeFile::new(PathBuf::from("test/test")),
            )
            .unwrap(),
        )
    }
}
