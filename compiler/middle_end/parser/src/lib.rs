use crate::top_level_parser::top_level_parser;
use ast::file::File;
use ast::visibility::Visibility;
use ast::{ASTNode, UntypedAST};
use chumsky::Parser;
use io::FullIO;
use lexer::{Token, TokenType, lex};
use shared::code_file::CodeFile;
use shared::code_reference::{CodeArea, CodeLocation};
use source::types::FileID;
use source::{SourceFile, SourceMap};
use std::fmt::Debug;
use std::ops::Deref;
use std::path::PathBuf;

mod composite_parser;
mod expression_parser;
mod function_parser;
mod misc_parsers;
mod statement_parser;
mod top_level_parser;

/// Information about a file
///
/// This is used to provide the parser with all the required information
pub struct FileInformation<'a, Loader: FullIO> {
    /// The file
    ///
    /// Must always be contained within source_map in order for `FileInformation` to be valid
    file: FileID,
    /// The name of the module where the file is located
    module_name: &'a str,
    /// "Contains" the file
    source_map: &'a SourceMap<Loader>,
}

impl<'a, Loader: FullIO> FileInformation<'a, Loader> {
    /// Attempts to create a new FileInformation
    ///
    /// # Params
    ///
    /// - **file**: The file, must be included in `source_map`
    /// - **module_name**: The name of the module where `file` is located
    /// - **source_map**: The source map that includes **file**
    ///
    /// # Return
    ///
    /// **None** if `file` is not included in `source_map`
    /// **The new instance** otherwise
    pub fn new(
        file: FileID,
        module_name: &'a str,
        source_map: &'a SourceMap<Loader>,
    ) -> Option<Self> {
        source_map.get_file(&file)?;
        Some(Self {
            file,
            module_name,
            source_map,
        })
    }

    pub fn file(&self) -> FileID {
        self.file
    }

    pub fn module_name(&self) -> &'a str {
        self.module_name
    }

    pub fn source_map(&self) -> &'a SourceMap<Loader> {
        self.source_map
    }

    /// Gets the content of the file of self
    pub fn file_content(&self) -> &'a str {
        self.file_resolved().content()
    }

    /// Resolves the contained file and returns it
    fn file_resolved(&self) -> &'a SourceFile {
        // unwrap: `FileInformation` is only valid if its source map contains its file
        // `get_file()` returns None if and only if the provided file is not in the `SourceMap`
        // Therefore, None is never returned and we never panic
        self.source_map().get_file(&self.file).unwrap()
    }

    /// Gets the filename without extension from the contained file
    ///
    /// # Failure
    ///
    /// This may fail for a variety of reasons:
    /// 1. **Empty Path**: The path of the file is empty
    /// 2. **Non-UTF8 Characters**: The name of the file contains non-utf8 characters
    /// 3. **Empty Filename**: The filename is empty. This differs from 1. by the fact that having
    ///    elements before the filepath does not exclude this failure condition
    ///
    /// # Return
    ///
    /// The name without file extension
    fn filename_without_extension(&self) -> Option<&str> {
        self.file_resolved()
            .path()
            .iter()
            .next_back()?
            .to_str()?
            .split('.')
            .next()
    }
}
/// Parses the provided [`SourceFile`] to a file from the ast
///
/// # Parameter
///
/// **to_parse**: The [`SourceFile`] to parse
///
/// # Return
///
/// - **None**: The parsing failed // TODO: Add error handling support
/// - **Some**: The parsing succeeded and the result is contained within
pub fn parse<Loader: FullIO>(to_parse: FileInformation<'_, Loader>) -> Option<File<UntypedAST>> {
    let content = to_parse.file_content();
    let mut tokens = Vec::new();
    let mut all_ok = true;
    lex(content).for_each(|token| match token {
        Ok(inner_token) => tokens.push(inner_token),
        Err(_) => all_ok = false,
    });
    if !all_ok {
        return None;
    }
    parse_tokens(tokens, &to_parse)
}

fn parse_tokens<Loader: FullIO>(
    to_parse: Vec<Token>,
    file_information: &FileInformation<'_, Loader>,
) -> Option<File<UntypedAST>> {
    let filename = file_information.filename_without_extension()?.to_owned();
    let to_parse_with_file_info = prepare_tokens(to_parse, filename.clone());
    // It's not a good idea to put this into a static to prevent recreating the parser
    // as it would require unsafe code
    let parser = top_level_parser(file_information);
    parser.parse(&to_parse_with_file_info).into_output().map(
        |(imports, functions, structs, enums)| {
            File::new(filename, imports, functions, enums, structs)
        },
    )
}

fn prepare_tokens(raw_tokens: Vec<Token>, file: String) -> Vec<PosInfoWrapper<TokenType>> {
    let code_file = CodeFile::new(PathBuf::from(file));
    raw_tokens
        .into_iter()
        // Remove all comments
        .filter(|token| !matches!(token.kind, TokenType::Comment(_)))
        // End will never be before start
        .map(|token| {
            PosInfoWrapper::new(
                token.kind,
                CodeArea::new(
                    CodeLocation::new(token.line, token.span.start),
                    CodeLocation::new(token.line, token.span.end),
                    code_file.clone(),
                )
                .unwrap(),
            )
        })
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

fn remove_pos_info_from_vec<T: PartialEq + Debug>(
    type_parameters: Vec<PosInfoWrapper<T>>,
) -> Vec<T> {
    type_parameters
        .into_iter()
        .map(|type_param| type_param.inner)
        .collect::<Vec<_>>()
}

pub(crate) fn map_visibility(visibility: Option<&PosInfoWrapper<TokenType>>) -> Visibility {
    visibility
        .map(|_| Visibility::Public)
        .unwrap_or(Visibility::Private)
}

#[cfg(test)]
pub(crate) mod test_shared {
    use crate::PosInfoWrapper;
    use ast::ASTNode;
    use lexer::TokenType;
    use shared::code_file::CodeFile;
    use shared::code_reference::{CodeArea, CodeLocation};
    use std::fmt::Debug;
    use std::path::PathBuf;

    pub(crate) fn wrap_token(to_convert: TokenType) -> PosInfoWrapper<TokenType> {
        PosInfoWrapper::new(
            to_convert,
            CodeArea::new(
                CodeLocation::new(0, 0),
                CodeLocation::new(0, 0),
                CodeFile::new(PathBuf::from("test/test")),
            )
            .unwrap(),
        )
    }

    pub(crate) fn wrap_in_ast_node<T: PartialEq + Debug>(to_wrap: T) -> ASTNode<T> {
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
