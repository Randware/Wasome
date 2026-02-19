use crate::input::ParserInput;
use crate::top_level_parser::top_level_parser;
use ast::UntypedAST;
use ast::file::File;
use ast::visibility::Visibility;
use chumsky::Parser;
use chumsky::error::{RichPattern, RichReason};
use chumsky::prelude::Rich;
use chumsky::span::{Span, Spanned, WrappingSpan};
use error::diagnostic::{Diagnostic, Snippet};
use io::FullIO;
use lexer::tokens::LexError;
use lexer::{Token, TokenType, lex};
use source::types::{BytePos, FileID, Span as SourceSpan};
use source::{SourceFile, SourceMap};
use std::fmt::Debug;
use std::ops::Range;

mod composite_parser;
mod expression_parser;
mod function_parser;
mod input;
mod misc_parsers;
mod statement_parser;
mod top_level_parser;

const INVALID_FILE_CODE: &str = "E2001";
const PARSING_CODE: &str = "E2002";
const LEXING_CODE: &str = "E1001";

/// Newtype to bypass trait implementation rules
#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct ParserSpan(pub SourceSpan);

impl ParserSpan {
    fn merge(self, other: Self) -> Option<Self> {
        Some(Self(self.0.merge(other.0)?))
    }

    fn set_end(&mut self, end: BytePos) {
        self.0.end = end;
    }
}
impl From<SourceSpan> for ParserSpan {
    fn from(value: SourceSpan) -> Self {
        Self(value)
    }
}

impl Into<SourceSpan> for ParserSpan {
    fn into(self) -> SourceSpan {
        self.0
    }
}

impl Span for ParserSpan {
    type Context = FileID;
    type Offset = BytePos;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self(context.span(range.start.0, range.end.0))
    }

    fn context(&self) -> Self::Context {
        self.0.file_id
    }

    fn start(&self) -> Self::Offset {
        self.0.start
    }

    fn end(&self) -> Self::Offset {
        self.0.end
    }
}

impl<T> WrappingSpan<T> for ParserSpan {
    type Spanned = Spanned<T, ParserSpan>;

    fn make_wrapped(self, inner: T) -> Self::Spanned {
        Spanned { inner, span: self }
    }

    fn inner_of(spanned: &Self::Spanned) -> &T {
        &spanned.inner
    }

    fn span_of(spanned: &Self::Spanned) -> &Self {
        &spanned.span
    }
}

// It's impossible to make this a method
pub(crate) fn map<T, S, O>(curr: Spanned<T, S>, mapper: impl FnOnce(T) -> O) -> Spanned<O, S> {
    Spanned {
        inner: mapper(curr.inner),
        span: curr.span,
    }
}
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
/// - **Err**: The parsing failed
/// - **Some**: The parsing succeeded and the result is contained within
pub fn parse<Loader: FullIO>(
    to_parse: FileInformation<'_, Loader>,
) -> Result<File<UntypedAST>, Diagnostic> {
    let content = to_parse.file_content();
    let mut tokens = Vec::new();
    let mut first_err = None;
    lex(content).for_each(|token| match token {
        Ok(inner_token) => tokens.push(inner_token),
        Err(err) => {
            if first_err.is_none() {
                first_err = Some(err)
            }
        }
    });
    if let Some(first_err) = first_err {
        return Err(lexer_error(to_parse.file, first_err));
    }
    parse_tokens(tokens, &to_parse)
}

fn parse_tokens<Loader: FullIO>(
    to_parse: Vec<Token>,
    file_information: &FileInformation<'_, Loader>,
) -> Result<File<UntypedAST>, Diagnostic> {
    let filename = file_information
        .filename_without_extension()
        .ok_or_else(|| invalid_filename_error(file_information.file_resolved()))?
        .to_owned();
    let to_parse_with_file_info = prepare_tokens(to_parse, file_information.file);
    let input = ParserInput::new(&to_parse_with_file_info);
    // It's not a good idea to put this into a static to prevent recreating the parser
    // as it would require unsafe code
    let parser = top_level_parser(file_information);
    parser
        .parse(input)
        .into_result()
        .map(|(imports, functions, structs, enums)| {
            File::new(filename, imports, functions, enums, structs)
        })
        .map_err(|mut err| {
            let err = err.pop().unwrap();
            parser_error(file_information.file(), err)
        })
}

fn invalid_filename_error(file: &SourceFile) -> Diagnostic {
    Diagnostic::builder()
        .message(format!(
            "The at {} has an invalid name",
            file.path().to_string_lossy()
        ))
        .code(INVALID_FILE_CODE)
        .build()
}

fn parser_error(file: FileID, err: Rich<TokenType, ParserSpan>) -> Diagnostic {
    let span = *err.span();
    let msg = match err.into_reason() {
        RichReason::ExpectedFound { expected, found } => {
            let expected = expected
                .into_iter()
                .map(|pat| match pat {
                    RichPattern::Token(tok) => tok.token_to_printable_string().to_string(),
                    RichPattern::EndOfInput => "end of input".to_string(),
                    // Future improvement: Use custom for all other errors
                    RichPattern::SomethingElse | RichPattern::Any => "something else".to_string(),
                    _ => unreachable!("This should never happen"),
                })
                .collect::<Vec<_>>()
                .join(" or ");
            let found = match found {
                None => "end of input".to_string(),
                Some(tok) => tok.token_to_printable_string().to_string(),
            };
            format!("Expected {expected}, but found {found}")
        }
        RichReason::Custom(msg) => msg,
    };
    Diagnostic::builder()
        .message("Token mismatch")
        .code(PARSING_CODE)
        .snippet(
            Snippet::builder()
                .file(file)
                .primary(span.0.start..span.0.end, msg)
                .build(),
        )
        .help("Provide a valid token".to_string())
        .build()
}

fn lexer_error(file: FileID, err: LexError) -> Diagnostic {
    let msg = err.inner.to_string();
    Diagnostic::builder()
        .message("Invalid token")
        .code(LEXING_CODE)
        .snippet(
            Snippet::builder()
                .file(file)
                .primary(err.byte_pos.start..err.byte_pos.end, msg)
                .build(),
        )
        .build()
}
fn prepare_tokens(raw_tokens: Vec<Token>, file: FileID) -> Vec<Spanned<TokenType, ParserSpan>> {
    raw_tokens
        .into_iter()
        // Remove all comments
        .filter(|token| !matches!(token.kind, TokenType::Comment(_)))
        // End will never be before start
        .map(|token| {
            ParserSpan(file.span(token.span.start as u32, token.span.end as u32))
                .make_wrapped(token.kind)
        })
        .collect()
}

fn unspan_vec<T: PartialEq + Debug, U>(type_parameters: Vec<Spanned<T, U>>) -> Vec<T> {
    type_parameters
        .into_iter()
        .map(|type_param| type_param.inner)
        .collect::<Vec<_>>()
}

pub(crate) fn map_visibility(visibility: Option<&Spanned<TokenType, ParserSpan>>) -> Visibility {
    visibility
        .map(|_| Visibility::Public)
        .unwrap_or(Visibility::Private)
}

#[cfg(test)]
pub(crate) mod test_shared {
    use crate::ParserSpan;
    use crate::input::ParserInput;
    use ast::ASTNode;
    use chumsky::span::Spanned;
    use lexer::TokenType;
    use source::types::FileID;
    use std::fmt::Debug;

    pub(crate) fn wrap_token(to_convert: TokenType) -> Spanned<TokenType, ParserSpan> {
        Spanned {
            inner: to_convert,
            span: FileID::from(0).span(0, 10).into(),
        }
    }

    pub(crate) fn wrap_in_ast_node<T: PartialEq + Debug>(to_wrap: T) -> ASTNode<T> {
        ASTNode::new(to_wrap, FileID::from(0).span(0, 10))
    }

    pub(crate) fn convert_nonempty_input(
        source: &[Spanned<TokenType, ParserSpan>],
    ) -> ParserInput<'_> {
        ParserInput::new(source)
    }
}
