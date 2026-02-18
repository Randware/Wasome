use crate::parser_driver::generate_untyped_ast;
use crate::pipeline::{Pipeline, from_func, from_infallible_func};
use crate::program_information::ProgramInformation;
use ast::{AST, TypedAST, UntypedAST};
use error::diagnostic::Diagnostic;
use io::FullIO;
use semantic_analyzer::analyze;
use source::SourceMap;

pub mod parser_driver;
pub mod pipeline;
pub mod program_information;

const INVALID_CHARS_IN_MAIN_FILE: &str = "E4001";
const MAIN_FILE_PROJECT_NOT_FOUND: &str = "E4002";
const MAIN_FILE_PATH_EMPTY: &str = "E4003";
const UNABLE_TO_LOAD_FILE: &str = "E4004";
const UNABLE_TO_LOAD_DIRECTORY: &str = "E4005";
const UNRESOLVED_IMPORT_ERROR: &str = "E4006";

/// Like [`syntax_check_pipeline`], but the pipeline is used immediately
///
/// # Errors
///
/// If an error is found, it is returned
pub fn syntax_check<'a, IO: FullIO>(
    to_check: &'a ProgramInformation,
    source_map: &'a mut SourceMap<IO>,
) -> Result<(), Diagnostic> {
    syntax_check_pipeline().process((to_check, source_map))
}

/// Creates a pipeline that
/// 1. reads the program according to `ProgramInformation`
/// 2. Parses the result of 1.
/// 3. Performs semantic analysis
/// 4. Voids all errors
#[must_use]
pub fn syntax_check_pipeline<IO: FullIO>()
-> impl for<'a> Pipeline<(&'a ProgramInformation, &'a mut SourceMap<IO>), Diagnostic, Output = ()> {
    let from: fn((_, &mut SourceMap<IO>)) = |_| ();
    typed_ast_pipeline().then(from_infallible_func::<_, (), (), _>(from))
}

#[must_use]
pub(crate) fn load_parse_pipeline<IO: FullIO>() -> impl for<'a> Pipeline<
    (&'a ProgramInformation, &'a mut SourceMap<IO>),
    Diagnostic,
    Output = (AST<UntypedAST>, &'a mut SourceMap<IO>),
> {
    let from: for<'a> fn((&'a _, &'a mut _)) -> Result<(_, &'a mut _), Diagnostic> =
        |(pi, sm)| generate_untyped_ast(pi, sm).map(|unt_ast| (unt_ast, sm));
    from_func(from)
}

#[must_use]
pub(crate) fn typed_ast_pipeline<IO: FullIO>() -> impl for<'a> Pipeline<
    (&'a ProgramInformation, &'a mut SourceMap<IO>),
    Diagnostic,
    Output = (AST<TypedAST>, &'a mut SourceMap<IO>),
> {
    let from: for<'a> fn((_, &'a mut _)) -> Result<(_, &'a mut _), Diagnostic> =
        |(ut_ast, sm)| Ok((analyze(ut_ast)?, sm));
    load_parse_pipeline().then(from_func(from))
}
