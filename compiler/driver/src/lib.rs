use crate::error::DriverError;
use crate::parser_driver::generate_untyped_ast;
use crate::pipeline::{Pipeline, from_func, from_infallible_func};
use crate::program_information::{
    FullProgramInformation, LoadBinaryProgramInformation, LoadInformation,
};
use crate::source_collector::{CollectionError, collect_program};
use crate::source_element::WasomeProgram;
use ::error::diagnostic::Diagnostic;
use ast::symbol::FunctionSymbol;
use ast::{AST, TypedAST, UntypedAST};
use codegen::{CodegenCreationError, codegen};
use io::FullIO;
use linker::{LinkableFile, Linker};
use semantic_analyzer::analyze;
use source::SourceMap;
use std::rc::Rc;

pub mod error;
pub mod parser_driver;
pub mod pipeline;
pub mod program_information;
pub mod source_collector;
pub mod source_element;

const MAIN_FUNCTION_NOT_FOUND_CODE: &str = "E4001";
const MAIN_FUNCTION_NOT_FOUND_MSG: &str = "The main function was not found";
const MAIN_FUNCTION_TAKES_ARGUMENTS_CODE: &str = "E4002";
const MAIN_FUNCTION_TAKES_ARGUMENTS_MSG: &str = "The main function must not take arguments";

const MAIN_FUNCTION_NONVOID_RETURN_CODE: &str = "E4003";
const MAIN_FUNCTION_NONVOID_RETURN_MSG: &str = "The main function must return void";

const LIKE_ERROR_CODE: &str = "E4004";
const LINK_ERROR_MSG: &str = "Linking error:";

/// Like [`syntax_check_pipeline`], but the pipeline is used immediately
///
/// # Errors
///
/// If an error is found, it is returned
pub fn syntax_check<'a, IO: FullIO>(
    to_check: &'a impl LoadBinaryProgramInformation,
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
pub fn syntax_check_pipeline<IO: FullIO, T: LoadBinaryProgramInformation>()
-> impl for<'a> Pipeline<(&'a T, &'a mut SourceMap<IO>), Diagnostic, Output = ()> {
    let from: fn((_, &mut SourceMap<IO>, &'_ _)) = |_| ();
    typed_ast_pipeline().then(from_infallible_func::<_, (), (), _>(from))
}

pub fn compile_link_pipeline<IO: FullIO, T: FullProgramInformation>() -> impl for<'a> Pipeline<
    (&'a T, &'a mut SourceMap<IO>, Vec<LinkableFile>),
    Diagnostic,
    Output = LinkableFile,
> {
    let from: for<'a> fn((&'a T, &mut SourceMap<IO>, Vec<LinkableFile>)) -> _ =
        |(prog_info, sm, mut link_files)| {
            let compiled = compile_pipeline().process((prog_info, sm))?;
            link_files.push(LinkableFile::new(compiled));
            let mut linker = Linker::builder();
            linker.add_files(link_files);
            linker.build().link().map_err(|(_, err)| {
                Diagnostic::builder()
                    .code(LIKE_ERROR_CODE)
                    .message(format!("{LINK_ERROR_MSG} {err}"))
                    .build()
            })
        };
    from_func(from)
}

pub fn compile_pipeline<IO: FullIO, T: FullProgramInformation>()
-> impl for<'a> Pipeline<(&'a T, &'a mut SourceMap<IO>), Diagnostic, Output = Vec<u8>> {
    let from: for<'a> fn((AST<TypedAST>, &mut SourceMap<IO>, &'a T)) -> _ =
        |(tast, _sm, prog_info)| {
            let main_func = extract_main_func(&tast, &prog_info).ok_or_else(|| {
                Diagnostic::builder()
                    .code(MAIN_FUNCTION_NOT_FOUND_CODE)
                    .message(MAIN_FUNCTION_NOT_FOUND_MSG)
                    .build()
            })?;
            let compiled = codegen(prog_info.opt_level(), main_func, tast);

            compiled.map_err(|err| {
                match err {
                    CodegenCreationError::MainFunctionNonVoidReturn => Diagnostic::builder()
                        .code(MAIN_FUNCTION_NONVOID_RETURN_CODE)
                        .message(MAIN_FUNCTION_NONVOID_RETURN_MSG),
                    CodegenCreationError::MainFunctionTakesArguments => Diagnostic::builder()
                        .code(MAIN_FUNCTION_TAKES_ARGUMENTS_CODE)
                        .message(MAIN_FUNCTION_TAKES_ARGUMENTS_MSG),
                }
                .build()
            })
        };
    typed_ast_pipeline().then(from_func(from))
}

fn extract_main_func<T: FullProgramInformation>(
    tast: &AST<TypedAST>,
    prog_info: &&T,
) -> Option<Rc<FunctionSymbol<TypedAST>>> {
    let main_project = tast.subdirectory_by_name(prog_info.main_project())?;
    let mut curr_dir = main_project;
    let main_path_len = prog_info.main_file().iter().count();
    for (i, path_elem) in prog_info.main_file().iter().enumerate() {
        if main_path_len - 1 == i {
            continue;
        }
        curr_dir = curr_dir.subdirectory_by_name(&path_elem.to_string_lossy())?;
    }
    let main_file = curr_dir.file_by_name(
        prog_info
            .main_file()
            .iter()
            .next_back()?
            .to_string_lossy()
            .rsplit_once('.')?
            .0,
    )?;
    main_file
        .function_by_identifier(("main", &[]))
        .map(|func| func.declaration_owned())
}

#[must_use]
pub(crate) fn load_parse_pipeline<IO: FullIO, T: LoadInformation>() -> impl for<'a> Pipeline<
    (&'a T, &'a mut SourceMap<IO>),
    Diagnostic,
    Output = (AST<UntypedAST>, &'a mut SourceMap<IO>, &'a T),
> {
    let from: for<'a> fn((_, &'a mut _, &'a _)) -> Result<(_, &'a mut _, &'a _), Diagnostic> =
        |(pi, sm, prog_info)| generate_untyped_ast(pi, sm).map(|unt_ast| (unt_ast, sm, prog_info));
    load_pipeline().then(from_func(from))
}

#[must_use]
pub fn typed_ast_pipeline<IO: FullIO, T: LoadBinaryProgramInformation>() -> impl for<'a> Pipeline<
    (&'a T, &'a mut SourceMap<IO>),
    Diagnostic,
    Output = (AST<TypedAST>, &'a mut SourceMap<IO>, &'a T),
> {
    let from: for<'a> fn((_, &'a mut _, &'a _)) -> Result<(_, &'a mut _, &'a _), Diagnostic> =
        |(ut_ast, sm, prog_info)| Ok((analyze(ut_ast)?, sm, prog_info));
    load_parse_pipeline().then(from_func(from))
}

#[must_use]
pub fn load_pipeline<IO: FullIO, T: LoadInformation>() -> impl for<'a> Pipeline<
    (&'a T, &'a mut SourceMap<IO>),
    Diagnostic,
    Output = (WasomeProgram, &'a mut SourceMap<IO>, &'a T),
> {
    let from: for<'a> fn((&'a _, &'a mut _)) -> Result<(_, &'a mut _, &'a _), Diagnostic> =
        |(program_info, load_from)| {
            let program = collect_program(program_info, load_from);
            program
                .map(|wp| (wp, load_from, program_info))
                .map_err(|err| match err {
                    CollectionError::Io(err) => DriverError::Io { source: err }.into(),
                })
        };
    from_func(from)
}
