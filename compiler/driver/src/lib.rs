use crate::parser_driver::generate_untyped_ast;
use crate::pipeline::{Pipeline, from_func, from_infallible_func};
use crate::program_information::ProgramInformation;
use ast::{AST, TypedAST, UntypedAST};
use semantic_analyzer::analyze;
use source::SourceMap;

pub mod parser_driver;
pub mod pipeline;
pub mod program_information;

pub fn syntax_check<'a>(
    to_check: &'a ProgramInformation,
    source_map: &'a mut SourceMap,
) -> Option<()> {
    syntax_check_pipeline().process((to_check, source_map)).ok()
}
pub fn syntax_check_pipeline()
-> impl for<'a> Pipeline<(&'a ProgramInformation, &'a mut SourceMap), (), Output = ()> {
    let from: fn((_, &mut SourceMap)) = |_| ();
    typed_ast_pipeline().then(from_infallible_func::<_, (), (), _>(from))
}

pub(crate) fn load_parse_pipeline() -> impl for<'a> Pipeline<
    (&'a ProgramInformation, &'a mut SourceMap),
    (),
    Output = (AST<UntypedAST>, &'a mut SourceMap),
> {
    // This is a false positive
    // Removing the unused parens turns this into a Result with three type parameters instead of a
    // tuple as type parameter
    #[allow(unused_parens)]
    let from: for<'a> fn((&'a _, &'a mut _)) -> Result<((_, &'a mut _)), ()> = |(pi, sm)| {
        generate_untyped_ast(pi, sm)
            .ok_or(())
            .map(|unt_ast| (unt_ast, sm))
    };
    from_func(from)
}

pub(crate) fn typed_ast_pipeline() -> impl for<'a> Pipeline<
    (&'a ProgramInformation, &'a mut SourceMap),
    (),
    Output = (AST<TypedAST>, &'a mut SourceMap),
> {
    let from: for<'a> fn((_, &'a mut _)) -> Result<(_, &'a mut _), ()> =
        |(ut_ast, sm)| analyze(ut_ast).map(|t_ast| (t_ast, sm)).ok_or(());
    load_parse_pipeline().then(from_func(from))
}
