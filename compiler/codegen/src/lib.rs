#![warn(clippy::pedantic, clippy::nursery)]
mod context;
mod generators;
mod global_registry;
mod memory;
mod symbols;
mod types;

use crate::types::OptLevel;
use ast::TypedAST;
use ast::symbol::FunctionSymbol;
use bon::bon;
use inkwell::context::Context;
use std::rc::Rc;

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    opt_level: OptLevel,
    main_function: Rc<FunctionSymbol<TypedAST>>,
}

#[bon]
impl<'ctx> Codegen<'ctx> {
    #[builder]
    pub fn new(
        context: &'ctx Context,
        #[builder(default = OptLevel::O0)] opt_level: OptLevel,
        /// Must return void and take no arguments
        /// Must exist in the AST
        /// May not be external
        main_function: Rc<FunctionSymbol<TypedAST>>,
    ) -> Result<Self, CodegenCreationError> {
        if main_function.return_type().is_some() {
            return Err(CodegenCreationError::MainFunctionNonVoidReturn);
        }
        if !main_function.params().is_empty() {
            return Err(CodegenCreationError::MainFunctionTakesArguments);
        }
        Ok(Self {
            context,
            opt_level,
            main_function,
        })
    }
}

#[derive(Debug)]
pub enum CodegenCreationError {
    MainFunctionNonVoidReturn,
    MainFunctionTakesArguments,
}

#[cfg(test)]
mod tests {
    use crate::Codegen;
    use crate::types::OptLevel;
    use driver::pipeline::Pipeline;
    use driver::program_information::{ProgramInformation, Project};
    use driver::typed_ast_pipeline;
    use inkwell::context::Context;
    use io::WasomeLoader;
    use source::SourceMap;
    use std::fs;
    use std::fs::File;
    use std::io::Write;
    use std::path::PathBuf;
    use tempfile::TempDir;

    const FIBONACCI: &str =
        include_str!("../../driver/tests/test_programs/single_file/fibonacci.waso");
    fn setup_temp_project(files: &[(&str, &str)]) -> TempDir {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        for (rel_path, content) in files {
            let path = root.join(rel_path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(path, content).unwrap();
        }
        dir
    }
    #[test]
    fn test_syntax_check_multi_module() {
        let dir = setup_temp_project(&[("fibonacci/main.waso", FIBONACCI)]);
        let root = dir.path().to_path_buf();
        let main_file = PathBuf::from("main.waso");

        let prog_info = ProgramInformation::new(
            "fibonacci".to_string(),
            root.clone(),
            vec![Project::new(
                "fibonacci".to_string(),
                PathBuf::from("fibonacci"),
            )],
            "fibonacci".to_string(),
            main_file,
        )
        .unwrap();

        let mut sm = SourceMap::<WasomeLoader>::with_default(root);

        let tap = typed_ast_pipeline();
        let tast = tap.process((&prog_info, &mut sm)).unwrap().0;
        let main = tast
            .subdirectory_by_name("fibonacci")
            .unwrap()
            .file_by_name("main")
            .unwrap()
            .function_by_identifier(("main", &[]))
            .unwrap()
            .declaration_owned();
        let context = Context::create();
        let mut codegen = Codegen::builder()
            .context(&context)
            .opt_level(OptLevel::O0)
            .main_function(main)
            .build()
            .unwrap();
        let code = codegen.compile(&tast);
        let mut file = File::create("../code.wasm").unwrap();
        file.write_all(&code).unwrap();
    }
}
