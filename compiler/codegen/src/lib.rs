#![warn(clippy::pedantic, clippy::nursery)]
mod context;
mod generators;
mod global_registry;
mod memory;
mod symbols;
mod types;

use bon::bon;
use inkwell::context::Context;

use crate::types::OptLevel;

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    opt_level: OptLevel,
}

#[bon]
impl<'ctx> Codegen<'ctx> {
    #[builder]
    pub const fn new(
        context: &'ctx Context,
        #[builder(default = OptLevel::O0)] opt_level: OptLevel,
    ) -> Self {
        Self { context, opt_level }
    }
}

#[cfg(test)]
mod tests {
    use crate::Codegen;
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
        let context = Context::create();
        let mut codegen = Codegen::builder().context(&context).build();
        let code = codegen.compile(&tast);
        let mut file = File::create("../code.wasm").unwrap();
        file.write_all(&code).unwrap();
    }
}
