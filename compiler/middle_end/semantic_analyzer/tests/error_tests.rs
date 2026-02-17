#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use tempfile::TempDir;

    use driver::parser_driver::generate_untyped_ast;
    use driver::program_information::{ProgramInformation, Project};
    use io::WasomeLoader;
    use semantic_analyzer::analyze;
    use source::SourceMap;

    /// Creates a temporary project structure
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

    fn print_diagnostic_for_code(code: &str) {
        let dir = setup_temp_project(&[("error_project/app/main.waso", code)]);

        let root = dir.path().to_path_buf().join("error_project");
        let main_file = PathBuf::from("main.waso");

        let prog_info = ProgramInformation::new(
            "error_project".to_string(),
            root.clone(),
            vec![Project::new("app".to_string(), PathBuf::from("app"))],
            "app".to_string(),
            main_file,
        )
        .expect("Failed to create ProgramInformation");

        let mut source_map = SourceMap::<WasomeLoader>::new(root);

        let untyped_ast = match generate_untyped_ast(&prog_info, &mut source_map) {
            Some(ast) => ast,
            None => {
                panic!(
                    "Parsing failed! The parser could not find the file or encountered a syntax error. Code: {}",
                    code
                );
            }
        };

        match analyze(untyped_ast) {
            Ok(_) => panic!("Expected a semantic error, but analysis succeeded!"),
            Err(diagnostic) => {
                println!("\n--- DIAGNOSTIC OUTPUT START ---");
                diagnostic.print_snippets(&source_map).unwrap();
                println!("--- DIAGNOSTIC OUTPUT END ---\n");
            }
        }
    }

    fn smoke_run_for_code(code: &str) {
        let dir = setup_temp_project(&[("error_project/app/main.waso", code)]);

        let root = dir.path().to_path_buf().join("error_project");
        let main_file = PathBuf::from("main.waso");

        let prog_info = ProgramInformation::new(
            "error_project".to_string(),
            root.clone(),
            vec![Project::new("app".to_string(), PathBuf::from("app"))],
            "app".to_string(),
            main_file,
        )
        .expect("Failed to create ProgramInformation");

        let mut source_map = SourceMap::<WasomeLoader>::new(root);
        let untyped_ast = generate_untyped_ast(&prog_info, &mut source_map)
            .expect("Parsing failed in smoke test (file not found or syntax error)");

        match analyze(untyped_ast) {
            Ok(_) => {
                println!("\n--- SMOKE OUTPUT START (no semantic error) ---");
                println!("Semantic analysis succeeded.");
                println!("--- SMOKE OUTPUT END ---\n");
            }
            Err(diagnostic) => {
                println!("\n--- SMOKE OUTPUT START (semantic error) ---");
                diagnostic.print_snippets(&source_map).unwrap();
                println!("--- SMOKE OUTPUT END ---\n");
            }
        }
    }

    #[test]
    #[ignore = "Smoke test for parser+analyzer setup (manual run, no asserts on output)"]
    fn test_smoke_parser() {
        // Expected: NO specific error. This test only ensures parser+analyzer run.
        // Output should either include "Semantic analysis succeeded." OR a rendered diagnostic.
        // (If there is a diagnostic, the exact code is irrelevant for the smoke test.)
        let code = "fn main() {}\n";
        smoke_run_for_code(code);
    }

    #[test]
    #[ignore = "Manual visual inspection of E3001"]
    fn test_e3001_unknown_type() {
        // Expected: E3001
        // Message should contain "Unknown type 'UnknownType'".
        // Snippet should point to "UnknownType" in the variable declaration.
        // (Typical current text: "Unknown type 'UnknownType'" / "Type not found in this scope".)
        let code = "fn main() {\n    UnknownType x <- 5\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore = "Manual visual inspection of E3002"]
    fn test_e3002_unknown_symbol() {
        // Expected: E3002
        // Message should contain "Cannot find symbol 'undefined_variable'".
        // Snippet should point to "undefined_variable" in "a + undefined_variable".
        // (Typical current text: "Symbol not found".)
        let code = "fn main() {\n    s32 a <- 10\n    s32 b <- a + undefined_variable\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore = "Manual visual inspection of E3003"]
    fn test_e3003_type_mismatch() {
        // Expected: E3003
        // Message should indicate a type mismatch.
        // Snippet should point to the literal "true".
        // (Typical current text: "Type mismatch: expected 'S32', found 'Bool'" / "Expected S32, found Bool".)
        let code = "fn main() {\n    s32 x <- true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore = "Manual visual inspection of E3004"]
    fn test_e3004_already_declared() {
        // Expected: E3004
        // Message should state that "x" is already defined.
        // Snippet should point to the SECOND declaration of "x".
        // (Typical current text: "Variable 'x' is already defined" / "Name 'x' is already in use".)
        let code = "fn main() {\n    s32 x <- 1\n    s32 x <- 2\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore = "Manual visual inspection of E3005"]
    fn test_e3005_missing_return() {
        // Expected: E3005
        // Message should indicate that the function must return a value.
        // Snippet typically highlights the function block (start/end).
        // (Typical current text: "Function 'calc' must return a value" / "Not all control paths return a value".)
        let code = "fn calc() -> s32 {\n    s32 x <- 1\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore = "Manual visual inspection of E3006"]
    fn test_e3006_argument_mismatch() {
        // Expected: E3006
        // Message should indicate an argument mismatch (expected 2, found 1).
        // Snippet should point to the call "add(5)".
        // (Typical current text: "Argument mismatch: expected 2 arguments, found 1" / "Incorrect number of arguments".)
        let code = "fn add(s32 a, s32 b) -> s32 {\n    -> a + b\n}\n\nfn main() {\n    add(5)\n}\n";
        print_diagnostic_for_code(code);
    }
}
