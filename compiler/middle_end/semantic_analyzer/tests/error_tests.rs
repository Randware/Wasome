#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use tempfile::TempDir;

    use driver::parser_driver::generate_untyped_ast;
    use driver::program_information::{ProgramInformation, Project};
    use driver::source_collector::collect_program;
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

        let mut source_map = SourceMap::<WasomeLoader>::with_default(root);

        let untyped_ast = match generate_untyped_ast(
            collect_program(&prog_info, &mut source_map).unwrap(),
            &mut source_map,
        )
            .ok()
        {
            Some(ast) => ast,
            None => {
                panic!(
                    "Parsing failed! The parser could not find the file or encountered a syntax error. Code:\n{}",
                    code
                );
            }
        };

        match analyze(untyped_ast) {
            Ok(_) => panic!("Expected a semantic error, but analysis succeeded! Code:\n{}", code),
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

        let mut source_map = SourceMap::<WasomeLoader>::with_default(root);
        let untyped_ast = generate_untyped_ast(
            collect_program(&prog_info, &mut source_map).unwrap(),
            &mut source_map,
        )
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
    #[ignore]
    fn test_smoke_parser() {
        // Expected: NO specific error. This test only ensures parser+analyzer run.
        // Output should either include "Semantic analysis succeeded." OR a rendered diagnostic.
        let code = "fn main() {}\n";
        smoke_run_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3001_unknown_type() {
        // Expected: E3001
        // Message should contain "Unknown type 'UnknownType'".
        // Snippet should point to "UnknownType" in the variable declaration.
        let code = "fn main() {\n    UnknownType x <- 5\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3002_unknown_symbol() {
        // Expected: E3002
        // Message should contain "Cannot find symbol 'undefined_variable'".
        // Snippet should point to "undefined_variable" in "a + undefined_variable".
        let code = "fn main() {\n    s32 a <- 10\n    s32 b <- a + undefined_variable\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3003_type_mismatch() {
        // Expected: E3003
        // Message should indicate a type mismatch (expected 'S32', found 'Bool').
        // Snippet should point to the literal "true".
        let code = "fn main() {\n    s32 x <- true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3004_already_declared_global() {
        // Expected: E3004
        // Message should state that a symbol is already declared.
        // We test this via parameters because the parser might deduplicate global functions.
        let code = "fn main(s32 x, s32 x) {}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3005_missing_return() {
        // Expected: E3005
        // Message should indicate that the function must return a value.
        // Snippet typically highlights the function block.
        let code = "fn calc() -> s32 {\n    s32 x <- 1\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3006_argument_mismatch() {
        // Expected: E3006
        // Message should indicate an argument mismatch (expected 2 arguments, found 1).
        // Snippet should point to the call "add(5)".
        let code = "fn add(s32 a, s32 b) -> s32 {\n    -> a + b\n}\n\nfn main() {\n    add(5)\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3007_condition_not_boolean() {
        // Expected: E3007
        // Message should indicate that the condition expression is not of type Bool.
        // Snippet should point to the "5" in the if statement.
        let code = "fn main() {\n    if (5) {\n        s32 x <- 1\n    }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3008_break_outside_loop() {
        // Expected: E3008
        // Message should indicate that break is used outside of a loop.
        // Snippet should point to "break".
        let code = "fn main() {\n    if (true) {\n        break\n    }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3010_generic_argument_count_mismatch() {
        // Expected: E3010
        // Message should state the wrong number of type parameters.
        // We declare TWO parameters, but only pass ONE.
        // This is guaranteed to trigger E3010 in the Semantic Analyzer.
        let code = "struct Pair[T, U] { T first }\nfn main() {\n    Pair[s32] p <- new Pair[s32] { first <- 1 }\n}\n";
        print_diagnostic_for_code(code);
    }


    #[test]
    #[ignore]
    fn test_e3014_method_on_non_struct() {
        // Expected: E3014
        // Message should state that a method cannot be called on a non-struct type.
        // Snippet should point to "x" in "x.do_something()".
        let code = "fn main() {\n    s32 x <- 5\n    x.do_something()\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3015_symbol_kind_mismatch() {
        // Expected: E3015
        // Message should state that 'not_a_struct' is not a struct.
        // Snippet should point to the struct initialization attempt.
        let code = "fn NotAStruct() {}\nfn main(NotAStruct obj) {}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3016_unknown_field() {
        // Expected: E3016
        // Message should state that the field 'y' is not found in the struct 'Vector'.
        // Snippet should point to the access of 'y'.
        let code = "struct Vector {\n    s32 x\n}\nfn main(Vector v) {\n    s32 y <- v.y\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3017_field_access_on_non_struct() {
        // Expected: E3017
        // Message should indicate that field access is not allowed on non-struct types.
        // Snippet should point to "number.field".
        let code = "fn main() {\n    s32 number <- 10\n    s32 invalid <- number.field\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3018_void_returns_value() {
        // Expected: E3018
        // Message should state that a void function must not return a value.
        // Snippet should point to "-> 5".
        let code = "fn do_nothing() {\n    -> 5\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3019_missing_return_value() {
        // Expected: E3019
        // Message should indicate that a return value is missing (expected 's32').
        // Snippet should point to the empty "->".
        let code = "fn get_five() -> s32 {\n    -> \n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3020_primitive_with_type_parameters() {
        // Expected: E3020
        // Message should indicate that primitives cannot have type parameters.
        // S32 großgeschrieben! Der Parser lässt das als Identifier durch,
        // danach knallt es im Semantic Analyzer (entweder als E3020 oder E3001).
        let code = "fn main() {\n    S32[bool] invalid_primitive <- 5\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3021_unsupported_binary_operation() {
        // Expected: E3021
        // Message should state that the binary operation is not supported between 's32' and 'bool'.
        // Snippet should point to the binary operation "5 + true".
        let code = "fn main() {\n    s32 x <- 5 + true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3022_unsupported_unary_operation() {
        // Expected: E3022
        // Message should state that the unary operation is unsupported for the type 'bool'.
        // Snippet should point to "-true".
        let code = "fn main() {\n    bool x <- -true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3023_invalid_literal_format() {
        // Expected: E3023
        // Message should indicate that the literal format is invalid or out of bounds.
        // Snippet should point to the literal "999999999999999999".
        let code = "fn main() {\n    s32 x <- 999999999999999999\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3024_unknown_enum_variant() {
        // Expected: E3024
        // Message should state that the variant 'Blue' is not found in enum 'Color'.
        // Snippet should point to the enum usage.
        let code = "enum Color {\n    Red\n}\nfn main(Color c) {\n    Color x <- Color::Blue\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3025_local_variable_shadowing() {
        // Expected: E3025
        // Message should state that the variable 'a' is already defined in the current scope.
        // Snippet should point to the second declaration of 'a'.
        let code = "fn main() {\n    s32 a <- 1\n    s32 a <- 2\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3026_void_used_as_value() {
        // Expected: E3026
        // Message should indicate that a void function/method cannot be used as a value.
        // Snippet should point to "action()".
        let code = "fn action() {}\nfn main() {\n    s32 result <- action()\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3027_struct_initialization_mismatch() {
        // Expected: E3027
        // Message should state that the initialization parameters do not match the fields of 'Vector'.
        // Snippet should point to "new Vector(...)".
        // The parser expects curly braces instead of parentheses for structs.
        let code = "struct Vector {\n    s32 x\n}\nfn main() {\n    Vector v <- new Vector { y <- 1 }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3012_variant_payload_mismatch() {
        // Expected: E3012
        // Message should indicate that the enum variant payload does not match the expected types.
        let code = "enum OptionalInt {\n    Some(s32)\n    None\n}\nfn main() {\n    OptionalInt opt <- OptionalInt::Some(true)\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3005_missing_return_nested() {
        // Expected: E3005
        // Message should indicate that a function path is missing a return.
        // Tests control flow analysis for returns inside loops/branches.
        let code = "fn get_val() -> s32 {\n    if (true) {\n        -> 1\n    }\n    // missing return here\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3003_type_mismatch_return() {
        // Expected: E3003
        // Message should indicate a type mismatch in return statement.
        let code = "fn get_val() -> s32 {\n    -> true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_all_loop_types_success() {
        // This test verifies that all loop types (while, for, infinite)
        // pass through the semantic analyzer without underflow or unwrap panics.
        let code = "fn main() {\n    // while loop\n    s32 count1 <- 0\n    loop (count1 < 10) {\n        count1 <- count1 + 1\n    }\n    // for loop\n    s32 sum <- 0\n    loop (s32 count2 <- 0; count2 < 100; count2 <- count2 + 1) {\n        sum <- sum + count2\n    }\n    // infinite loop\n    s32 count3 <- 0\n    loop {\n        count3 <- count3 + 1\n    }\n}\n";
        smoke_run_for_code(code);
    }
}