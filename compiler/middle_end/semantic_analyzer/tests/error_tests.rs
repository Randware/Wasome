#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use tempfile::TempDir;

    use driver::parser_driver::generate_untyped_ast;
    use driver::program_information::{
        ConcreteBinaryProgramInformation, ConcreteLoadBinaryProgramInformation,
        ConcreteLoadInformation, Project,
    };
    use driver::source_collector::collect_program;
    use io::WasomeLoader;
    use semantic_analyzer::analyze;
    use source::SourceMap;

    /// Creates a temporary project structure for compiler testing
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

        let prog_info = ConcreteLoadBinaryProgramInformation::new(
            ConcreteLoadInformation::new(
                "error_project".to_string(),
                root.clone(),
                vec![Project::new("app".to_string(), PathBuf::from("app"))],
            ),
            ConcreteBinaryProgramInformation::new("app".to_string(), main_file),
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
                    "Parsing failed! The parser encountered a syntax error. Code boundary violation:\n{}",
                    code
                );
            }
        };

        match analyze(untyped_ast) {
            Ok(_) => panic!(
                "Expected a semantic error, but analysis cleanly succeeded! Code:\n{}",
                code
            ),
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

        let prog_info = ConcreteLoadBinaryProgramInformation::new(
            ConcreteLoadInformation::new(
                "error_project".to_string(),
                root.clone(),
                vec![Project::new("app".to_string(), PathBuf::from("app"))],
            ),
            ConcreteBinaryProgramInformation::new("app".to_string(), main_file),
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
        let code = "fn main() {}\n";
        smoke_run_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3001_unknown_type() {
        // Expected: E3001 - Unknown type
        let code = "fn main() {\n    UnknownType x <- 5\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3002_unknown_symbol() {
        // Expected: E3002 - Cannot find symbol
        let code = "fn main() {\n    s32 a <- 10\n    s32 b <- a + undefined_variable\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3003_type_mismatch() {
        // Expected: E3003 - Type mismatch (expected s32, found bool)
        let code = "fn main() {\n    s32 x <- true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3003_type_mismatch_return() {
        // Expected: E3003 - Yielded type does not conform to declared return type signature
        let code = "fn get_val() -> s32 {\n    -> true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3004_already_declared_parameter() {
        // Expected: E3004 - Parameter already defined
        let code = "fn main(s32 x, s32 x) {}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3005_missing_return() {
        // Expected: E3005 - Control path must return a value
        let code = "fn calc() -> s32 {\n    s32 x <- 1\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3006_argument_mismatch() {
        // Expected: E3006 - Argument count mismatch
        let code = "fn add(s32 a, s32 b) -> s32 {\n    -> a + b\n}\n\nfn main() {\n    add(5)\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3007_condition_not_boolean() {
        // Expected: E3007 - Condition must evaluate to a boolean
        let code = "fn main() {\n    if (5) {\n        s32 x <- 1\n    }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3008_break_outside_loop() {
        // Expected: E3008 - Break used outside of a loop context
        let code = "fn main() {\n    if (true) {\n        break\n    }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3009_invalid_usage_of_symbol() {
        // Expected: E3009 - Invalid usage of a symbol
        // A struct name cannot be used as a standalone value in an expression
        // or an assignment.
        let code = "struct Vector {\n    s32 x\n}\nfn main() {\n    s32 invalid <- Vector\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3010_generic_argument_count_mismatch() {
        // Expected: E3010
        // Message should state the wrong number of type parameters.
        // Correct Wasome syntax with square brackets [] for generics!
        let code = "struct Pair[T, U] {\n    T first\n}\nfn main() {\n    Pair[s32] p <- new Pair[s32] { first <- 1 }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3011_missing_or_invalid_struct_field() {
        // Expected: E3011 - Missing or incorrectly named field during struct initialization
        // Initializing a structure with a missing field or invalid field configuration.
        let code = "struct Vector {\n    s32 x\n}\nfn main() {\n    Vector v <- new Vector { y <- 1 }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3012_variant_payload_mismatch() {
        // Expected: E3012 - Enum variant payload type mismatch
        // The type passed to the enum variant constructor does not match its declared payload definition.
        let code = "enum OptionalInt {\n    Some(s32)\n    None\n}\nfn main() {\n    OptionalInt opt <- OptionalInt::Some(true)\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3013_private_symbol_access() {
        // Expected: E3013 - Access to a private symbol from outside its module
        // Defining a local symbol 'math' to trigger the namespaced privacy resolution pass without file collection dependencies.
        let code = "fn math() {}\nfn main() {\n    math.Test t <- new math.Test { num <- 10 }\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3014_method_on_non_struct() {
        // Expected: E3014 - Cannot call methods on primitive types
        let code = "fn main() {\n    s32 x <- 5\n    x.do_something()\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3015_symbol_kind_mismatch() {
        // Expected: E3015 - Symbol kind mismatch (Function used as a Type descriptor)
        let code = "fn NotAType() {}\nfn main(NotAType obj) {}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3016_unknown_field() {
        // Expected: E3016 - Unknown field access on structural type
        let code = "struct Vector {\n    s32 x\n}\nfn main(Vector v) {\n    s32 y <- v.y\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3017_field_access_on_non_struct() {
        // Expected: E3017 - Cannot access fields on non-struct primitive instances
        let code = "fn main() {\n    s32 number <- 10\n    s32 invalid <- number.field\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3018_void_returns_value() {
        // Expected: E3018 - Void functions cannot yield evaluations
        let code = "fn do_nothing() {\n    -> 5\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3019_missing_return_value() {
        // Expected: E3019 - Missing return value expression
        let code = "fn get_five() -> s32 {\n    -> \n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3020_primitive_with_type_parameters() {
        // Expected: E3020 - Primitives cannot accept generic bindings
        // S32 capitalized allows the code to pass the parser as a valid Identifier token,
        // which then securely routes into the semantic analyzer to trigger the E3020 constraint check.
        let code = "fn main() {\n    S32[bool] invalid_primitive <- 5\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3021_unsupported_binary_operation() {
        // Expected: E3021 - Invalid operator application across disparate types
        let code = "fn main() {\n    s32 x <- 5 + true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3022_unsupported_unary_operation() {
        // Expected: E3022 - Negation operator applied to logical bool type
        let code = "fn main() {\n    bool x <- -true\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3023_invalid_literal_format() {
        // Expected: E3023 - Integer literal magnitude out of bounds for s32 limits
        let code = "fn main() {\n    s32 x <- 999999999999999999\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3024_unknown_enum_variant() {
        // Expected: E3024 - Variant not found inside target Enum scope
        let code = "enum Color {\n    Red\n}\nfn main() {\n    Color x <- Color::Blue\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3025_local_variable_shadowing() {
        // Expected: E3025 - Local variable duplicate definition within identical scope block
        let code = "fn main() {\n    s32 a <- 1\n    s32 a <- 2\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3026_void_used_as_value() {
        // Expected: E3026 - Void result captured inside assignment expression
        let code = "fn action() {}\nfn main() {\n    s32 result <- action()\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3027_struct_initialization_mismatch() {
        // Expected: E3027 - Structural initialization parameter field layout mismatch
        // Empty parameters fail count validation, triggering E3027 instead of individual field lookups
        let code = "struct Vector {\n    s32 x\n}\nfn main() {\n    Vector v <- new Vector {}\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3028_duplicate_type_parameter() {
        // Expected: E3028 - Type parameter is declared more than once
        // Correct Wasome syntax utilizing square brackets [] for generic type parameters.
        let code = "struct Pair[T, T] {\n    T first\n}\nfn main() {}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3029_invalid_drop_signature() {
        // Expected: E3029 - Invalid drop method signature
        // Declaring drop with an invalid parameter layout triggers E3029 via standard function analysis.
        let code = "fn drop(s32 extra_arg) {}\nfn main() {}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_e3030_private_field_access() {
        // Expected: E3030 - Cannot access private field of struct
        // Versucht von außerhalb der Struct-Definition (in fn main)
        // auf das private Feld 'secret' zuzugreifen.
        let code = "struct Account {\n    s32 secret\n}\n\nfn main(Account acc) {\n    s32 stolen <- acc.secret\n}\n";
        print_diagnostic_for_code(code);
    }

    #[test]
    #[ignore]
    fn test_all_loop_types_success() {
        // Success Regression: Assures standard loops run flawlessly without underflows
        let code = "fn main() {\n    s32 count1 <- 0\n    loop (count1 < 10) {\n        count1 <- count1 + 1\n    }\n    s32 sum <- 0\n    loop (s32 count2 <- 0; count2 < 100; count2 <- count2 + 1) {\n        sum <- sum + count2\n    }\n    s32 count3 <- 0\n    loop {\n        count3 <- count3 + 1\n        break\n    }\n}\n";
        smoke_run_for_code(code);
    }

    const STRUCT_FIELD_DATA: &str =
        include_str!("private_struct_field_outside_struct/lib/data.waso");
    const STRUCT_FIELD_MAIN: &str =
        include_str!("private_struct_field_outside_struct/app/main.waso");

    #[test]
    #[ignore = "Manual visual inspection of the struct field visibility check"]
    fn test_struct_field_visibility_check() {
        let dir = setup_temp_project(&[
            ("struct_field/lib/data.waso", STRUCT_FIELD_DATA),
            ("struct_field/app/main.waso", STRUCT_FIELD_MAIN),
        ]);
        let root = dir.path().to_path_buf().join("struct_field");
        let main_file = PathBuf::from("main.waso");

        let prog_info = ConcreteLoadBinaryProgramInformation::new(
            ConcreteLoadInformation::new(
                "struct_field".to_string(),
                root.clone(),
                vec![
                    Project::new("app".to_string(), PathBuf::from("app")),
                    Project::new("lib".to_string(), PathBuf::from("lib")),
                ],
            ),
            ConcreteBinaryProgramInformation::new("app".to_string(), main_file),
        )
        .unwrap();

        let mut sm = SourceMap::<WasomeLoader>::with_default(root);
        let ast = generate_untyped_ast(collect_program(&prog_info, &mut sm).unwrap(), &mut sm)
            .expect("Failed to generate AST");
        let err = analyze(ast).unwrap_err();
        err.print_snippets(&sm).unwrap()
    }
}
