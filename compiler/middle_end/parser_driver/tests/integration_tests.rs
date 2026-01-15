use parser_driver::generate_untyped_ast;
use shared::program_information::{ProgramInformation, Project};
use source::SourceMap;
use io::WasomeLoader;
use std::ops::Deref;
use std::path::PathBuf;

/// Helper to get the path to the test programs directory
fn get_test_programs_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test_programs")
}

#[test]
fn test_simple_program() {
    let root = get_test_programs_dir();
    let main_file = PathBuf::from("main.waso");

    let prog_info = ProgramInformation::new(
        "simple".to_string(),
        root.clone(),
        vec![Project::new("simple".to_string(), PathBuf::from("simple"))],
        "simple".to_string(),
        main_file,
    );

    let mut sm = SourceMap::<WasomeLoader>::new(root);

    let ast = generate_untyped_ast(&prog_info, &mut sm).expect("Failed to generate AST");

    let root_node = ast.deref().subdirectory_by_name("simple").unwrap();
    assert_eq!(root_node.name(), "simple");
    
    // Check main file
    let main_file_node = root_node.files().iter().find(|f| f.name() == "main").expect("main not found");
    assert!(main_file_node.functions().iter().any(|f| f.declaration().name() == "main"));
}

#[test]
fn test_multi_module_program() {
    let root = get_test_programs_dir();
    let main_file = PathBuf::from("main.waso");

    let prog_info = ProgramInformation::new(
        "multi_module".to_string(),
        root.clone(),
        vec![Project::new("multi_module".to_string(), PathBuf::from("multi_module"))],
        "multi_module".to_string(),
        main_file,
    );

    let mut sm = SourceMap::<WasomeLoader>::new(root);

    let ast = generate_untyped_ast(&prog_info, &mut sm).expect("Failed to generate AST");

    let root_node = ast.deref().subdirectory_by_name("multi_module").unwrap();
    
    // Check main file
    let main_file_node = root_node.file_by_name("main").expect("main not found");
    assert_eq!(main_file_node.imports().len(), 1);

    // Check utils directory
    let utils_dir = root_node.subdirectory_by_name("utils").expect("utils dir not found");
    
    // Check math.waso and string.waso
    let math_file = utils_dir.file_by_name("math").expect("math not found");
    assert!(math_file.functions().iter().any(|f| f.declaration().name() == "add"));

    let string_file = utils_dir.file_by_name("string").expect("string not found");
    assert!(string_file.functions().iter().any(|f| f.declaration().name() == "concat"));
}

#[test]
fn test_multi_project_program() {
    let root = get_test_programs_dir();
    let main_file = PathBuf::from("main.waso");

    let prog_info = ProgramInformation::new(
        "multi_project".to_string(),
        root.clone(),
        vec![
            Project::new("app".to_string(), PathBuf::from("multi_project/app")),
            Project::new("lib".to_string(), PathBuf::from("multi_project/lib"))
        ],
        "app".to_string(),
        main_file,
    );

    let mut sm = SourceMap::<WasomeLoader>::new(root);

    let ast = generate_untyped_ast(&prog_info, &mut sm).expect("Failed to generate AST");

    let root_node = ast.deref();
    
    // Check app project
    let app_dir = root_node.subdirectory_by_name("app").expect("app dir not found");
    let main_file_node = app_dir.file_by_name("main").expect("main not found");
    assert_eq!(main_file_node.imports().len(), 1); // imports lib/math

    // Check lib project
    let lib_dir = root_node.subdirectory_by_name("lib").expect("lib dir not found");
    let math_dir = lib_dir.subdirectory_by_name("math").expect("math dir not found");
    
    let ops_file = math_dir.file_by_name("ops").expect("ops not found");
    assert!(ops_file.functions().iter().any(|f| f.declaration().name() == "op"));
}

#[test]
fn test_circular_imports() {
    let root = get_test_programs_dir();
    let main_file = PathBuf::from("a/a.waso");

    let prog_info = ProgramInformation::new(
        "circular".to_string(),
        root.clone(),
        vec![Project::new("circular".to_string(), PathBuf::from("circular"))],
        "circular".to_string(),
        main_file,
    );

    let mut sm = SourceMap::<WasomeLoader>::new(root);

    let ast = generate_untyped_ast(&prog_info, &mut sm).expect("Failed to generate AST");

    let root_node = ast.deref().subdirectory_by_name("circular").unwrap();
    
    // circular/a/a.waso
    let a_dir = root_node.subdirectory_by_name("a").expect("a dir not found");
    let a_file = a_dir.file_by_name("a").expect("a not found");
    
    // circular/b/b.waso
    let b_dir = root_node.subdirectory_by_name("b").expect("b dir not found");
    let b_file = b_dir.file_by_name("b").expect("b not found");

    assert!(a_file.functions().iter().any(|f| f.declaration().name() == "func_a"));
    assert!(b_file.functions().iter().any(|f| f.declaration().name() == "func_b"));
}
