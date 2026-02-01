use std::fs;
use std::path::PathBuf;
use ast::composite::{Enum, EnumVariant, Struct, StructField};
use ast::data_type::DataType;
use ast::symbol::{EnumSymbol, EnumVariantSymbol, FunctionSymbol, ModuleUsageNameSymbol, StructFieldSymbol, StructSymbol, SymbolWithTypeParameter, VariableSymbol};
use ast::expression::{Expression, FunctionCall, Literal, NewStruct, StructFieldAccess, NewEnum};
use ast::statement::{CodeBlock, Statement, VariableDeclaration};
use std::rc::Rc;
use tempfile::TempDir;
use ast::{ASTNode, SemanticEq, TypedAST, AST};
use ast::directory::Directory;
use ast::file::File;
use ast::top_level::{Function, Import, ImportRoot};
use ast::type_parameter::TypedTypeParameter;
use ast::visibility::Visibility;
use driver::parser_driver::generate_untyped_ast;
use driver::program_information::{ProgramInformation, Project};
use io::WasomeLoader;
use semantic_analyzer::analyze;
use shared::code_file::CodeFile;
use shared::code_reference::{CodeArea, CodeLocation};
use source::SourceMap;

// --- Test Program Contents ---
const MULTI_PROJECT_APP_MAIN: &str = include_str!("test_programs/multi-project/min/app/main.waso");
const MULTI_PROJECT_LIB_OPS: &str = include_str!("test_programs/multi-project/min/lib/math/ops.waso");
const MULTI_PROJECT_GENERICS_LIB_DATA: &str = include_str!("test_programs/multi-project/generics/lib/data.waso");
const MULTI_PROJECT_GENERICS_APP_MAIN: &str = include_str!("test_programs/multi-project/generics/app/main.waso");

// --- Helper Functions ---

fn dummy_code_area() -> CodeArea {
    CodeArea::new(
        CodeLocation::new(0, 0),
        CodeLocation::new(0, 0),
        CodeFile::new(PathBuf::from("test")),
    )
    .unwrap()
}

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

// --- Tests ---

#[test]
fn test_multi_project_program() {
    let dir = setup_temp_project(&[
        ("multi_project/app/main.waso", MULTI_PROJECT_APP_MAIN),
        ("multi_project/lib/math/ops.waso", MULTI_PROJECT_LIB_OPS),
    ]);
    let root = dir.path().to_path_buf().join("multi_project");
    let main_file = PathBuf::from("main.waso");

    let prog_info = ProgramInformation::new(
        "multi_project".to_string(),
        root.clone(),
        vec![
            Project::new("app".to_string(), PathBuf::from("app")),
            Project::new("lib".to_string(), PathBuf::from("lib")),
        ],
        "app".to_string(),
        main_file,
    )
    .unwrap();

    let mut sm = SourceMap::<WasomeLoader>::new(root);

    let ast = generate_untyped_ast(&prog_info, &mut sm).expect("Failed to generate AST");

    let typed_ast = analyze(ast).unwrap();

    let op_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
        "op".to_string(),
        None,
        vec![],
        vec![],
    ));

    let main_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
        "main".to_string(),
        None,
        vec![],
        vec![],
    ));

    let m_symbol = Rc::new(ModuleUsageNameSymbol::new("m".to_string()));

    let op_function = ASTNode::new(
        Function::new(
            op_symbol.clone(),
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![])),
                dummy_code_area(),
            ),
            Visibility::Public,
        ),
        dummy_code_area(),
    );

    let ops_file = ASTNode::new(
        File::new(
            "ops".to_string(),
            vec![],
            vec![op_function],
            vec![],
            vec![],
        ),
        PathBuf::from("lib/math/ops.waso"),
    );

    let math_dir = ASTNode::new(
        Directory::new(
            "math".to_string(),
            vec![],
            vec![ops_file],
        ),
        PathBuf::from("lib/math"),
    );

    let lib_dir = ASTNode::new(
        Directory::new(
            "lib".to_string(),
            vec![math_dir],
            vec![],
        ),
        PathBuf::from("lib"),
    );

    let import_stmt = ASTNode::new(
        Import::new(
            ImportRoot::Root,
            vec!["lib".to_string(), "math".to_string()],
            m_symbol.clone(),
        ),
        dummy_code_area(),
    );

    let main_body_stmt = ASTNode::new(
        Statement::VoidFunctionCall(FunctionCall::<TypedAST>::new(op_symbol.clone(), vec![]).unwrap()),
        dummy_code_area(),
    );

    let main_function = ASTNode::new(
        Function::new(
            main_symbol.clone(),
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![main_body_stmt])),
                dummy_code_area(),
            ),
            Visibility::Private,
        ),
        dummy_code_area(),
    );

    let main_file = ASTNode::new(
        File::new(
            "main".to_string(),
            vec![import_stmt],
            vec![main_function],
            vec![],
            vec![],
        ),
        PathBuf::from("app/main.waso"),
    );

    let app_dir = ASTNode::new(
        Directory::new("app".to_string(), vec![], vec![main_file]),
        PathBuf::from("app"),
    );

    let root_dir = ASTNode::new(
        Directory::new(
            "multi_project".to_string(),
            vec![app_dir, lib_dir],
            vec![],
        ),
        PathBuf::from(""),
    );

    let expected_ast = AST::new(root_dir).unwrap();
    
    // Check equality both ways to be sure
    assert!(typed_ast.semantic_eq(&expected_ast), "Actual AST does not match expected AST");
}

#[test]
fn test_multi_project_generics() {
    let dir = setup_temp_project(&[
        ("multi_project_generics/lib/data.waso", MULTI_PROJECT_GENERICS_LIB_DATA),
        ("multi_project_generics/app/main.waso", MULTI_PROJECT_GENERICS_APP_MAIN),
    ]);
    let root = dir.path().to_path_buf().join("multi_project_generics");
    let main_file = PathBuf::from("main.waso");

    let prog_info = ProgramInformation::new(
        "multi_project_generics".to_string(),
        root.clone(),
        vec![
            Project::new("app".to_string(), PathBuf::from("app")),
            Project::new("lib".to_string(), PathBuf::from("lib")),
        ],
        "app".to_string(),
        main_file,
    )
    .unwrap();

    let mut sm = SourceMap::<WasomeLoader>::new(root);
    let ast = generate_untyped_ast(&prog_info, &mut sm).expect("Failed to generate AST");
    let typed_ast = analyze(ast).unwrap();

    // --- Symbols ---
    
    // DataType: s32
    let s32_type = DataType::S32;

    // StructSymbol: LinkedList<s32>
    let linked_list_s32_param = TypedTypeParameter::new("T".to_string(), s32_type.clone());
    let linked_list_s32_symbol = Rc::new(StructSymbol::new(
        "LinkedList".to_string(),
        vec![linked_list_s32_param],
    ));
    let linked_list_s32_type = DataType::Struct(linked_list_s32_symbol.clone());

    // EnumSymbol: Option<LinkedList<s32>>
    let option_ll_param = TypedTypeParameter::new("T".to_string(), linked_list_s32_type.clone());
    let option_ll_symbol = Rc::new(EnumSymbol::new(
        "Option".to_string(),
        vec![option_ll_param],
    ));
    let option_ll_type = DataType::Enum(option_ll_symbol.clone());

    // Field Symbols
    let val_field_symbol = Rc::new(StructFieldSymbol::new("value".to_string(), s32_type.clone()));
    let next_field_symbol = Rc::new(StructFieldSymbol::new("next".to_string(), option_ll_type.clone()));

    // Enum Variant Symbols
    let some_variant_symbol = Rc::new(EnumVariantSymbol::new(
        "Some".to_string(),
        vec![linked_list_s32_type.clone()],
    ));
    let none_variant_symbol = Rc::new(EnumVariantSymbol::new(
        "None".to_string(),
        vec![],
    ));

    // Main Function Symbol
    let main_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
        "main".to_string(),
        None,
        vec![],
        vec![],
    ));

    // Variable Symbols (in main)
    let tail_var_symbol = Rc::new(VariableSymbol::new(
        "tail".to_string(),
        linked_list_s32_type.clone(),
    ));
    let head_var_symbol = Rc::new(VariableSymbol::new(
        "head".to_string(),
        linked_list_s32_type.clone(),
    ));

    // Import Symbol
    let d_usage_symbol = Rc::new(ModuleUsageNameSymbol::new("d".to_string()));

    // --- Construct App File (usage) ---
    
    // tail definition: new d.LinkedList<s32> { value <- 10, next <- new d.Option<...>::None() }
    let none_expr = ASTNode::new(
        Expression::NewEnum(Box::new(NewEnum::<TypedAST>::new(
            option_ll_symbol.clone(),
            none_variant_symbol.clone(),
            vec![],
        ).unwrap())),
        dummy_code_area(),
    );

    let tail_new_struct = ASTNode::new(
        Expression::NewStruct(Box::new(NewStruct::new(
            linked_list_s32_symbol.clone(),
            vec![
                (
                    ASTNode::new(val_field_symbol.clone(), dummy_code_area()),
                    ASTNode::new(Expression::Literal(Literal::S32(10)), dummy_code_area()),
                ),
                (
                    ASTNode::new(next_field_symbol.clone(), dummy_code_area()),
                    none_expr,
                ),
            ],
        ))),
        dummy_code_area(),
    );

    let tail_decl = ASTNode::new(
        Statement::VariableDeclaration(
            VariableDeclaration::<TypedAST>::new(tail_var_symbol.clone(), tail_new_struct).unwrap(),
        ),
        dummy_code_area(),
    );

    // head definition: new d.LinkedList<s32> { value <- 20, next <- new d.Option<...>::Some(tail) }
    let some_expr = ASTNode::new(
        Expression::NewEnum(Box::new(NewEnum::<TypedAST>::new(
            option_ll_symbol.clone(),
            some_variant_symbol.clone(),
            vec![
                ASTNode::new(Expression::Variable(tail_var_symbol.clone()), dummy_code_area())
            ],
        ).unwrap())),
        dummy_code_area(),
    );

    let head_new_struct = ASTNode::new(
        Expression::NewStruct(Box::new(NewStruct::new(
            linked_list_s32_symbol.clone(),
            vec![
                (
                    ASTNode::new(val_field_symbol.clone(), dummy_code_area()),
                    ASTNode::new(Expression::Literal(Literal::S32(20)), dummy_code_area()),
                ),
                (
                    ASTNode::new(next_field_symbol.clone(), dummy_code_area()),
                    some_expr,
                ),
            ],
        ))),
        dummy_code_area(),
    );

    let head_decl = ASTNode::new(
        Statement::VariableDeclaration(
            VariableDeclaration::<TypedAST>::new(head_var_symbol.clone(), head_new_struct).unwrap(),
        ),
        dummy_code_area(),
    );

    let main_func = ASTNode::new(
        Function::new(
            main_symbol.clone(),
            ASTNode::new(
                Statement::Codeblock(CodeBlock::new(vec![tail_decl, head_decl])),
                dummy_code_area(),
            ),
            Visibility::Private,
        ),
        dummy_code_area(),
    );

    let import_stmt = ASTNode::new(
        Import::new(
            ImportRoot::Root,
            vec!["lib".to_string()],
            d_usage_symbol.clone(),
        ),
        dummy_code_area(),
    );

    let app_file = ASTNode::new(
        File::new(
            "main".to_string(),
            vec![import_stmt],
            vec![main_func],
            vec![],
            vec![],
        ),
        PathBuf::from("app/main.waso"),
    );

    // --- Construct Lib File (definitions) ---
    
    let enum_def = ASTNode::new(
        Enum::new(
            option_ll_symbol.clone(),
            vec![
                ASTNode::new(EnumVariant::new(some_variant_symbol.clone()), dummy_code_area()),
                ASTNode::new(EnumVariant::new(none_variant_symbol.clone()), dummy_code_area()),
            ],
            Visibility::Public,
        ),
        dummy_code_area(),
    );

    let struct_def = ASTNode::new(
        Struct::new(
            linked_list_s32_symbol.clone(),
            vec![], // functions
            vec![
                ASTNode::new(StructField::new(val_field_symbol.clone(), Visibility::Private), dummy_code_area()),
                ASTNode::new(StructField::new(next_field_symbol.clone(), Visibility::Private), dummy_code_area()),
            ],
            Visibility::Public,
        ),
        dummy_code_area(),
    );

    let lib_file = ASTNode::new(
        File::new(
            "data".to_string(),
            vec![],
            vec![],
            vec![enum_def],
            vec![struct_def],
        ),
        PathBuf::from("lib/data.waso"),
    );

    // --- Construct Directories ---

    let app_dir = ASTNode::new(
        Directory::new("app".to_string(), vec![], vec![app_file]),
        PathBuf::from("app"),
    );

    let lib_dir = ASTNode::new(
        Directory::new("lib".to_string(), vec![], vec![lib_file]),
        PathBuf::from("lib"),
    );

    let root_dir = ASTNode::new(
        Directory::new(
            "multi_project_generics".to_string(),
            vec![app_dir, lib_dir],
            vec![],
        ),
        PathBuf::from(""),
    );

    let expected_ast = AST::new(root_dir).unwrap();

    assert!(typed_ast.semantic_eq(&expected_ast), "Actual AST does not match expected AST");
}

