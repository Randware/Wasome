use ast::composite::{Enum, EnumVariant, Struct, StructField};
use ast::data_type::UntypedDataType;
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, NewEnum, NewStruct, StructFieldAccess,
    Typecast, UnaryOp, UnaryOpType,
};
use ast::statement::{
    CodeBlock, Conditional, ControlStructure, IfEnumVariant, Loop, LoopType, Return, Statement,
    StructFieldAssignment, VariableAssignment, VariableDeclaration,
};
use ast::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, ModuleUsageNameSymbol, StructFieldSymbol,
    StructSymbol, VariableSymbol,
};
use ast::top_level::{Function, Import, ImportRoot};
use ast::visibility::Visibility;
use ast::{ASTNode, SemanticEq, UntypedAST};
use parser::{FileInformation, parse};
use shared::code_file::CodeFile;
use shared::code_reference::{CodeArea, CodeLocation};
use source::SourceMap;
use source::types::FileID;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;
use tempfile::TempDir;

fn setup_file(name: &str, content: &str) -> (TempDir, PathBuf) {
    let dir = tempfile::tempdir().unwrap();
    let file_path = dir.path().join(name);
    let mut file = File::create(&file_path).unwrap();
    write!(file, "{}", content).unwrap();
    (dir, file_path)
}

// Helper functions for AST construction
fn dummy_codearea() -> CodeArea {
    CodeArea::new(
        CodeLocation::new(0, 0),
        CodeLocation::new(0, 0),
        CodeFile::new(PathBuf::from("")),
    )
    .unwrap()
}

fn wrap<T: Debug>(inner: T) -> ASTNode<T> {
    ASTNode::new(inner, dummy_codearea())
}

// --- INTEGRATION TESTS ---

const FIBONACCI: &'static str = include_str!("test_programs/single_file/fibonacci.waso");
const MAX: &'static str = include_str!("test_programs/single_file/max.waso");
const SUM_N: &'static str = include_str!("test_programs/single_file/sum_n.waso");
const IS_EVEN: &'static str = include_str!("test_programs/single_file/is_even.waso");
const MODULAR_ADD: &'static str =
    include_str!("test_programs/single_project/modular_arithmetic/modular_add.waso");
const MODULAR_MUL: &'static str =
    include_str!("test_programs/single_project/modular_arithmetic/modular_mul.waso");
const MISC_FEATURES: &'static str = include_str!("test_programs/single_file/misc.waso");
const UNARY_CAST: &'static str = include_str!("test_programs/single_file/unary_cast.waso");
const IF_TEST: &'static str = include_str!("test_programs/single_file/if.waso");
const LOOP_TEST: &'static str = include_str!("test_programs/single_file/loop.waso");
const OPERATOR_TEST: &'static str = include_str!("test_programs/single_file/operator.waso");
const MISSING_STATEMENT_SEPARATOR: &'static str =
    include_str!("test_programs/single_file/missing_statement_separator.waso");
const STRUCT_TEST: &'static str = include_str!("test_programs/single_file/struct.waso");
const ENUM_TEST: &'static str = include_str!("test_programs/single_file/enum.waso");
const EXHAUSTIVE_DEFS: &'static str =
    include_str!("test_programs/single_project/exhaustive/defs.waso");
const EXHAUSTIVE_MAIN: &'static str =
    include_str!("test_programs/single_project/exhaustive/main.waso");
const GENERICS_TEST: &'static str = include_str!("test_programs/single_file/generics.waso");
const GENERICS_NESTED_TEST: &'static str =
    include_str!("test_programs/single_file/generic_nested.waso");
const GENERICS_METHODS_TEST: &'static str =
    include_str!("test_programs/single_file/generic_methods.waso");
const GENERICS_MULTI_FILE_DEFS: &'static str =
    include_str!("test_programs/single_project/generic_multi_file/defs.waso");
const GENERICS_MULTI_FILE_MAIN: &'static str =
    include_str!("test_programs/single_project/generic_multi_file/main.waso");

#[test]
fn test_parse_generics() {
    let (sm, id) = setup_source_map(GENERICS_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    use ast::symbol::UntypedTypeParameterSymbol;
    use ast::type_parameter::UntypedTypeParameter;

    let t_tp = UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new("T".to_string())));
    let u_tp = UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new("U".to_string())));

    // struct Box<T> { T value }
    let box_symbol = Rc::new(StructSymbol::new("Box".to_string(), vec![t_tp.clone()]));
    let box_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "value".to_string(),
            UntypedDataType::new("T".to_string(), Vec::new()),
        )),
        Visibility::Private,
    ));
    let box_struct = wrap(Struct::new(
        box_symbol.clone(),
        Vec::new(),
        vec![box_field],
        Visibility::Private,
    ));

    // enum Option<T> { Some(T), None }
    let option_symbol = Rc::new(EnumSymbol::new("Option".to_string(), vec![t_tp.clone()]));
    let some_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "Some".to_string(),
        vec![UntypedDataType::new("T".to_string(), Vec::new())],
    ))));
    let none_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "None".to_string(),
        vec![],
    ))));
    let option_enum = wrap(Enum::new(
        option_symbol.clone(),
        vec![some_variant, none_variant],
        Visibility::Private,
    ));

    // struct Pair<T, U> { T first, U second }
    let pair_symbol = Rc::new(StructSymbol::new(
        "Pair".to_string(),
        vec![t_tp.clone(), u_tp.clone()],
    ));
    let first_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "first".to_string(),
            UntypedDataType::new("T".to_string(), Vec::new()),
        )),
        Visibility::Private,
    ));
    let second_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "second".to_string(),
            UntypedDataType::new("U".to_string(), Vec::new()),
        )),
        Visibility::Private,
    ));
    let pair_struct = wrap(Struct::new(
        pair_symbol.clone(),
        Vec::new(),
        vec![first_field, second_field],
        Visibility::Private,
    ));

    // pub fn identity<T>(T val) -> T { -> val }
    let val_param = Rc::new(VariableSymbol::new(
        "val".to_string(),
        UntypedDataType::new("T".to_string(), Vec::new()),
    ));
    let identity_symbol = Rc::new(FunctionSymbol::new(
        "identity".to_string(),
        Some(UntypedDataType::new("T".to_string(), Vec::new())),
        vec![val_param],
        vec![t_tp.clone()],
    ));
    let identity_func = wrap(Function::new(
        identity_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::Variable(
                "val".to_string(),
            ))))),
        )]))),
        Visibility::Public,
    ));

    // pub fn map<T, U>(Option<T> opt, Box<T> b) -> Option<U>
    let opt_param = Rc::new(VariableSymbol::new(
        "opt".to_string(),
        UntypedDataType::new(
            "Option".to_string(),
            vec![UntypedDataType::new("T".to_string(), Vec::new())],
        ),
    ));
    let b_param = Rc::new(VariableSymbol::new(
        "b".to_string(),
        UntypedDataType::new(
            "Box".to_string(),
            vec![UntypedDataType::new("T".to_string(), Vec::new())],
        ),
    ));
    let map_symbol = Rc::new(FunctionSymbol::new(
        "map".to_string(),
        Some(UntypedDataType::new(
            "Option".to_string(),
            vec![UntypedDataType::new("U".to_string(), Vec::new())],
        )),
        vec![opt_param, b_param],
        vec![t_tp.clone(), u_tp.clone()],
    ));
    // Option<U> result <- Option<U>::None
    let result_symbol = Rc::new(VariableSymbol::new(
        "result".to_string(),
        UntypedDataType::new(
            "Option".to_string(),
            vec![UntypedDataType::new("U".to_string(), Vec::new())],
        ),
    ));
    let result_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        result_symbol,
        wrap(Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
            (
                "Option".to_string(),
                vec![UntypedDataType::new("U".to_string(), Vec::new())],
            ),
            "None".to_string(),
            vec![],
        )))),
    )));
    let map_func = wrap(Function::new(
        map_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![
            result_decl,
            wrap(Statement::Return(Return::new(Some(wrap(
                Expression::Variable("result".to_string()),
            ))))),
        ]))),
        Visibility::Public,
    ));

    // fn main() { ... }
    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        None,
        vec![],
        Vec::new(),
    ));
    // Box<s32> b <- new Box<s32> { value <- 10 }
    let b_var_symbol = Rc::new(VariableSymbol::new(
        "b".to_string(),
        UntypedDataType::new(
            "Box".to_string(),
            vec![UntypedDataType::new("s32".to_string(), Vec::new())],
        ),
    ));
    let b_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        b_var_symbol,
        wrap(Expression::NewStruct(Box::new(
            NewStruct::<UntypedAST>::new(
                (
                    "Box".to_string(),
                    vec![UntypedDataType::new("s32".to_string(), Vec::new())],
                ),
                vec![(
                    wrap("value".to_string()),
                    wrap(Expression::Literal("10".to_string())),
                )],
            ),
        ))),
    )));
    // Option<f64> o <- Option<f64>::Some(5.5)
    let o_var_symbol = Rc::new(VariableSymbol::new(
        "o".to_string(),
        UntypedDataType::new(
            "Option".to_string(),
            vec![UntypedDataType::new("f64".to_string(), Vec::new())],
        ),
    ));
    let o_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        o_var_symbol,
        wrap(Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
            (
                "Option".to_string(),
                vec![UntypedDataType::new("f64".to_string(), Vec::new())],
            ),
            "Some".to_string(),
            vec![wrap(Expression::Literal("5.5".to_string()))],
        )))),
    )));
    // Pair<s32, bool> p <- new Pair<s32, bool> { first <- 1, second <- true }
    let p_var_symbol = Rc::new(VariableSymbol::new(
        "p".to_string(),
        UntypedDataType::new(
            "Pair".to_string(),
            vec![
                UntypedDataType::new("s32".to_string(), Vec::new()),
                UntypedDataType::new("bool".to_string(), Vec::new()),
            ],
        ),
    ));
    let p_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        p_var_symbol,
        wrap(Expression::NewStruct(Box::new(
            NewStruct::<UntypedAST>::new(
                (
                    "Pair".to_string(),
                    vec![
                        UntypedDataType::new("s32".to_string(), Vec::new()),
                        UntypedDataType::new("bool".to_string(), Vec::new()),
                    ],
                ),
                vec![
                    (
                        wrap("first".to_string()),
                        wrap(Expression::Literal("1".to_string())),
                    ),
                    (
                        wrap("second".to_string()),
                        wrap(Expression::Literal("true".to_string())),
                    ),
                ],
            ),
        ))),
    )));
    // s32 id <- identity<s32>(b.value)
    let id_var_symbol = Rc::new(VariableSymbol::new(
        "id".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let id_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        id_var_symbol,
        wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            (
                "identity".to_string(),
                vec![UntypedDataType::new("s32".to_string(), Vec::new())],
            ),
            vec![wrap(Expression::StructFieldAccess(Box::new(
                StructFieldAccess::<UntypedAST>::new(
                    wrap(Expression::Variable("b".to_string())),
                    "value".to_string(),
                ),
            )))],
        ))),
    )));

    let main_func = wrap(Function::new(
        main_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![
            b_decl, o_decl, p_decl, id_decl,
        ]))),
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        vec![],
        vec![identity_func, map_func, main_func],
        vec![option_enum],
        vec![box_struct, pair_struct],
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_generics_nested() {
    let (sm, id) = setup_source_map(GENERICS_NESTED_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    use ast::symbol::UntypedTypeParameterSymbol;
    use ast::type_parameter::UntypedTypeParameter;

    let t_tp = UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new("T".to_string())));

    // struct Box<T> { T value }
    let box_symbol = Rc::new(StructSymbol::new("Box".to_string(), vec![t_tp.clone()]));
    let box_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "value".to_string(),
            UntypedDataType::new("T".to_string(), Vec::new()),
        )),
        Visibility::Private,
    ));
    let box_struct = wrap(Struct::new(
        box_symbol.clone(),
        Vec::new(),
        vec![box_field],
        Visibility::Private,
    ));

    // enum Option<T> { Some(T), None }
    let option_symbol = Rc::new(EnumSymbol::new("Option".to_string(), vec![t_tp.clone()]));
    let some_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "Some".to_string(),
        vec![UntypedDataType::new("T".to_string(), Vec::new())],
    ))));
    let none_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "None".to_string(),
        vec![],
    ))));
    let option_enum = wrap(Enum::new(
        option_symbol.clone(),
        vec![some_variant, none_variant],
        Visibility::Private,
    ));

    // Box<Option<s32>> nested <- new Box<Option<s32>> { value <- Option<s32>::Some(10) }
    let nested_type = UntypedDataType::new(
        "Box".to_string(),
        vec![UntypedDataType::new(
            "Option".to_string(),
            vec![UntypedDataType::new("s32".to_string(), Vec::new())],
        )],
    );
    let nested_var_symbol = Rc::new(VariableSymbol::new(
        "nested".to_string(),
        nested_type.clone(),
    ));
    let nested_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        nested_var_symbol,
        wrap(Expression::NewStruct(Box::new(
            NewStruct::<UntypedAST>::new(
                (
                    "Box".to_string(),
                    vec![UntypedDataType::new(
                        "Option".to_string(),
                        vec![UntypedDataType::new("s32".to_string(), Vec::new())],
                    )],
                ),
                vec![(
                    wrap("value".to_string()),
                    wrap(Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
                        (
                            "Option".to_string(),
                            vec![UntypedDataType::new("s32".to_string(), Vec::new())],
                        ),
                        "Some".to_string(),
                        vec![wrap(Expression::Literal("10".to_string()))],
                    )))),
                )],
            ),
        ))),
    )));

    // Option<Box<f64>> opt_box <- Option<Box<f64>>::Some(new Box<f64> { value <- 5.5 })
    let opt_box_type = UntypedDataType::new(
        "Option".to_string(),
        vec![UntypedDataType::new(
            "Box".to_string(),
            vec![UntypedDataType::new("f64".to_string(), Vec::new())],
        )],
    );
    let opt_box_var_symbol = Rc::new(VariableSymbol::new("opt_box".to_string(), opt_box_type));
    let opt_box_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        opt_box_var_symbol,
        wrap(Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
            (
                "Option".to_string(),
                vec![UntypedDataType::new(
                    "Box".to_string(),
                    vec![UntypedDataType::new("f64".to_string(), Vec::new())],
                )],
            ),
            "Some".to_string(),
            vec![wrap(Expression::NewStruct(Box::new(
                NewStruct::<UntypedAST>::new(
                    (
                        "Box".to_string(),
                        vec![UntypedDataType::new("f64".to_string(), Vec::new())],
                    ),
                    vec![(
                        wrap("value".to_string()),
                        wrap(Expression::Literal("5.5".to_string())),
                    )],
                ),
            )))],
        )))),
    )));

    let main_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "main".to_string(),
            None,
            vec![],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![
            nested_decl,
            opt_box_decl,
        ]))),
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        vec![],
        vec![main_func],
        vec![option_enum],
        vec![box_struct],
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_generics_methods() {
    let (sm, id) = setup_source_map(GENERICS_METHODS_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    use ast::symbol::UntypedTypeParameterSymbol;
    use ast::type_parameter::UntypedTypeParameter;

    let t_tp = UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new("T".to_string())));
    let u_tp = UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new("U".to_string())));

    // fn get_item() -> T { -> self.item }
    let get_item_symbol = Rc::new(FunctionSymbol::new(
        "get_item".to_string(),
        Some(UntypedDataType::new("T".to_string(), Vec::new())),
        vec![],
        vec![],
    ));
    let get_item_func = wrap(Function::new(
        get_item_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::StructFieldAccess(
                Box::new(StructFieldAccess::<UntypedAST>::new(
                    wrap(Expression::Variable("self".to_string())),
                    "item".to_string(),
                )),
            ))))),
        )]))),
        Visibility::Private,
    ));

    // fn update<U>(U extra) -> T { ... }
    let extra_param = Rc::new(VariableSymbol::new(
        "extra".to_string(),
        UntypedDataType::new("U".to_string(), Vec::new()),
    ));
    let update_symbol = Rc::new(FunctionSymbol::new(
        "update".to_string(),
        Some(UntypedDataType::new("T".to_string(), Vec::new())),
        vec![extra_param],
        vec![u_tp.clone()],
    ));
    let temp_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "temp".to_string(),
            UntypedDataType::new("U".to_string(), Vec::new()),
        )),
        wrap(Expression::Variable("extra".to_string())),
    )));
    let update_func = wrap(Function::new(
        update_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![
            temp_decl,
            wrap(Statement::Return(Return::new(Some(wrap(
                Expression::StructFieldAccess(Box::new(StructFieldAccess::<UntypedAST>::new(
                    wrap(Expression::Variable("self".to_string())),
                    "item".to_string(),
                ))),
            ))))),
        ]))),
        Visibility::Private,
    ));

    // struct Wrapper<T> { T item, methods... }
    let wrapper_symbol = Rc::new(StructSymbol::new("Wrapper".to_string(), vec![t_tp.clone()]));
    let item_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "item".to_string(),
            UntypedDataType::new("T".to_string(), Vec::new()),
        )),
        Visibility::Private,
    ));
    let wrapper_struct = wrap(Struct::new(
        wrapper_symbol.clone(),
        vec![get_item_func, update_func],
        vec![item_field],
        Visibility::Private,
    ));

    // main function
    let w_var_symbol = Rc::new(VariableSymbol::new(
        "w".to_string(),
        UntypedDataType::new(
            "Wrapper".to_string(),
            vec![UntypedDataType::new("s32".to_string(), Vec::new())],
        ),
    ));
    let w_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        w_var_symbol,
        wrap(Expression::NewStruct(Box::new(
            NewStruct::<UntypedAST>::new(
                (
                    "Wrapper".to_string(),
                    vec![UntypedDataType::new("s32".to_string(), Vec::new())],
                ),
                vec![(
                    wrap("item".to_string()),
                    wrap(Expression::Literal("42".to_string())),
                )],
            ),
        ))),
    )));

    let val_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "val".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            ("w.get_item".to_string(), Vec::new()),
            vec![],
        ))),
    )));

    let old_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "old".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            (
                "w.update".to_string(),
                vec![UntypedDataType::new("bool".to_string(), Vec::new())],
            ),
            vec![wrap(Expression::Literal("true".to_string()))],
        ))),
    )));

    let main_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "main".to_string(),
            None,
            vec![],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![
            w_decl, val_decl, old_decl,
        ]))),
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        vec![],
        vec![main_func],
        vec![],
        vec![wrapper_struct],
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_generics_multi_file_defs() {
    let (sm, id) = setup_source_map(GENERICS_MULTI_FILE_DEFS);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    use ast::symbol::UntypedTypeParameterSymbol;
    use ast::type_parameter::UntypedTypeParameter;

    let t_tp = UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new("T".to_string())));
    let e_tp = UntypedTypeParameter::new(Rc::new(UntypedTypeParameterSymbol::new("E".to_string())));

    // pub struct Container<T> { T data }
    let container_symbol = Rc::new(StructSymbol::new(
        "Container".to_string(),
        vec![t_tp.clone()],
    ));
    let data_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "data".to_string(),
            UntypedDataType::new("T".to_string(), Vec::new()),
        )),
        Visibility::Private,
    ));
    let container_struct = wrap(Struct::new(
        container_symbol.clone(),
        Vec::new(),
        vec![data_field],
        Visibility::Public,
    ));

    // pub enum Result<T, E> { Ok(T), Err(E) }
    let result_symbol = Rc::new(EnumSymbol::new(
        "Result".to_string(),
        vec![t_tp.clone(), e_tp.clone()],
    ));
    let ok_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "Ok".to_string(),
        vec![UntypedDataType::new("T".to_string(), Vec::new())],
    ))));
    let err_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "Err".to_string(),
        vec![UntypedDataType::new("E".to_string(), Vec::new())],
    ))));
    let result_enum = wrap(Enum::new(
        result_symbol.clone(),
        vec![ok_variant, err_variant],
        Visibility::Public,
    ));

    // pub fn create_container<T>(T val) -> Container<T>
    let val_param = Rc::new(VariableSymbol::new(
        "val".to_string(),
        UntypedDataType::new("T".to_string(), Vec::new()),
    ));
    let create_container_symbol = Rc::new(FunctionSymbol::new(
        "create_container".to_string(),
        Some(UntypedDataType::new(
            "Container".to_string(),
            vec![UntypedDataType::new("T".to_string(), Vec::new())],
        )),
        vec![val_param],
        vec![t_tp.clone()],
    ));
    let create_container_func = wrap(Function::new(
        create_container_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::NewStruct(Box::new(
                NewStruct::<UntypedAST>::new(
                    (
                        "Container".to_string(),
                        vec![UntypedDataType::new("T".to_string(), Vec::new())],
                    ),
                    vec![(
                        wrap("data".to_string()),
                        wrap(Expression::Variable("val".to_string())),
                    )],
                ),
            )))))),
        )]))),
        Visibility::Public,
    ));

    let expected = ast::file::File::new(
        // The file is called main on disk
        "main".to_string(),
        vec![],
        vec![create_container_func],
        vec![result_enum],
        vec![container_struct],
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_generics_multi_file_main() {
    let (sm, id) = setup_source_map(GENERICS_MULTI_FILE_MAIN);
    let to_parse = FileInformation::new(id, "generic_multi_file", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    // import "./"
    let import = wrap(Import::new(
        ImportRoot::CurrentModule,
        vec![],
        Rc::new(ModuleUsageNameSymbol::new("generic_multi_file".to_string())),
    ));

    // generic_multi_file.Container<s32> c <- generic_multi_file.create_container<s32>(100)
    let c_var_symbol = Rc::new(VariableSymbol::new(
        "c".to_string(),
        UntypedDataType::new(
            "generic_multi_file.Container".to_string(),
            vec![UntypedDataType::new("s32".to_string(), Vec::new())],
        ),
    ));
    let c_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        c_var_symbol,
        wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            (
                "generic_multi_file.create_container".to_string(),
                vec![UntypedDataType::new("s32".to_string(), Vec::new())],
            ),
            vec![wrap(Expression::Literal("100".to_string()))],
        ))),
    )));

    // generic_multi_file.Result<s32, bool> r <- generic_multi_file.Result<s32, bool>::Ok(c.data)
    let r_var_symbol = Rc::new(VariableSymbol::new(
        "r".to_string(),
        UntypedDataType::new(
            "generic_multi_file.Result".to_string(),
            vec![
                UntypedDataType::new("s32".to_string(), Vec::new()),
                UntypedDataType::new("bool".to_string(), Vec::new()),
            ],
        ),
    ));
    let r_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        r_var_symbol,
        wrap(Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
            (
                "generic_multi_file.Result".to_string(),
                vec![
                    UntypedDataType::new("s32".to_string(), Vec::new()),
                    UntypedDataType::new("bool".to_string(), Vec::new()),
                ],
            ),
            "Ok".to_string(),
            vec![wrap(Expression::StructFieldAccess(Box::new(
                StructFieldAccess::<UntypedAST>::new(
                    wrap(Expression::Variable("c".to_string())),
                    "data".to_string(),
                ),
            )))],
        )))),
    )));

    let main_body = wrap(Statement::Codeblock(CodeBlock::new(vec![c_decl, r_decl])));
    let main_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "main".to_string(),
            None,
            vec![],
            Vec::new(),
        )),
        main_body,
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        vec![import],
        vec![main_func],
        Vec::new(),
        Vec::new(),
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_simple_program() {
    let (sm, id) = setup_source_map(FIBONACCI);

    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    // Construct Expected AST
    let n_symbol = Rc::new(VariableSymbol::new(
        "n".to_string(),
        UntypedDataType::new("u8".to_string(), Vec::new()),
    ));
    let fib_symbol = Rc::new(FunctionSymbol::new(
        "fibonacci".to_string(),
        Some(UntypedDataType::new("u64".to_string(), Vec::new())),
        vec![n_symbol.clone()],
        Vec::new(),
    ));

    let curr_symbol = Rc::new(VariableSymbol::new(
        "curr".to_string(),
        UntypedDataType::new("u64".to_string(), Vec::new()),
    ));
    let prev_symbol = Rc::new(VariableSymbol::new(
        "prev".to_string(),
        UntypedDataType::new("u64".to_string(), Vec::new()),
    ));
    let temp_symbol = Rc::new(VariableSymbol::new(
        "temp".to_string(),
        UntypedDataType::new("u64".to_string(), Vec::new()),
    ));

    // u64 curr <- 1 as u32 as u64
    let curr_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        curr_symbol.clone(),
        wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
            UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                "u64".to_string(),
                Vec::new(),
            ))),
            wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                    "u32".to_string(),
                    Vec::new(),
                ))),
                wrap(Expression::Literal("1".to_string())),
            )))),
        )))),
    )));

    // u64 prev <- 0 as u32 as u64
    let prev_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        prev_symbol.clone(),
        wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
            UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                "u64".to_string(),
                Vec::new(),
            ))),
            wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                    "u32".to_string(),
                    Vec::new(),
                ))),
                wrap(Expression::Literal("0".to_string())),
            )))),
        )))),
    )));

    // Loop condition: n > 0
    let loop_cond = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Greater,
        wrap(Expression::Variable("n".to_string())),
        wrap(Expression::Literal("0".to_string())),
    ))));

    // Loop Body
    // u64 temp <- curr
    let temp_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        temp_symbol.clone(),
        wrap(Expression::Variable("curr".to_string())),
    )));

    // curr <- curr + prev
    let curr_assign = wrap(Statement::VariableAssignment(VariableAssignment::<
        UntypedAST,
    >::new(
        "curr".to_string(),
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Addition,
            wrap(Expression::Variable("curr".to_string())),
            wrap(Expression::Variable("prev".to_string())),
        )))),
    )));

    // prev <- temp
    let prev_assign = wrap(Statement::VariableAssignment(VariableAssignment::<
        UntypedAST,
    >::new(
        "prev".to_string(),
        wrap(Expression::Variable("temp".to_string())),
    )));

    // n <- n - 1 as u32 as u16 as u8
    let n_assign = wrap(Statement::VariableAssignment(VariableAssignment::<
        UntypedAST,
    >::new(
        "n".to_string(),
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Subtraction,
            wrap(Expression::Variable("n".to_string())),
            wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                    "u8".to_string(),
                    Vec::new(),
                ))),
                wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                        "u16".to_string(),
                        Vec::new(),
                    ))),
                    wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                            "u32".to_string(),
                            Vec::new(),
                        ))),
                        wrap(Expression::Literal("1".to_string())),
                    )))),
                )))),
            )))),
        )))),
    )));

    let loop_stmt = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Loop(Loop::new(
            wrap(Statement::Codeblock(CodeBlock::new(vec![
                temp_decl,
                curr_assign,
                prev_assign,
                n_assign,
            ]))),
            LoopType::While(loop_cond),
        )),
    )));

    // -> curr
    let ret_stmt = wrap(Statement::Return(Return::new(Some(wrap(
        Expression::Variable("curr".to_string()),
    )))));

    let function_body = wrap(Statement::Codeblock(CodeBlock::new(vec![
        curr_decl, prev_decl, loop_stmt, ret_stmt,
    ])));

    let function = wrap(Function::new(
        fib_symbol,
        function_body,
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(), // imports
        vec![function],
        Vec::new(),
        Vec::new(),
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_max() {
    let (sm, id) = setup_source_map(MAX);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let a_symbol = Rc::new(VariableSymbol::new(
        "a".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let b_symbol = Rc::new(VariableSymbol::new(
        "b".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let max_symbol = Rc::new(FunctionSymbol::new(
        "max".to_string(),
        Some(UntypedDataType::new("s32".to_string(), Vec::new())),
        vec![a_symbol.clone(), b_symbol.clone()],
        Vec::new(),
    ));

    let cond = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Greater,
        wrap(Expression::Variable("a".to_string())),
        wrap(Expression::Variable("b".to_string())),
    ))));

    let then_stmt = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::Variable(
            "a".to_string(),
        ))))),
    )])));

    let else_stmt = Some(wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::Variable(
            "b".to_string(),
        ))))),
    )]))));

    let conditional = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Conditional(Conditional::new(cond, then_stmt, else_stmt)),
    )));

    let function = wrap(Function::new(
        max_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![conditional]))),
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![function],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_sum_n() {
    let (sm, id) = setup_source_map(SUM_N);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let n_symbol = Rc::new(VariableSymbol::new(
        "n".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let sum_n_symbol = Rc::new(FunctionSymbol::new(
        "sum_n".to_string(),
        Some(UntypedDataType::new("s32".to_string(), Vec::new())),
        vec![n_symbol.clone()],
        Vec::new(),
    ));

    let sum_symbol = Rc::new(VariableSymbol::new(
        "sum".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let sum_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        sum_symbol.clone(),
        wrap(Expression::Literal("0".to_string())),
    )));

    let i_symbol = Rc::new(VariableSymbol::new(
        "i".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let init = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        i_symbol.clone(),
        wrap(Expression::Literal("0".to_string())),
    )));

    let cond = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Lesser,
        wrap(Expression::Variable("i".to_string())),
        wrap(Expression::Variable("n".to_string())),
    ))));

    let step = wrap(Statement::VariableAssignment(VariableAssignment::<
        UntypedAST,
    >::new(
        "i".to_string(),
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Addition,
            wrap(Expression::Variable("i".to_string())),
            wrap(Expression::Literal("1".to_string())),
        )))),
    )));

    let body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
            "sum".to_string(),
            wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                BinaryOpType::Addition,
                wrap(Expression::Variable("sum".to_string())),
                wrap(Expression::Variable("i".to_string())),
            )))),
        )),
    )])));

    let loop_stmt = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Loop(Loop::new(
            body,
            LoopType::For {
                start: init,
                cond,
                after_each: step,
            },
        )),
    )));

    let ret_stmt = wrap(Statement::Return(Return::new(Some(wrap(
        Expression::Variable("sum".to_string()),
    )))));

    let function = wrap(Function::new(
        sum_n_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![
            sum_decl, loop_stmt, ret_stmt,
        ]))),
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![function],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_is_even() {
    let (sm, id) = setup_source_map(IS_EVEN);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let n_symbol = Rc::new(VariableSymbol::new(
        "n".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let is_even_symbol = Rc::new(FunctionSymbol::new(
        "is_even".to_string(),
        Some(UntypedDataType::new("bool".to_string(), Vec::new())),
        vec![n_symbol.clone()],
        Vec::new(),
    ));

    let cond = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Equals,
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Modulo,
            wrap(Expression::Variable("n".to_string())),
            wrap(Expression::Literal("2".to_string())),
        )))),
        wrap(Expression::Literal("0".to_string())),
    ))));

    let then_stmt = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::Literal(
            "true".to_string(),
        ))))),
    )])));

    let conditional = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Conditional(Conditional::new(cond, then_stmt, None)),
    )));

    let ret_stmt = wrap(Statement::Return(Return::new(Some(wrap(
        Expression::Literal("false".to_string()),
    )))));

    let function = wrap(Function::new(
        is_even_symbol,
        wrap(Statement::Codeblock(CodeBlock::new(vec![
            conditional,
            ret_stmt,
        ]))),
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![function],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

fn setup_source_map(content: &'static str) -> (SourceMap, FileID) {
    let (dir, _path) = setup_file("main.waso", content);

    let mut sm: SourceMap = SourceMap::new(dir.path().to_path_buf());

    let id = sm
        .load_file("main.waso")
        .expect("Failed to load existing file");
    (sm, id)
}

#[test]
fn test_parse_modular_arithmetic() {
    // Test modular_add.waso
    {
        let (sm, id) = setup_source_map(MODULAR_ADD);
        let to_parse = FileInformation::new(id, "test", &sm).unwrap();
        let parsed = parse(to_parse).expect("Parsing failed");

        let a_symbol = Rc::new(VariableSymbol::new(
            "a".to_string(),
            UntypedDataType::new("u32".to_string(), Vec::new()),
        ));
        let b_symbol = Rc::new(VariableSymbol::new(
            "b".to_string(),
            UntypedDataType::new("u32".to_string(), Vec::new()),
        ));
        let m_symbol = Rc::new(VariableSymbol::new(
            "m".to_string(),
            UntypedDataType::new("u32".to_string(), Vec::new()),
        ));
        let mod_add_symbol = Rc::new(FunctionSymbol::new(
            "modular_add".to_string(),
            Some(UntypedDataType::new("u32".to_string(), Vec::new())),
            vec![a_symbol, b_symbol, m_symbol],
            Vec::new(),
        ));

        let body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::BinaryOp(Box::new(
                BinaryOp::<UntypedAST>::new(
                    BinaryOpType::Modulo,
                    wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Addition,
                        wrap(Expression::Variable("a".to_string())),
                        wrap(Expression::Variable("b".to_string())),
                    )))),
                    wrap(Expression::Variable("m".to_string())),
                ),
            )))))),
        )])));

        let function = wrap(Function::new(mod_add_symbol, body, Visibility::Private));
        let expected = ast::file::File::new(
            "main".to_string(),
            Vec::new(),
            vec![function],
            Vec::new(),
            Vec::new(),
        );
        assert!(parsed.semantic_eq(&expected));
    }

    // Test modular_mul.waso
    {
        let (sm, id) = setup_source_map(MODULAR_MUL);
        let to_parse = FileInformation::new(id, "test", &sm).unwrap();
        let parsed = parse(to_parse).expect("Parsing failed");

        let test_symbol = Rc::new(ModuleUsageNameSymbol::new("test".to_string()));
        let import = wrap(Import::new(
            ImportRoot::CurrentModule,
            Vec::new(),
            test_symbol,
        ));

        let a_symbol = Rc::new(VariableSymbol::new(
            "a".to_string(),
            UntypedDataType::new("u32".to_string(), Vec::new()),
        ));
        let b_symbol = Rc::new(VariableSymbol::new(
            "b".to_string(),
            UntypedDataType::new("u32".to_string(), Vec::new()),
        ));
        let m_symbol = Rc::new(VariableSymbol::new(
            "m".to_string(),
            UntypedDataType::new("u32".to_string(), Vec::new()),
        ));
        let mod_mul_symbol = Rc::new(FunctionSymbol::new(
            "modular_mul".to_string(),
            Some(UntypedDataType::new("u32".to_string(), Vec::new())),
            vec![a_symbol, b_symbol, m_symbol],
            Vec::new(),
        ));

        let call = wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            ("test.modular_add".to_string(), Vec::new()),
            vec![
                wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::Multiplication,
                    wrap(Expression::Variable("a".to_string())),
                    wrap(Expression::Variable("b".to_string())),
                )))),
                wrap(Expression::Literal("0".to_string())),
                wrap(Expression::Variable("m".to_string())),
            ],
        )));

        let body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(call))),
        )])));

        let function = wrap(Function::new(mod_mul_symbol, body, Visibility::Private));
        let expected = ast::file::File::new(
            "main".to_string(),
            vec![import],
            vec![function],
            Vec::new(),
            Vec::new(),
        );
        assert!(parsed.semantic_eq(&expected));
    }
}

#[test]
fn test_misc_features() {
    let (sm, id) = setup_source_map(MISC_FEATURES);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    // 1. public_func
    let pub_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "public_func".to_string(),
            None,
            vec![],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![]))),
        Visibility::Public,
    ));

    // 2. bitwise
    let a = Rc::new(VariableSymbol::new(
        "a".to_string(),
        UntypedDataType::new("u32".to_string(), Vec::new()),
    ));
    let b = Rc::new(VariableSymbol::new(
        "b".to_string(),
        UntypedDataType::new("u32".to_string(), Vec::new()),
    ));
    let bitwise_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "bitwise".to_string(),
            Some(UntypedDataType::new("u32".to_string(), Vec::new())),
            vec![a.clone(), b.clone()],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::BinaryOp(Box::new(
                BinaryOp::<UntypedAST>::new(
                    BinaryOpType::BitwiseOr,
                    wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::BitwiseAnd,
                        wrap(Expression::Variable("a".to_string())),
                        wrap(Expression::Variable("b".to_string())),
                    )))),
                    wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::RightShift,
                        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                            BinaryOpType::LeftShift,
                            wrap(Expression::Variable("a".to_string())),
                            wrap(Expression::Literal("1".to_string())),
                        )))),
                        wrap(Expression::Literal("1".to_string())),
                    )))),
                ),
            )))))),
        )]))),
        Visibility::Private,
    ));

    // 3. infinite
    let infinite_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "infinite".to_string(),
            None,
            vec![],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::ControlStructure(Box::new(ControlStructure::Loop(Loop::new(
                wrap(Statement::Codeblock(CodeBlock::new(vec![]))),
                LoopType::Infinite,
            )))),
        )]))),
        Visibility::Private,
    ));

    // 4. chars
    let chars_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "chars".to_string(),
            Some(UntypedDataType::new("char".to_string(), Vec::new())),
            vec![],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::Literal(
                "'c'".to_string(),
            ))))),
        )]))),
        Visibility::Private,
    ));

    // 5. unary
    let bool_a = Rc::new(VariableSymbol::new(
        "a".to_string(),
        UntypedDataType::new("bool".to_string(), Vec::new()),
    ));
    let unary_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "unary".to_string(),
            Some(UntypedDataType::new("bool".to_string(), Vec::new())),
            vec![bool_a.clone()],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::UnaryOp(Box::new(
                UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Not,
                    wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Not,
                        wrap(Expression::Variable("a".to_string())),
                    )))),
                ),
            )))))),
        )]))),
        Visibility::Private,
    ));

    // 6. precedence
    let u32_a = Rc::new(VariableSymbol::new(
        "a".to_string(),
        UntypedDataType::new("u32".to_string(), Vec::new()),
    ));
    let u32_b = Rc::new(VariableSymbol::new(
        "b".to_string(),
        UntypedDataType::new("u32".to_string(), Vec::new()),
    ));
    let u32_c = Rc::new(VariableSymbol::new(
        "c".to_string(),
        UntypedDataType::new("u32".to_string(), Vec::new()),
    ));
    let precedence_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "precedence".to_string(),
            Some(UntypedDataType::new("u32".to_string(), Vec::new())),
            vec![u32_a.clone(), u32_b.clone(), u32_c.clone()],
            Vec::new(),
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::BinaryOp(Box::new(
                BinaryOp::<UntypedAST>::new(
                    BinaryOpType::Addition,
                    wrap(Expression::Variable("a".to_string())),
                    wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        wrap(Expression::Variable("b".to_string())),
                        wrap(Expression::Variable("c".to_string())),
                    )))),
                ),
            )))))),
        )]))),
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![
            pub_func,
            bitwise_func,
            infinite_func,
            chars_func,
            unary_func,
            precedence_func,
        ],
        Vec::new(),
        Vec::new(),
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_unary_on_typecast() {
    let (sm, id) = setup_source_map(UNARY_CAST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        Some(UntypedDataType::new("f32".to_string(), Vec::new())),
        vec![],
        Vec::new(),
    ));

    let body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::UnaryOp(Box::new(
            UnaryOp::<UntypedAST>::new(
                UnaryOpType::Negative,
                wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                        "f32".to_string(),
                        Vec::new(),
                    ))),
                    wrap(Expression::Literal("5".to_string())),
                )))),
            ),
        )))))),
    )])));

    let function = wrap(Function::new(main_symbol, body, Visibility::Private));
    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![function],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_if() {
    let (sm, id) = setup_source_map(IF_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    // fn main() { char wasome <- showcase_if_conditionals() }
    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        None,
        vec![],
        Vec::new(),
    ));
    let main_body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::VariableDeclaration(VariableDeclaration::<UntypedAST>::new(
            Rc::new(VariableSymbol::new(
                "wasome".to_string(),
                UntypedDataType::new("char".to_string(), Vec::new()),
            )),
            wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
                ("showcase_if_conditionals".to_string(), Vec::new()),
                vec![],
            ))),
        )),
    )])));
    let main_func = wrap(Function::new(main_symbol, main_body, Visibility::Private));

    // fn showcase_if_conditionals() -> char
    let showcase_symbol = Rc::new(FunctionSymbol::new(
        "showcase_if_conditionals".to_string(),
        Some(UntypedDataType::new("char".to_string(), Vec::new())),
        vec![],
        Vec::new(),
    ));

    // bool wasome_is_awesome <- true
    let var_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "wasome_is_awesome".to_string(),
            UntypedDataType::new("bool".to_string(), Vec::new()),
        )),
        wrap(Expression::Literal("true".to_string())),
    )));

    // if (wasome_is_awesome) { -> '✨' } else { -> '🤥' }
    let cond = wrap(Expression::Variable("wasome_is_awesome".to_string()));
    let then_stmt = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::Literal(
            "'✨'".to_string(),
        ))))),
    )])));
    let else_stmt = Some(wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::Literal(
            "'🤥'".to_string(),
        ))))),
    )]))));

    let if_stmt = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Conditional(Conditional::new(cond, then_stmt, else_stmt)),
    )));

    // -> ' '
    let ret_stmt = wrap(Statement::Return(Return::new(Some(wrap(
        Expression::Literal("' '".to_string()),
    )))));

    let showcase_body = wrap(Statement::Codeblock(CodeBlock::new(vec![
        var_decl, if_stmt, ret_stmt,
    ])));
    let showcase_func = wrap(Function::new(
        showcase_symbol,
        showcase_body,
        Visibility::Private,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![main_func, showcase_func],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_loop() {
    let (sm, id) = setup_source_map(LOOP_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        None,
        vec![],
        Vec::new(),
    ));

    // s32 count1 <- 0
    let decl1 = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "count1".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        wrap(Expression::Literal("0".to_string())),
    )));

    // loop (count1 < 10) { count1 <- count1 + 1 }
    let while_cond = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Lesser,
        wrap(Expression::Variable("count1".to_string())),
        wrap(Expression::Literal("10".to_string())),
    ))));
    let while_body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
            "count1".to_string(),
            wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                BinaryOpType::Addition,
                wrap(Expression::Variable("count1".to_string())),
                wrap(Expression::Literal("1".to_string())),
            )))),
        )),
    )])));
    let while_loop = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Loop(Loop::new(while_body, LoopType::While(while_cond))),
    )));

    // s32 sum <- 0
    let decl_sum = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "sum".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        wrap(Expression::Literal("0".to_string())),
    )));

    // loop (s32 count2 <- 0; count2 < 100; count2 <- count2 + 1) { sum <- sum + count2 }
    let for_init = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "count2".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        wrap(Expression::Literal("0".to_string())),
    )));
    let for_cond = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Lesser,
        wrap(Expression::Variable("count2".to_string())),
        wrap(Expression::Literal("100".to_string())),
    ))));
    let for_after = wrap(Statement::VariableAssignment(VariableAssignment::<
        UntypedAST,
    >::new(
        "count2".to_string(),
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Addition,
            wrap(Expression::Variable("count2".to_string())),
            wrap(Expression::Literal("1".to_string())),
        )))),
    )));
    let for_body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
            "sum".to_string(),
            wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                BinaryOpType::Addition,
                wrap(Expression::Variable("sum".to_string())),
                wrap(Expression::Variable("count2".to_string())),
            )))),
        )),
    )])));
    let for_loop = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Loop(Loop::new(
            for_body,
            LoopType::For {
                start: for_init,
                cond: for_cond,
                after_each: for_after,
            },
        )),
    )));

    // s32 count3 <- 0
    let decl3 = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "count3".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        wrap(Expression::Literal("0".to_string())),
    )));

    // loop { count3 <- count3 + 1 }
    let inf_body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::VariableAssignment(VariableAssignment::<UntypedAST>::new(
            "count3".to_string(),
            wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                BinaryOpType::Addition,
                wrap(Expression::Variable("count3".to_string())),
                wrap(Expression::Literal("1".to_string())),
            )))),
        )),
    )])));
    let inf_loop = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Loop(Loop::new(inf_body, LoopType::Infinite)),
    )));

    let main_body = wrap(Statement::Codeblock(CodeBlock::new(vec![
        decl1, while_loop, decl_sum, for_loop, decl3, inf_loop,
    ])));
    let main_func = wrap(Function::new(main_symbol, main_body, Visibility::Private));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![main_func],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_operator() {
    let (sm, id) = setup_source_map(OPERATOR_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        None,
        vec![],
        Vec::new(),
    ));

    // s32 math_showcase <- 10 * 2 + 5 - 3 / 1
    // ((10 * 2) + 5) - (3 / 1)
    let expr_math = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Subtraction,
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Addition,
            wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                BinaryOpType::Multiplication,
                wrap(Expression::Literal("10".to_string())),
                wrap(Expression::Literal("2".to_string())),
            )))),
            wrap(Expression::Literal("5".to_string())),
        )))),
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Division,
            wrap(Expression::Literal("3".to_string())),
            wrap(Expression::Literal("1".to_string())),
        )))),
    ))));
    let decl_math = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "math_showcase".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        expr_math,
    )));

    // s32 num <- 10
    let decl_num = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "num".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        wrap(Expression::Literal("10".to_string())),
    )));

    // Helper to create simple if statements with empty bodies
    let create_if = |op: BinaryOpType, rhs: &str| {
        let cond = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            op,
            wrap(Expression::Variable("num".to_string())),
            wrap(Expression::Literal(rhs.to_string())),
        ))));
        wrap(Statement::ControlStructure(Box::new(
            ControlStructure::Conditional(Conditional::new(
                cond,
                wrap(Statement::Codeblock(CodeBlock::new(vec![]))),
                None,
            )),
        )))
    };

    // if (num < 10)
    let if_lt = create_if(BinaryOpType::Lesser, "10");
    // if (num <= 10)
    let if_le = create_if(BinaryOpType::LesserEquals, "10");
    // if (num == 10)
    let if_eq = create_if(BinaryOpType::Equals, "10");
    // if (num != 10)
    let if_ne = create_if(BinaryOpType::NotEquals, "10");
    // if (num > 10)
    let if_gt = create_if(BinaryOpType::Greater, "10");
    // if (num >= 10)
    let if_ge = create_if(BinaryOpType::GreaterEquals, "10");

    // if (num == 10 && true)
    let cond_and = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::And,
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Equals,
            wrap(Expression::Variable("num".to_string())),
            wrap(Expression::Literal("10".to_string())),
        )))),
        wrap(Expression::Literal("true".to_string())),
    ))));
    let if_and = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Conditional(Conditional::new(
            cond_and,
            wrap(Statement::Codeblock(CodeBlock::new(vec![]))),
            None,
        )),
    )));

    // if (num == 10 || false)
    let cond_or = wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
        BinaryOpType::Or,
        wrap(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            BinaryOpType::Equals,
            wrap(Expression::Variable("num".to_string())),
            wrap(Expression::Literal("10".to_string())),
        )))),
        wrap(Expression::Literal("false".to_string())),
    ))));
    let if_or = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::Conditional(Conditional::new(
            cond_or,
            wrap(Statement::Codeblock(CodeBlock::new(vec![]))),
            None,
        )),
    )));

    let main_body = wrap(Statement::Codeblock(CodeBlock::new(vec![
        decl_math, decl_num, if_lt, if_le, if_eq, if_ne, if_gt, if_ge, if_and, if_or,
    ])));
    let main_func = wrap(Function::new(main_symbol, main_body, Visibility::Private));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![main_func],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_missing_statement_separator() {
    let (sm, id) = setup_source_map(MISSING_STATEMENT_SEPARATOR);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse);
    assert!(parsed.is_none());
}

#[test]
fn test_parse_struct() {
    let (sm, id) = setup_source_map(STRUCT_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let point_symbol = Rc::new(StructSymbol::new("Point".to_string(), Vec::new()));
    let x_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "x".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        Visibility::Public,
    ));
    let y_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "y".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        Visibility::Public,
    ));

    let point_struct = wrap(Struct::new(
        point_symbol.clone(),
        Vec::new(),
        vec![x_field, y_field],
        Visibility::Private,
    ));

    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        None,
        vec![],
        Vec::new(),
    ));

    // Point point <- new Point { x <- 10, y <- 20 }
    let point_var = Rc::new(VariableSymbol::new(
        "point".to_string(),
        UntypedDataType::new("Point".to_string(), Vec::new()),
    ));
    let new_point_expr = wrap(Expression::NewStruct(Box::new(
        NewStruct::<UntypedAST>::new(
            ("Point".to_string(), Vec::new()),
            vec![
                (
                    wrap("x".to_string()),
                    wrap(Expression::Literal("10".to_string())),
                ),
                (
                    wrap("y".to_string()),
                    wrap(Expression::Literal("20".to_string())),
                ),
            ],
        ),
    )));
    let stmt1 = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        point_var.clone(),
        new_point_expr,
    )));

    // s32 old_x_coordinate <- point.x
    let old_x_var = Rc::new(VariableSymbol::new(
        "old_x_coordinate".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let access_expr = wrap(Expression::StructFieldAccess(Box::new(
        StructFieldAccess::<UntypedAST>::new(
            wrap(Expression::Variable("point".to_string())),
            "x".to_string(),
        ),
    )));
    let stmt2 = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        old_x_var.clone(),
        access_expr,
    )));

    // point.x <- 15
    let stmt3 = wrap(Statement::StructFieldAssignment(StructFieldAssignment::<
        UntypedAST,
    >::new(
        wrap(Expression::Variable("point".to_string())),
        "x".to_string(),
        wrap(Expression::Literal("15".to_string())),
    )));

    let main_body = wrap(Statement::Codeblock(CodeBlock::new(vec![
        stmt1, stmt2, stmt3,
    ])));
    let main_func = wrap(Function::new(main_symbol, main_body, Visibility::Private));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![main_func],
        Vec::new(),
        vec![point_struct],
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_enum() {
    let (sm, id) = setup_source_map(ENUM_TEST);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let weekday_symbol = Rc::new(EnumSymbol::new("Weekday".to_string(), Vec::new()));
    let variants = vec![
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday",
    ]
    .into_iter()
    .map(|name| {
        wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
            name.to_string(),
            vec![],
        ))))
    })
    .collect();

    let weekday_enum = wrap(Enum::new(
        weekday_symbol.clone(),
        variants,
        Visibility::Private,
    ));

    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        None,
        vec![],
        Vec::new(),
    ));

    // Weekday weekday <- Weekday::Saturday
    let weekday_var = Rc::new(VariableSymbol::new(
        "weekday".to_string(),
        UntypedDataType::new("Weekday".to_string(), Vec::new()),
    ));
    let new_enum_expr = wrap(Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
        ("Weekday".to_string(), Vec::new()),
        "Saturday".to_string(),
        vec![],
    ))));
    let stmt1 = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        weekday_var.clone(),
        new_enum_expr,
    )));

    let main_body = wrap(Statement::Codeblock(CodeBlock::new(vec![stmt1])));
    let main_func = wrap(Function::new(main_symbol, main_body, Visibility::Private));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![main_func],
        vec![weekday_enum],
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_exhaustive_defs() {
    let (sm, id) = setup_source_map(EXHAUSTIVE_DEFS);
    let to_parse = FileInformation::new(id, "exhaustive", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing defs failed");

    let status_symbol = Rc::new(EnumSymbol::new("Status".to_string(), Vec::new()));
    let ok_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "Ok".to_string(),
        vec![],
    ))));
    let err_variant = wrap(EnumVariant::new(Rc::new(EnumVariantSymbol::new(
        "Err".to_string(),
        vec![UntypedDataType::new("s32".to_string(), Vec::new())],
    ))));
    let status_enum = wrap(Enum::new(
        status_symbol,
        vec![ok_variant, err_variant],
        Visibility::Public,
    ));

    let point_symbol = Rc::new(StructSymbol::new("Point".to_string(), Vec::new()));
    let x_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "x".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        Visibility::Public,
    ));
    let y_field = wrap(StructField::new(
        Rc::new(StructFieldSymbol::new(
            "y".to_string(),
            UntypedDataType::new("s32".to_string(), Vec::new()),
        )),
        Visibility::Public,
    ));
    let point_struct = wrap(Struct::new(
        point_symbol,
        Vec::new(),
        vec![x_field, y_field],
        Visibility::Public,
    ));

    let x_param = Rc::new(VariableSymbol::new(
        "x".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let y_param = Rc::new(VariableSymbol::new(
        "y".to_string(),
        UntypedDataType::new("s32".to_string(), Vec::new()),
    ));
    let create_point_symbol = Rc::new(FunctionSymbol::new(
        "create_point".to_string(),
        Some(UntypedDataType::new("Point".to_string(), Vec::new())),
        vec![x_param, y_param],
        Vec::new(),
    ));
    let create_point_body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::NewStruct(Box::new(
            NewStruct::<UntypedAST>::new(
                ("Point".to_string(), Vec::new()),
                vec![
                    (
                        wrap("x".to_string()),
                        wrap(Expression::Variable("x".to_string())),
                    ),
                    (
                        wrap("y".to_string()),
                        wrap(Expression::Variable("y".to_string())),
                    ),
                ],
            ),
        )))))),
    )])));
    let create_point_func = wrap(Function::new(
        create_point_symbol,
        create_point_body,
        Visibility::Public,
    ));

    let expected = ast::file::File::new(
        "main".to_string(),
        Vec::new(),
        vec![create_point_func],
        vec![status_enum],
        vec![point_struct],
    );
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_exhaustive_main() {
    let (sm, id) = setup_source_map(EXHAUSTIVE_MAIN);
    let to_parse = FileInformation::new(id, "exhaustive", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing main failed");

    let import = wrap(Import::new(
        ImportRoot::CurrentModule,
        Vec::new(),
        Rc::new(ModuleUsageNameSymbol::new("exhaustive".to_string())),
    ));

    let main_symbol = Rc::new(FunctionSymbol::new(
        "main".to_string(),
        None,
        vec![],
        Vec::new(),
    ));

    // exhaustive.Point p <- exhaustive.create_point(10, 20)
    let p_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        Rc::new(VariableSymbol::new(
            "p".to_string(),
            UntypedDataType::new("exhaustive.Point".to_string(), Vec::new()),
        )),
        wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            ("exhaustive.create_point".to_string(), Vec::new()),
            vec![
                wrap(Expression::Literal("10".to_string())),
                wrap(Expression::Literal("20".to_string())),
            ],
        ))),
    )));

    // if (let exhaustive.Status.Err(s32 code) <- exhaustive.Status.Ok) { p.x <- code }
    let if_let = wrap(Statement::ControlStructure(Box::new(
        ControlStructure::IfEnumVariant(IfEnumVariant::<UntypedAST>::new(
            ("exhaustive.Status".to_string(), Vec::new()),
            "Err".to_string(),
            wrap(Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
                ("exhaustive.Status".to_string(), Vec::new()),
                "Ok".to_string(),
                vec![],
            )))),
            vec![Rc::new(VariableSymbol::new(
                "code".to_string(),
                UntypedDataType::new("s32".to_string(), Vec::new()),
            ))],
            wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
                Statement::StructFieldAssignment(StructFieldAssignment::<UntypedAST>::new(
                    wrap(Expression::Variable("p".to_string())),
                    "x".to_string(),
                    wrap(Expression::Variable("code".to_string())),
                )),
            )]))),
        )),
    )));

    // p.y <- exhaustive.create_point(1, 2).y
    let p_y_assign = wrap(Statement::StructFieldAssignment(StructFieldAssignment::<
        UntypedAST,
    >::new(
        wrap(Expression::Variable("p".to_string())),
        "y".to_string(),
        wrap(Expression::StructFieldAccess(Box::new(
            StructFieldAccess::<UntypedAST>::new(
                wrap(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
                    ("exhaustive.create_point".to_string(), Vec::new()),
                    vec![
                        wrap(Expression::Literal("1".to_string())),
                        wrap(Expression::Literal("2".to_string())),
                    ],
                ))),
                "y".to_string(),
            ),
        ))),
    )));

    let main_body = wrap(Statement::Codeblock(CodeBlock::new(vec![
        p_decl, if_let, p_y_assign,
    ])));
    let main_func = wrap(Function::new(main_symbol, main_body, Visibility::Private));

    let expected = ast::file::File::new(
        "main".to_string(),
        vec![import],
        vec![main_func],
        Vec::new(),
        Vec::new(),
    );
    assert!(parsed.semantic_eq(&expected));
}
