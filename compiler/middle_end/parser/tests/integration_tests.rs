use ast::expression::{BinaryOp, BinaryOpType, Expression, Typecast, UnaryOp, UnaryOpType};
use ast::statement::{
    CodeBlock, Conditional, ControlStructure, Loop, LoopType, Return, Statement,
    VariableAssignment, VariableDeclaration,
};
use ast::symbol::{FunctionSymbol, ModuleUsageNameSymbol, VariableSymbol};
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

const FIBONACCI: &'static str = include_str!("test_programms/fibonacci.waso");
const MAX: &'static str = include_str!("test_programms/max.waso");
const SUM_N: &'static str = include_str!("test_programms/sum_n.waso");
const IS_EVEN: &'static str = include_str!("test_programms/is_even.waso");
const MODULAR_ADD: &'static str =
    include_str!("test_programms/modular_arithmetic/modular_add.waso");
const MODULAR_MUL: &'static str =
    include_str!("test_programms/modular_arithmetic/modular_mul.waso");
const MISC_FEATURES: &'static str = include_str!("test_programms/misc.waso");
const UNARY_CAST: &'static str = include_str!("test_programms/unary_cast.waso");
const MISSING_IMPORT_SEPARATOR: &'static str =
    include_str!("test_programms/missing_import_separator.waso");

#[test]
fn test_parse_simple_programm() {
    let (sm, id) = setup_source_map(FIBONACCI);

    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    // Construct Expected AST
    let n_symbol = Rc::new(VariableSymbol::new("n".to_string(), "u8".to_string()));
    let fib_symbol = Rc::new(FunctionSymbol::new(
        "fibonacci".to_string(),
        Some("u64".to_string()),
        vec![n_symbol.clone()],
    ));

    let curr_symbol = Rc::new(VariableSymbol::new("curr".to_string(), "u64".to_string()));
    let prev_symbol = Rc::new(VariableSymbol::new("prev".to_string(), "u64".to_string()));
    let temp_symbol = Rc::new(VariableSymbol::new("temp".to_string(), "u64".to_string()));

    // u64 curr <- 1 as u32 as u64
    let curr_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        curr_symbol.clone(),
        wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
            UnaryOpType::Typecast(Typecast::new("u64".to_string())),
            wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                UnaryOpType::Typecast(Typecast::new("u32".to_string())),
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
            UnaryOpType::Typecast(Typecast::new("u64".to_string())),
            wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                UnaryOpType::Typecast(Typecast::new("u32".to_string())),
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
                UnaryOpType::Typecast(Typecast::new("u8".to_string())),
                wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("u16".to_string())),
                    wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new("u32".to_string())),
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
        Vec::new()
    );

    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_max() {
    let (sm, id) = setup_source_map(MAX);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let a_symbol = Rc::new(VariableSymbol::new("a".to_string(), "s32".to_string()));
    let b_symbol = Rc::new(VariableSymbol::new("b".to_string(), "s32".to_string()));
    let max_symbol = Rc::new(FunctionSymbol::new(
        "max".to_string(),
        Some("s32".to_string()),
        vec![a_symbol.clone(), b_symbol.clone()],
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

    let expected = ast::file::File::new("main".to_string(), Vec::new(), vec![function], Vec::new(),
                                        Vec::new());
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_sum_n() {
    let (sm, id) = setup_source_map(SUM_N);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let n_symbol = Rc::new(VariableSymbol::new("n".to_string(), "s32".to_string()));
    let sum_n_symbol = Rc::new(FunctionSymbol::new(
        "sum_n".to_string(),
        Some("s32".to_string()),
        vec![n_symbol.clone()],
    ));

    let sum_symbol = Rc::new(VariableSymbol::new("sum".to_string(), "s32".to_string()));
    let sum_decl = wrap(Statement::VariableDeclaration(VariableDeclaration::<
        UntypedAST,
    >::new(
        sum_symbol.clone(),
        wrap(Expression::Literal("0".to_string())),
    )));

    let i_symbol = Rc::new(VariableSymbol::new("i".to_string(), "s32".to_string()));
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

    let expected = ast::file::File::new("main".to_string(), Vec::new(), vec![function], Vec::new(),
                                        Vec::new());
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_parse_is_even() {
    let (sm, id) = setup_source_map(IS_EVEN);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse).expect("Parsing failed");

    let n_symbol = Rc::new(VariableSymbol::new("n".to_string(), "s32".to_string()));
    let is_even_symbol = Rc::new(FunctionSymbol::new(
        "is_even".to_string(),
        Some("bool".to_string()),
        vec![n_symbol.clone()],
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

    let expected = ast::file::File::new("main".to_string(), Vec::new(), vec![function], Vec::new(),
                                        Vec::new());
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

        let a_symbol = Rc::new(VariableSymbol::new("a".to_string(), "u32".to_string()));
        let b_symbol = Rc::new(VariableSymbol::new("b".to_string(), "u32".to_string()));
        let m_symbol = Rc::new(VariableSymbol::new("m".to_string(), "u32".to_string()));
        let mod_add_symbol = Rc::new(FunctionSymbol::new(
            "modular_add".to_string(),
            Some("u32".to_string()),
            vec![a_symbol, b_symbol, m_symbol],
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
        let expected = ast::file::File::new("main".to_string(), Vec::new(), vec![function], Vec::new(),
                                            Vec::new());
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

        let a_symbol = Rc::new(VariableSymbol::new("a".to_string(), "u32".to_string()));
        let b_symbol = Rc::new(VariableSymbol::new("b".to_string(), "u32".to_string()));
        let m_symbol = Rc::new(VariableSymbol::new("m".to_string(), "u32".to_string()));
        let mod_mul_symbol = Rc::new(FunctionSymbol::new(
            "modular_mul".to_string(),
            Some("u32".to_string()),
            vec![a_symbol, b_symbol, m_symbol],
        ));

        let call = wrap(Expression::FunctionCall(ast::expression::FunctionCall::<
            UntypedAST,
        >::new(
            "test.modular_add".to_string(),
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
        let expected = ast::file::File::new("main".to_string(), vec![import], vec![function], Vec::new(),
                                            Vec::new());
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
        Rc::new(FunctionSymbol::new("public_func".to_string(), None, vec![])),
        wrap(Statement::Codeblock(CodeBlock::new(vec![]))),
        Visibility::Public,
    ));

    // 2. bitwise
    let a = Rc::new(VariableSymbol::new("a".to_string(), "u32".to_string()));
    let b = Rc::new(VariableSymbol::new("b".to_string(), "u32".to_string()));
    let bitwise_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "bitwise".to_string(),
            Some("u32".to_string()),
            vec![a.clone(), b.clone()],
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
        Rc::new(FunctionSymbol::new("infinite".to_string(), None, vec![])),
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
            Some("char".to_string()),
            vec![],
        )),
        wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
            Statement::Return(Return::new(Some(wrap(Expression::Literal(
                "c".to_string(),
            ))))),
        )]))),
        Visibility::Private,
    ));

    // 5. unary
    let bool_a = Rc::new(VariableSymbol::new("a".to_string(), "bool".to_string()));
    let unary_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "unary".to_string(),
            Some("bool".to_string()),
            vec![bool_a.clone()],
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
    let u32_a = Rc::new(VariableSymbol::new("a".to_string(), "u32".to_string()));
    let u32_b = Rc::new(VariableSymbol::new("b".to_string(), "u32".to_string()));
    let u32_c = Rc::new(VariableSymbol::new("c".to_string(), "u32".to_string()));
    let precedence_func = wrap(Function::new(
        Rc::new(FunctionSymbol::new(
            "precedence".to_string(),
            Some("u32".to_string()),
            vec![u32_a.clone(), u32_b.clone(), u32_c.clone()],
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
        Vec::new()
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
        Some("f32".to_string()),
        vec![],
    ));

    let body = wrap(Statement::Codeblock(CodeBlock::new(vec![wrap(
        Statement::Return(Return::new(Some(wrap(Expression::UnaryOp(Box::new(
            UnaryOp::<UntypedAST>::new(
                UnaryOpType::Negative,
                wrap(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new("f32".to_string())),
                    wrap(Expression::Literal("5".to_string())),
                )))),
            ),
        )))))),
    )])));

    let function = wrap(Function::new(main_symbol, body, Visibility::Private));
    let expected = ast::file::File::new("main".to_string(), Vec::new(), vec![function], Vec::new(),
                                        Vec::new());
    assert!(parsed.semantic_eq(&expected));
}

#[test]
fn test_missing_import_separator() {
    let (sm, id) = setup_source_map(MISSING_IMPORT_SEPARATOR);
    let to_parse = FileInformation::new(id, "test", &sm).unwrap();
    let parsed = parse(to_parse);
    assert!(parsed.is_none());
}
