use crate::symbol::syntax_element_map::SyntaxElementMap;
use ast::composite::{Struct, StructField};
use ast::file::File;
use ast::top_level::{Import, ImportRoot};
use ast::{ASTNode, TypedAST, UntypedAST};
use std::path::PathBuf;

/// Analyzes a single file and converts it into its typed representation.
///
/// This process involves three main steps:
/// 1. Constructing a file-specific context to handle imports and path resolutions.
/// 2. Iterating over all functions in the file and performing semantic analysis on them.
/// 3. Reconstructing the file node with typed imports and typed functions.
///
/// # Parameters
/// * `untyped_file` - The untyped file node to analyze (`&ASTNode<File<UntypedAST>, PathBuf>`).
/// * `global_elements` - The global registry of typed symbols (`&mut SyntaxElementMap`).
///
/// # Returns
/// * `Result<ASTNode<File<TypedAST>, PathBuf>, String>` - The typed file node on success, or an error string.
pub(crate) fn analyze_file(
    untyped_file: &ASTNode<File<UntypedAST>, PathBuf>,
    global_elements: &mut SyntaxElementMap,
) -> Result<ASTNode<File<TypedAST>, PathBuf>, String> {
    let mut typed_functions = Vec::new();
    for func in untyped_file.function_iterator() {
        global_elements
            .function_implementations_for_untyped_symbol(func.declaration())
            .into_iter()
            .flatten()
            .for_each(|func_impl| typed_functions.push(func_impl));
    }

    let mut typed_enums = Vec::new();
    for en in untyped_file.enum_iterator() {
        global_elements
            .enum_implementations_for_untyped_symbol(en.symbol())
            .into_iter()
            .flatten()
            .for_each(|en_impl| typed_enums.push(en_impl));
    }

    let mut typed_structs = Vec::new();
    for st in untyped_file.struct_iterator() {
        global_elements
            .struct_implementations_for_untyped_symbol(st.symbol())
            .into_iter()
            .flatten()
            .map(|st_impl| {
                ASTNode::new(
                    Struct::new(
                        st_impl.0.0,
                        st_impl.1.implementations().map(|(func, _)| func).collect(),
                        st_impl
                            .0
                            .1
                            .into_iter()
                            .zip(st.fields().iter())
                            .map(|(typed, untyped)| {
                                ASTNode::new(
                                    StructField::new(typed, untyped.visibility()),
                                    untyped.position().clone(),
                                )
                            })
                            .collect(),
                        st.visibility(),
                    ),
                    st.position().clone(),
                )
            })
            .for_each(|st_impl| typed_structs.push(st_impl));
    }

    let typed_imports: Vec<ASTNode<Import>> = untyped_file
        .imports()
        .iter()
        .map(|node| {
            let root = match node.root() {
                ImportRoot::CurrentModule => ImportRoot::CurrentModule,
                ImportRoot::Root => ImportRoot::Root,
            };

            let new_import = Import::new(root, node.path().clone(), node.usage_name_owned());

            ASTNode::new(new_import, node.position().clone())
        })
        .collect();

    let typed_file = File::new(
        untyped_file.name().to_string(),
        typed_imports,
        typed_functions,
        typed_enums,
        typed_structs,
    );

    Ok(ASTNode::new(typed_file, untyped_file.position().clone()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression_sa::sample_codearea;
    use ast::directory::Directory;
    use ast::statement::{CodeBlock, Statement};
    use ast::symbol::{FunctionSymbol, ModuleUsageNameSymbol};
    use ast::top_level::{Function, Import, ImportRoot};
    use ast::visibility::Visibility;
    use ast::{AST, ASTNode, UntypedAST};
    use std::collections::HashMap;
    use std::rc::Rc;

    fn create_dummy_func_parts(
        name: &str,
    ) -> (
        Rc<FunctionSymbol<UntypedAST>>,
        ASTNode<Function<UntypedAST>>,
    ) {
        let symbol = Rc::new(FunctionSymbol::new(
            name.to_string(),
            None,
            vec![],
            Vec::new(),
        ));
        let body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![])),
            sample_codearea(),
        );
        let func_node = ASTNode::new(
            Function::new(symbol.clone(), body, Visibility::Public),
            sample_codearea(),
        );
        (symbol, func_node)
    }

    fn create_import(path: Vec<&str>, alias: Option<&str>) -> ASTNode<Import> {
        let path_vec: Vec<String> = path.iter().map(|s| s.to_string()).collect();
        let effective_name = alias.unwrap_or_else(|| path.last().expect("Empty path"));

        let usage_symbol = Rc::new(ModuleUsageNameSymbol::new(effective_name.to_string()));

        let import = Import::new(ImportRoot::Root, path_vec, usage_symbol);
        ASTNode::new(import, sample_codearea())
    }

    struct DirNode {
        name: String,
        subdirs: HashMap<String, DirNode>,
        functions: Vec<ASTNode<Function<UntypedAST>>>,
    }

    impl DirNode {
        fn new(name: &str) -> Self {
            Self {
                name: name.to_string(),
                subdirs: HashMap::new(),
                functions: Vec::new(),
            }
        }

        fn insert(&mut self, path: &[&str], funcs: Vec<ASTNode<Function<UntypedAST>>>) {
            if let Some((first, rest)) = path.split_first() {
                let entry = self
                    .subdirs
                    .entry(first.to_string())
                    .or_insert_with(|| DirNode::new(first));

                if rest.is_empty() {
                    entry.functions.extend(funcs);
                } else {
                    // Recurse deeper
                    entry.insert(rest, funcs);
                }
            }
        }

        fn to_ast_node(self, parent_path: PathBuf) -> ASTNode<Directory<UntypedAST>, PathBuf> {
            let my_path = parent_path.join(&self.name);

            let converted_subdirs: Vec<_> = self
                .subdirs
                .into_values()
                .map(|node| node.to_ast_node(my_path.clone()))
                .collect();

            let dummy_file = ASTNode::new(
                File::new(
                    "mod.wa".to_string(),
                    vec![],
                    self.functions,
                    Vec::new(),
                    Vec::new(),
                ),
                my_path.join("mod.wa"),
            );

            ASTNode::new(
                Directory::new(self.name, converted_subdirs, vec![dummy_file]),
                my_path,
            )
        }
    }

    fn create_valid_ast(
        main_filename: &str,
        imports: Vec<ASTNode<Import>>,
        functions: Vec<ASTNode<Function<UntypedAST>>>,
        dependencies: Vec<(Vec<&str>, Vec<ASTNode<Function<UntypedAST>>>)>,
    ) -> AST<UntypedAST> {
        let main_file = ASTNode::new(
            File::new(
                main_filename.to_string(),
                imports,
                functions,
                Vec::new(),
                Vec::new(),
            ),
            PathBuf::from(format!("root/{}", main_filename)),
        );

        let mut root_nodes: HashMap<String, DirNode> = HashMap::new();
        for (path, funcs) in dependencies {
            if let Some((first, rest)) = path.split_first() {
                let entry = root_nodes
                    .entry(first.to_string())
                    .or_insert_with(|| DirNode::new(first));

                if rest.is_empty() {
                    entry.functions.extend(funcs);
                } else {
                    entry.insert(rest, funcs);
                }
            }
        }

        let subdirs: Vec<_> = root_nodes
            .into_values()
            .map(|n| n.to_ast_node(PathBuf::from("root")))
            .collect();

        let root = ASTNode::new(
            Directory::new("root".to_string(), subdirs, vec![main_file]),
            PathBuf::from("root"),
        );

        AST::new(root).unwrap()
    }

    /*fn register_symbol_direct(map: &mut GlobalSymbolMap, untyped: &Rc<FunctionSymbol<UntypedAST>>) {
        let typed = Rc::new(FunctionSymbol::<TypedAST>::new(
            untyped.name().to_string(),
            None,
            vec![],
        ));
        map.insert(untyped.as_ref().clone(), typed);
    }

    /// Tests successful analysis of a simple file containing a single function.
    ///
    /// It verifies that the resulting typed file retains the structure and content of the original.
    #[test]
    fn analyze_file_simple_ok() {
        let (main_sym, func_node) = create_dummy_func_parts("main");
        let ast = create_valid_ast("main.wa", vec![], vec![func_node], vec![]);

        let mut global_map = GlobalSymbolMap::new();
        register_symbol_direct(&mut global_map, &main_sym);
        let lookup_map = GlobalFunctionMap::new();

        let root_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_helper = root_helper.index_file(0).unwrap();

        let result = analyze_file(&file_helper, &lookup_map, &global_map);

        assert!(result.is_ok());
        let typed_file = result.unwrap();

        assert_eq!(typed_file.name(), "main.wa");
        assert_eq!(typed_file.functions().len(), 1);
    }

    /// Tests that import aliases are correctly resolved during semantic analysis.
    ///
    /// It simulates a function call `math.cos` and verifies that the `FileContext` properly
    /// resolves `math` to `std::math`. Crucially, it populates the `std::math` module with
    /// a `cos` function so that the analyzer can actually find the symbol.
    #[test]
    fn analyze_file_resolves_imports() {
        let imported_func_name = "cos";
        let imported_full_path = "std::math::cos";

        let untyped_cos_sym = Rc::new(FunctionSymbol::<UntypedAST>::new(
            imported_func_name.to_string(),
            None,
            vec![],
        ));

        let typed_cos_sym = Rc::new(FunctionSymbol::<TypedAST>::new(
            imported_func_name.to_string(),
            None,
            vec![],
        ));

        let mut global_map = GlobalSymbolMap::new();
        global_map.insert(untyped_cos_sym.as_ref().clone(), typed_cos_sym.clone());

        let mut lookup_map = GlobalFunctionMap::new();
        lookup_map.insert(imported_full_path.to_string(), typed_cos_sym);

        let body = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![])),
            sample_codearea(),
        );
        let cos_func_node = ASTNode::new(
            Function::new(untyped_cos_sym.clone(), body, Visibility::Public),
            sample_codearea(),
        );

        let import_node = create_import(vec!["std", "math"], None);

        let call_expr = FunctionCall::<UntypedAST>::new("math.cos".to_string(), vec![]);

        let stmt_node = ASTNode::new(Statement::VoidFunctionCall(call_expr), sample_codearea());
        let body_node = ASTNode::new(
            Statement::Codeblock(CodeBlock::new(vec![stmt_node])),
            sample_codearea(),
        );

        let main_sym = Rc::new(FunctionSymbol::new("main".to_string(), None, vec![]));
        let main_func = ASTNode::new(
            Function::new(main_sym.clone(), body_node, Visibility::Public),
            sample_codearea(),
        );

        let typed_main = Rc::new(FunctionSymbol::<TypedAST>::new(
            "main".to_string(),
            None,
            vec![],
        ));
        global_map.insert(main_sym.as_ref().clone(), typed_main);

        let ast = create_valid_ast(
            "main.wa",
            vec![import_node],
            vec![main_func],
            vec![(vec!["std", "math"], vec![cos_func_node])],
        );

        let root_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_helper = root_helper.index_file(0).unwrap();

        let result = analyze_file(&file_helper, &lookup_map, &global_map);

        assert!(result.is_ok(), "Analysis failed: {:?}", result.err());
    }

    /// Tests that the analysis process preserves imports in the final TypedAST.
    ///
    /// It ensures that both explicit aliases (`other::pkg` as `op`) and implicit ones (`std::io`)
    /// are correctly reconstructed in the output.
    #[test]
    fn analyze_file_preserves_imports() {
        let import1 = create_import(vec!["std", "io"], None);
        let import2 = create_import(vec!["other", "pkg"], Some("op"));

        let (test_sym, func_node) = create_dummy_func_parts("test");

        let mut global_map = GlobalSymbolMap::new();
        register_symbol_direct(&mut global_map, &test_sym);
        let lookup_map = GlobalFunctionMap::new();

        let ast = create_valid_ast(
            "test.wa",
            vec![import1, import2],
            vec![func_node],
            vec![(vec!["std", "io"], vec![]), (vec!["other", "pkg"], vec![])],
        );

        let root_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let file_helper = root_helper.index_file(0).unwrap();

        let result = analyze_file(&file_helper, &lookup_map, &global_map).unwrap();

        let imports = result.imports();
        assert_eq!(imports.len(), 2);

        assert_eq!(
            imports[0].path(),
            &vec!["std".to_string(), "io".to_string()]
        );
        assert_eq!(imports[0].usage_name().name(), "io");

        assert_eq!(
            imports[1].path(),
            &vec!["other".to_string(), "pkg".to_string()]
        );
        assert_eq!(imports[1].usage_name().name(), "op");
    }*/
}
