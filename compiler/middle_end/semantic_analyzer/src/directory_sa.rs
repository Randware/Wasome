use crate::file_sa::analyze_file;
use crate::symbol_translation::global_system_collector::GlobalSymbolMap;
use ast::directory::Directory;
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::path::PathBuf;

/// Recursively analyzes a directory and its contents.
///
/// It traverses all subdirectories and files contained within the directory,
/// aggregating the typed results into a new `Directory` node.
///
/// # Parameters
/// * `dir_helper` - Traversal helper for the current directory (`&DirectoryTraversalHelper<UntypedAST>`).
/// * `lookup_map` - Map used by `FileSymbolMapper` for string-based symbol lookups (`&GlobalFunctionMap`).
/// * `global_map` - The global registry of typed symbols (`&GlobalSymbolMap`).
///
/// # Returns
/// * `Result<ASTNode<Directory<TypedAST>, PathBuf>, String>` - The typed directory node on success, or an error string.
pub(crate) fn analyze_directory(
    dir_helper: &DirectoryTraversalHelper<UntypedAST>,
    global_map: &GlobalSymbolMap,
) -> Result<ASTNode<Directory<TypedAST>, PathBuf>, String> {
    let mut typed_subdirs = Vec::new();
    let mut typed_files = Vec::new();

    for i in 0..dir_helper.len_subdirectories() {
        let subdir_helper = dir_helper
            .index_subdirectory(i)
            .expect("Index within bounds");
        let typed_subdir = analyze_directory(&subdir_helper, global_map)?;
        typed_subdirs.push(typed_subdir);
    }

    for i in 0..dir_helper.len_files() {
        let file_helper = dir_helper.index_file(i).expect("Index within bounds");
        let typed_file = analyze_file(&file_helper, global_map)?;
        typed_files.push(typed_file);
    }

    let untyped_dir = dir_helper.inner();
    let typed_dir = Directory::new(untyped_dir.name().to_string(), typed_subdirs, typed_files);

    Ok(ASTNode::new(
        typed_dir,
        dir_helper.inner().position().clone(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression_sa::sample_codearea;
    use ast::file::File;
    use ast::statement::{CodeBlock, Statement};
    use ast::symbol::FunctionSymbol;
    use ast::top_level::Function;
    use ast::visibility::Visibility;
    use ast::{AST, ASTNode, UntypedAST};
    use std::rc::Rc;

    /// Helper to create a dummy function symbol and node.
    fn create_dummy_function(
        name: &str,
    ) -> (
        Rc<FunctionSymbol<UntypedAST>>,
        ASTNode<Function<UntypedAST>>,
    ) {
        let symbol = Rc::new(FunctionSymbol::new(name.to_string(), None, vec![]));
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

    /*/// Helper to register a function in the maps (simulating Stage 2 results).
    fn register_function(
        name: &str,
        untyped_sym: Rc<FunctionSymbol<UntypedAST>>,
        global_map: &mut GlobalSymbolMap,
        lookup_map: &mut GlobalFunctionMap,
    ) {
        let typed_sym = Rc::new(FunctionSymbol::<TypedAST>::new(
            name.to_string(),
            None,
            vec![],
        ));
        global_map.insert(untyped_sym.as_ref().clone(), typed_sym.clone());
        lookup_map.insert(name.to_string(), typed_sym);
    }

    /// Tests that an empty directory is analyzed successfully.
    /// Result should be a typed directory with no files or subdirectories.
    #[test]
    fn analyze_empty_directory_ok() {
        let untyped_dir = Directory::new("root".to_string(), vec![], vec![]);
        let dir_node = ASTNode::new(untyped_dir, PathBuf::from("root"));
        let ast = AST::new(dir_node).unwrap();

        let root_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let global_map = GlobalSymbolMap::new();
        let lookup_map = GlobalFunctionMap::new();

        let result = analyze_directory(&root_helper, &lookup_map, &global_map);

        assert!(result.is_ok(), "Empty directory should be valid");
        let typed_node = result.unwrap();

        assert_eq!(typed_node.name(), "root");
        assert_eq!(typed_node.files().len(), 0);
        assert_eq!(typed_node.subdirectories().len(), 0);
    }

    /// Tests that a directory containing files is analyzed correctly.
    /// Ensures that `analyze_file` is called and results are aggregated.
    #[test]
    fn analyze_directory_with_files_ok() {
        let (sym_main, func_main) = create_dummy_function("main");

        let file = ASTNode::new(
            File::new("main.wa".to_string(), vec![], vec![func_main]),
            PathBuf::from("root/main.wa"),
        );

        let dir = ASTNode::new(
            Directory::new("root".to_string(), vec![], vec![file]),
            PathBuf::from("root"),
        );
        let ast = AST::new(dir).unwrap();

        let mut global_map = GlobalSymbolMap::new();
        let mut lookup_map = GlobalFunctionMap::new();
        register_function("main", sym_main, &mut global_map, &mut lookup_map);

        let root_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let result = analyze_directory(&root_helper, &lookup_map, &global_map);

        assert!(result.is_ok());
        let typed_dir = result.unwrap();

        assert_eq!(typed_dir.files().len(), 1);
        assert_eq!(typed_dir.files()[0].name(), "main.wa");
    }

    /// Tests recursive analysis: Root -> Subdir -> File.
    /// Ensures that subdirectories are traversed deeply.
    #[test]
    fn analyze_recursive_structure_ok() {
        let (sym_add, func_add) = create_dummy_function("add");

        let file_math = ASTNode::new(
            File::new("math.wa".to_string(), vec![], vec![func_add]),
            PathBuf::from("root/utils/math.wa"),
        );

        let subdir_utils = ASTNode::new(
            Directory::new("utils".to_string(), vec![], vec![file_math]),
            PathBuf::from("root/utils"),
        );

        let root_dir = ASTNode::new(
            Directory::new("root".to_string(), vec![subdir_utils], vec![]),
            PathBuf::from("root"),
        );

        let ast = AST::new(root_dir).unwrap();

        let mut global_map = GlobalSymbolMap::new();
        let mut lookup_map = GlobalFunctionMap::new();
        register_function("add", sym_add, &mut global_map, &mut lookup_map);

        let root_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let result = analyze_directory(&root_helper, &lookup_map, &global_map);

        assert!(result.is_ok());
        let typed_root = result.unwrap();

        assert_eq!(typed_root.subdirectories().len(), 1);

        let typed_subdir = &typed_root.subdirectories()[0];
        assert_eq!(typed_subdir.name(), "utils");
        assert_eq!(typed_subdir.files().len(), 1);
        assert_eq!(typed_subdir.files()[0].functions().len(), 1);
    }

    /// Tests error propagation.
    /// If `analyze_file` fails (e.g., missing symbol in map), `analyze_directory` should fail.
    #[test]
    fn analyze_directory_propagates_file_error() {
        let (sym_unknown, func_unknown) = create_dummy_function("unknown");

        let file = ASTNode::new(
            File::new("broken.wa".to_string(), vec![], vec![func_unknown]),
            PathBuf::from("root/broken.wa"),
        );

        let dir = ASTNode::new(
            Directory::new("root".to_string(), vec![], vec![file]),
            PathBuf::from("root"),
        );
        let ast = AST::new(dir).unwrap();

        let global_map = GlobalSymbolMap::new();
        let lookup_map = GlobalFunctionMap::new();

        let root_helper = DirectoryTraversalHelper::new_from_ast(&ast);
        let result = analyze_directory(&root_helper, &lookup_map, &global_map);

        assert!(
            result.is_err(),
            "Should fail because function symbol is missing in maps"
        );
    }*/
}
