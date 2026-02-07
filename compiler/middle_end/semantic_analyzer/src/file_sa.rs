use crate::symbol::syntax_element_map::SyntaxElementMap;
use ast::composite::{Struct, StructField};
use ast::file::File;
use ast::top_level::{Import, ImportRoot};
use ast::{ASTNode, TypedAST, UntypedAST};
use std::path::PathBuf;
use source::types::FileID;

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
    untyped_file: &ASTNode<File<UntypedAST>, FileID>,
    global_elements: &mut SyntaxElementMap,
) -> Result<ASTNode<File<TypedAST>, FileID>, String> {
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
