use crate::error_sa::SemanticError;
use crate::file_sa::analyze_file;
use crate::symbol::syntax_element_map::SyntaxElementMap;
use ast::directory::Directory;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::path::PathBuf;

/// Recursively analyzes a directory and its contents.
///
/// It traverses all subdirectories and files contained within the directory,
/// aggregating the typed results into a new `Directory` node.
///
/// # Parameters
/// * `untyped_directory` - The untyped directory node to analyze (`&ASTNode<Directory<UntypedAST>, PathBuf>`).
/// * `global_elements` - The global registry of typed symbols (`&mut SyntaxElementMap`).
///
/// # Returns
/// * `Result<ASTNode<Directory<TypedAST>, PathBuf>, SemanticError>` - The typed directory node on success, or a semantic error.
pub(crate) fn analyze_directory(
    untyped_directory: &ASTNode<Directory<UntypedAST>, PathBuf>,
    global_elements: &mut SyntaxElementMap,
) -> Result<ASTNode<Directory<TypedAST>, PathBuf>, SemanticError> {
    let mut typed_subdirs = Vec::new();
    let mut typed_files = Vec::new();

    for subdir in untyped_directory.subdirectories_iterator() {
        let typed_subdir = analyze_directory(subdir, global_elements)?;
        typed_subdirs.push(typed_subdir);
    }

    for file in untyped_directory.files_iterator() {
        let typed_file = analyze_file(file, global_elements)?;
        typed_files.push(typed_file);
    }

    let typed_dir = Directory::new(
        untyped_directory.name().to_string(),
        typed_subdirs,
        typed_files,
    );

    Ok(ASTNode::new(
        typed_dir,
        untyped_directory.position().clone(),
    ))
}
