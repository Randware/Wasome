use crate::error_sa::SemanticError;
use crate::symbol::syntax_element_map::SyntaxElementMap;
use ast::UntypedAST;
use ast::symbol::SymbolWithTypeParameter;
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::file_traversal::FileTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use typed_arena::Arena;

/// Entry Point: Collects all global symbols from the AST.
///
/// This function initiates the traversal starting from the root directory of the AST.
/// It collects all function signatures, converts them to their typed counterparts,
/// and stores them in a global map for later retrieval during semantic analysis.
///
/// # Parameters
/// * `ast` - The entire Untyped Abstract Syntax Tree.
///
/// # Returns
/// * `Ok(GlobalSymbolMap)` - The populated map of symbols.
/// * `Err(SemanticError)` - If a semantic error occurs during type conversion (e.g., unknown types or duplicate definitions).
pub fn collect_global_symbols<'a>(
    dir: &'a DirectoryTraversalHelper<'a, 'a, UntypedAST>,
    to_alloc_in: &'a TraversalHelpers<'a>,
) -> Result<SyntaxElementMap<'a>, SemanticError> {
    let mut map = SyntaxElementMap::new();
    collect_from_directory(dir, &mut map, to_alloc_in)?;
    Ok(map)
}

/// Recursive helper function that traverses directories and files.
///
/// It iterates through all files in the current directory to register their functions
/// and then recursively calls itself for all subdirectories.
///
/// # Parameters
/// * `dir` - The current directory to traverse.
/// * `map` - The mutable reference to the global symbol map being populated.
fn collect_from_directory<'a>(
    dir: &'a DirectoryTraversalHelper<'a, 'a, UntypedAST>,
    map: &mut SyntaxElementMap<'a>,
    to_alloc_in: &'a TraversalHelpers<'a>,
) -> Result<(), SemanticError> {
    for file in dir.files_iterator() {
        let file = to_alloc_in.files.alloc(file);

        for function in file.function_iterator() {
            let function: &'a _ = to_alloc_in.functions.alloc(function);
            if map.insert_untyped_function(function).is_none() {
                return Err(SemanticError::AlreadyDeclared {
                    name: function.inner().declaration().name().to_string(),
                    kind: "Function".to_string(),
                    span: *function.inner().position(),
                });
            }
        }

        for en in file.enums_iterator() {
            let en: &'a _ = to_alloc_in.enums.alloc(en);
            if map.insert_untyped_enum(en).is_none() {
                return Err(SemanticError::AlreadyDeclared {
                    name: en.inner().symbol().name().to_string(),
                    kind: "Enum".to_string(),
                    span: *en.inner().position(),
                });
            }
        }

        for st in file.structs_iterator() {
            let st: &'a _ = to_alloc_in.structs.alloc(st);
            if map.insert_untyped_struct(st).is_none() {
                return Err(SemanticError::AlreadyDeclared {
                    name: st.inner().symbol().name().to_string(),
                    kind: "Struct".to_string(),
                    span: *st.inner().position(),
                });
            }
        }
    }

    for subdir in dir.subdirectories_iterator() {
        let subdir = to_alloc_in.directories.alloc(subdir);

        collect_from_directory(subdir, map, to_alloc_in)?;
    }

    Ok(())
}

/// A place where traversal helpers are stored
///
/// Arenas are used as they all must have the same lifetime for the [`SyntaxMap`]
pub(crate) struct TraversalHelpers<'a> {
    pub directories: Arena<DirectoryTraversalHelper<'a, 'a, UntypedAST>>,
    pub files: Arena<FileTraversalHelper<'a, 'a, UntypedAST>>,
    pub functions: Arena<FunctionTraversalHelper<'a, 'a, UntypedAST>>,
    pub enums: Arena<EnumTraversalHelper<'a, 'a, UntypedAST>>,
    pub structs: Arena<StructTraversalHelper<'a, 'a, UntypedAST>>,
}

impl<'a> TraversalHelpers<'a> {
    pub fn new() -> Self {
        Self {
            directories: Arena::new(),
            files: Arena::new(),
            functions: Arena::new(),
            enums: Arena::new(),
            structs: Arena::new(),
        }
    }
}
