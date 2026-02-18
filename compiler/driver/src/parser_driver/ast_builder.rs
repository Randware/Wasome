use crate::parser_driver::directory_builder::DirectoryBuilder;
use crate::parser_driver::module_path::{ModulePath, ModulePathProjectRelative};
use crate::program_information::ProgramInformation;
use crate::{
    INVALID_CHARS_IN_MAIN_FILE, MAIN_FILE_PATH_EMPTY, MAIN_FILE_PROJECT_NOT_FOUND,
    UNABLE_TO_LOAD_DIRECTORY, UNABLE_TO_LOAD_FILE, UNRESOLVED_IMPORT_ERROR,
};
use ast::file::File;
use ast::{AST, ASTNode, UntypedAST};
use error::diagnostic::{Diagnostic, Snippet};
use io::FullIO;
use parser::{FileInformation, parse};
use source::SourceMap;
use source::types::{FileID, Span};
use std::io::Error;
use std::path::{Path, PathBuf};

/// All valid wasome file extensions
const WASOME_FILE_ENDINGS: &[&str] = &[".waso", ".✨"];

/// Builds an entire untyped AST
#[derive(Debug)]
pub struct ASTBuilder<'a, Loader: FullIO> {
    root: DirectoryBuilder,
    load_from: &'a mut SourceMap<Loader>,
    program_information: &'a ProgramInformation,
}

impl<'a, Loader: FullIO> ASTBuilder<'a, Loader> {
    /// Creates a new `ASTBuilder`
    ///
    /// The resulting `ASTBuilder` will be ready to be [`Self::build`]
    /// # Parameters
    ///
    /// - **from** - Information about the program being compiled
    /// - **`load_from`** - Where the data should be loaded from
    ///
    /// # Return
    ///
    /// - **Err** - If there was an error during building
    /// - **Ok(Self)** - Otherwise
    pub fn new(
        from: &'a ProgramInformation,
        load_from: &'a mut SourceMap<Loader>,
    ) -> Result<Self, Diagnostic> {
        let mut to_ret = Self {
            root: DirectoryBuilder::new(from.name().to_owned(), PathBuf::new()),
            load_from,
            program_information: from,
        };
        let main_file_location = Self::extract_main_file_module(from)
            .ok_or_else(Self::main_file_non_utf8_chars_error)?;
        let mut main_file_path = main_file_location
            .build_path_buf(from.projects())
            .ok_or_else(Self::main_file_project_not_found_error)?;
        let main_file_name = from
            .main_file()
            .iter()
            .next_back()
            .ok_or_else(Self::main_file_path_empty_error)?
            .to_str()
            .ok_or_else(Self::main_file_non_utf8_chars_error)?;
        main_file_path.push(main_file_name);
        let main_file_id = to_ret
            .load_from
            .load_file(&main_file_path)
            .map_err(|err| Self::unable_to_load_file_error(&main_file_path, &err))?;
        to_ret.add_file_handle_imports(&main_file_location, main_file_id)?;
        Ok(to_ret)
    }

    fn main_file_non_utf8_chars_error() -> Diagnostic {
        Diagnostic::builder()
            .message("The main file path may not contain non-UTF8 chars")
            .code(INVALID_CHARS_IN_MAIN_FILE)
            .help("Only use valid UTF-8 characters")
            .build()
    }

    fn main_file_project_not_found_error() -> Diagnostic {
        Diagnostic::builder()
            .message("The project of the main file could not be found")
            .code(MAIN_FILE_PROJECT_NOT_FOUND)
            .help("Provide a valid main file path")
            .build()
    }

    fn main_file_path_empty_error() -> Diagnostic {
        Diagnostic::builder()
            .message("The path of the main file is empty")
            .code(MAIN_FILE_PATH_EMPTY)
            .help("Provide a valid main file path")
            .build()
    }

    fn unable_to_load_file_error(file_path: &Path, error: &Error) -> Diagnostic {
        Diagnostic::builder()
            .message(format!(
                "Unable to load file {}: {}",
                file_path.to_string_lossy(),
                error
            ))
            .code(UNABLE_TO_LOAD_FILE)
            // This is a generic error msg
            // So giving help is not easily possible
            .build()
    }

    fn unable_to_load_directory_error(directory_path: &Path, error: &Error) -> Diagnostic {
        Diagnostic::builder()
            .message(format!(
                "Unable to load directory {}: {}",
                directory_path.to_string_lossy(),
                error
            ))
            .code(UNABLE_TO_LOAD_DIRECTORY)
            // This is a generic error msg
            // So giving help is not easily possible
            .build()
    }

    fn unresolved_import_error(pos: Span) -> Diagnostic {
        Diagnostic::builder()
            .message("Unable to resolve import")
            .code(UNRESOLVED_IMPORT_ERROR)
            .snippet(
                Snippet::builder()
                    .file(pos.file_id)
                    .primary(pos.start..pos.end, "Unable to find the referenced file")
                    .build(),
            )
            .build()
    }

    /// Turns this into an actual untyped AST
    ///
    /// # Return
    ///
    /// The untyped AST
    pub fn build(self) -> AST<UntypedAST> {
        // This will never panic as all imports must be valid as we add all imports and error if they
        // are invalid
        AST::new(self.root.build()).unwrap()
    }

    /// Extracts the module
    ///
    /// # Parameter
    ///
    /// - **from** - From where to extract the path
    ///
    /// # Return
    ///
    /// The main file path.
    /// Its project component can potentially not be included in `from.projects()`
    ///
    /// # Errors
    ///
    /// The main file path contains non-UTF8 characters
    ///
    /// All errors are represented by a return of `None`
    fn extract_main_file_module(from: &ProgramInformation) -> Option<ModulePath> {
        let mut main_file_location = from
            .main_file()
            .iter()
            .map(|file_name| file_name.to_os_string().into_string().ok())
            .collect::<Option<Vec<_>>>()?;
        // Don't include the filename
        // This will never panic as the main file of [`ProgramInformation`] will never be empty
        main_file_location.pop().unwrap();
        let main_file_location = ModulePath::new(
            ModulePathProjectRelative::new(main_file_location),
            from.main_project().to_owned(),
        );
        Some(main_file_location)
    }

    /// Handles the addition of a file
    ///
    /// All imports are handled recursively
    ///
    /// # Parameters
    ///
    /// - **`file_location`** - The location of the file
    /// - **`to_add`** - The file to handle
    /// - **`file_name`** - The filename on disk (e.g.: with file extension)
    ///
    /// # Errors
    ///
    /// - There is a syntax error
    /// - `file_location` is empty
    fn add_file_handle_imports(
        &mut self,
        file_location: &ModulePath,
        to_add: FileID,
    ) -> Result<(), Diagnostic> {
        let imports_information = self.handle_file(file_location, to_add)?;
        imports_information
            .into_iter()
            .map(|path| self.handle_import((&path.0, path.1)))
            .try_fold((), |_a, b| b)
    }

    /// Handles the addition of a file
    ///
    /// All modules referenced by it are returned
    ///
    /// # Parameters
    ///
    /// - **`file_location`** - The location of the file
    /// - **file** - The file to handle
    /// - **`file_name`** - The filename on disk (e.g.: with file extension)
    ///
    /// # Returns
    ///
    /// All modules imported by the file
    ///
    /// # Errors
    ///
    /// - There is a syntax error in the file
    /// - `file_location` is empty
    ///
    /// # Panics
    ///
    /// - The file already exists
    fn handle_file(
        &mut self,
        file_location: &ModulePath,
        to_add: FileID,
    ) -> Result<Vec<(ModulePath, Span)>, Diagnostic> {
        let parsed = self.parse_file(file_location, to_add)?;
        let imports_information = ModulePath::from_file(&parsed, file_location);
        self.add_file(file_location, parsed, to_add).unwrap();
        Ok(imports_information)
    }

    /// Adds a file to the AST
    ///
    /// # Parameter
    ///
    /// - **`file_location`** - Where to add the file
    /// - **file** - The file to add
    /// - **`file_name`** - The filename on disk (e.g.: with file extension)
    ///
    /// # Errors
    ///
    /// If file already exists
    ///
    /// All errors are represented by a return of `None`
    fn add_file(
        &mut self,
        file_location: &ModulePath,
        file: File<UntypedAST>,
        file_id: FileID,
    ) -> Option<()> {
        self.root
            .add_file(ASTNode::new(file, file_id), &file_location.elements())?;
        Some(())
    }

    /// Parses a file
    ///
    /// The behavior is arbitrary, including possible panics but no UB, if `to_parse` does not
    /// originate from the [`SourceMap`] of self
    ///
    /// # Parameters
    ///
    /// - **`file_location`** - The location of the file. May not be empty
    /// - **`to_parse`**: The file, provided as id in the [`SourceMap`] of self
    ///
    /// # Return
    ///
    /// The parsed file
    ///
    /// # Errors
    ///
    /// - The file contains syntax errors
    ///
    /// # Panics
    ///
    /// - **`FileID`** is not in the [`SourceMap`] of self
    fn parse_file(
        &self,
        file_location: &ModulePath,
        to_parse: FileID,
    ) -> Result<File<UntypedAST>, Diagnostic> {
        // This can never panic as a ModulePath can never be empty
        let last = file_location.elements().pop().unwrap();
        let file_information = FileInformation::new(to_parse, &last, self.load_from).unwrap();
        let parsed = parse(file_information)?;
        Ok(parsed)
    }

    /// Handles an import
    ///
    /// More precisely, it loads all files imported by an import
    /// Their imports are then loaded recursively
    ///
    /// Should a file already exist, nothing is done
    ///
    /// The import is only provided via it's import path
    ///
    /// # Parameter
    ///
    /// - **`module_path`** - The import path
    ///
    /// # Errors
    ///
    /// There was an IO error
    ///     - This includes if `module_path` can't be resolved
    ///
    /// All errors are represented by a return of `None`
    fn handle_import(&mut self, import_path: (&ModulePath, Span)) -> Result<(), Diagnostic> {
        let module_dir = import_path
            .0
            .build_path_buf(self.program_information.projects())
            .ok_or_else(|| Self::unresolved_import_error(import_path.1))?;

        let imported_files = self
            .list_wasome_files_in_dir(&module_dir)
            .map_err(|err| Self::unable_to_load_directory_error(&module_dir, &err))?;
        let mut err = None;
        imported_files.for_each(|file| {
            // Only load the file if it isn't loaded, yet
            // We can't use an entire module at once as the main file is loaded alone
            // All wasome files must have a file extension
            // So this will never panic
            if self.does_file_exist_in_ast(import_path.0, &file[0..file.rfind('.').unwrap()]) {
                // We don't load the file, but there is no error
                return;
            }
            let loaded = match self.load_file(module_dir.clone(), &file) {
                Ok(val) => val,
                Err(io_err) => {
                    err = Some(Self::unable_to_load_file_error(
                        &module_dir.join(file),
                        &io_err,
                    ));
                    return;
                }
            };
            if let Err(val) = self.add_file_handle_imports(import_path.0, loaded) {
                err = Some(val);
            }
        });
        err.map_or(Ok(()), |err| Err(err))
    }

    /// Checks if a file exists in the AST
    ///
    /// The file is provided via a path to the module and the name
    ///
    /// # Parameters
    ///
    /// - **`module_path`** - The path of the module the file belongs to
    ///     - Relative to the root of the [`SourceMap`]
    /// - **`file_name`** - The name of the file
    ///     - Excluding the file extension
    ///
    /// # Return
    ///
    /// Does the file exist?
    fn does_file_exist_in_ast(&self, path: &ModulePath, filename: &str) -> bool {
        self.root
            .file_by_path_name(&path.elements(), filename)
            .is_some()
    }

    /// Lists all Wasome source files in the provided directory
    ///
    /// # Parameters
    ///
    /// - **`module_path`** - The path of the module the file belongs to
    ///     - Relative to the root of the [`SourceMap`]
    ///
    /// # Return
    ///
    /// An iterator over the filenames of all wasome files in the provided directory
    ///     - Including file extensions
    ///
    /// # Errors
    ///
    /// There was an IO error, for example
    /// - Missing permissions
    /// - Directory not found
    ///
    /// All errors are represented by a return of `None`
    fn list_wasome_files_in_dir(
        &self,
        dir: &Path,
    ) -> Result<impl Iterator<Item = String> + 'static, Error> {
        Ok(
            Loader::list_files(self.program_information.path().join(dir))?
                // Skip files with non-UTF8 filenames
                // They might be non-wasome files so we don't want to hard-fail
                .filter_map(|file_name| file_name.into_string().ok())
                .filter(|file| {
                    WASOME_FILE_ENDINGS
                        .iter()
                        .any(|ending| file.ends_with(ending))
                }),
        )
    }

    /// Loads a file into the `SourceMap` and returns the `FileID` handle to it
    ///
    /// The file is specified via a module path and the file name. They are concatenated to get the
    /// final path
    ///
    /// # Parameters
    ///
    /// - **`module_path`** - The path of the module the file belongs to
    ///     - Relative to the root of the [`SourceMap`]
    /// - **`file_name`** - The name of the file
    ///     - Including the file extension
    ///
    /// # Return
    ///
    /// A handle to the loaded file
    ///
    /// # Errors
    ///
    /// The file could not be loaded, for example due to nonexistence
    ///
    /// All errors are represented by a return of `None`
    fn load_file(&mut self, mut module_path: PathBuf, file_name: &str) -> Result<FileID, Error> {
        module_path.push(file_name);
        self.load_file_combined_path(&module_path)
    }

    /// Like [`Self::load_file`], but the filepath is already combined
    fn load_file_combined_path(&mut self, module_path: &Path) -> Result<FileID, Error> {
        self.load_from.load_file(module_path)
    }
}
