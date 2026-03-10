use crate::error::DriverError;
use crate::parser_driver::directory_builder::DirectoryBuilder;
use crate::parser_driver::module_path::{ModulePath, ModulePathProjectRelative};
use crate::program_information::ProgramInformation;
use ast::file::File;
use ast::{ASTNode, UntypedAST, AST};
use io::FullIO;
use parser::{parse, FileInformation};
use source::types::{FileID, Span};
use source::SourceMap;
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
    ) -> Result<Self, DriverError> {
        let mut to_ret = Self {
            root: DirectoryBuilder::new(from.name().to_owned(), PathBuf::new()),
            load_from,
            program_information: from,
        };
        let main_file_location = Self::extract_main_file_module(from)
            .ok_or_else(|| DriverError::MainFileNonUtf8Chars)?;
        let mut main_file_path = main_file_location
            .build_path_buf(from.projects())
            .ok_or_else(|| DriverError::MainFileProjectNotFound)?;
        let main_file_name = from
            .main_file()
            .iter()
            .next_back()
            .ok_or_else(|| DriverError::MainFilePathEmpty)?
            .to_str()
            .ok_or_else(|| DriverError::MainFileNonUtf8Chars)?;
        main_file_path.push(main_file_name);
        let main_file_id = to_ret.load_from.load_file(&main_file_path).map_err(|err| {
            DriverError::UnableToLoadFile {
                path: main_file_path,
                source: err,
            }
        })?;
        to_ret.add_file_handle_imports(&main_file_location, main_file_id)?;
        Ok(to_ret)
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
    ) -> Result<(), DriverError> {
        let imports_information = self.handle_file(file_location, to_add)?;
        imports_information
            .into_iter()
            .map(|path| self.handle_import(&path))
            .try_fold((), |_a, b| b)
    }

    /// Handles the addition of a file
    ///
    /// All modules referenced by it are returned
    ///
    /// # Parameters
    ///
    /// - **`file_location`** - The location of the file
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
    ) -> Result<Vec<ImportInformation>, DriverError> {
        let parsed = self.parse_file(file_location, to_add)?;
        let imports_information = ImportInformation::from_file(&parsed, file_location);
        self.add_file(file_location, parsed, to_add).unwrap();
        Ok(imports_information)
    }

    /// Adds a file to the AST
    ///
    /// # Parameter
    ///
    /// - **`file_location`** - Where to add the file
    /// - **`file`** - The file to add
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
    ) -> Result<File<UntypedAST>, DriverError> {
        // This can never panic as a ModulePath can never be empty
        let last = file_location.elements().pop().unwrap();
        let file_information = FileInformation::new(to_parse, &last, self.load_from).unwrap();
        let parsed = parse(&file_information)
            .map_err(<error::diagnostic::Diagnostic as Into<DriverError>>::into)?;
        Ok(parsed)
    }

    /// Handles an import
    ///
    /// More precisely, it loads all files imported by an import
    /// Their imports are then loaded recursively
    ///
    /// Should a file already exist, nothing is done
    ///
    /// # Parameter
    ///
    /// - **`module_path`** - Information about the import to handle
    ///
    /// # Errors
    ///
    /// There was an IO error
    ///     - This includes if `module_path` can't be resolved
    fn handle_import(&mut self, import_path: &ImportInformation) -> Result<(), DriverError> {
        let module_dir = import_path
            .path()
            .build_path_buf(self.program_information.projects())
            .ok_or_else(|| DriverError::UnresolvedImport {
                span: import_path.span(),
            })?;
        // Ensures that the module exists even if empty
        // See https://github.com/Randware/Wasome/issues/45 for more information
        self.root.ensure_module_exists(import_path.path());
        let imported_files: Vec<_> = self
            .list_wasome_files_in_dir(&module_dir)
            .map_err(|err| DriverError::UnableToLoadDirectory {
                path: module_dir.clone(),
                source: err,
            })?
            .collect();
        for file in imported_files {
            // Only load the file if it isn't loaded, yet
            // We can't use an entire module at once as the main file is loaded alone
            // All wasome files must have a file extension
            // So this will never panic
            if self.does_file_exist_in_ast(import_path.path(), &file[0..file.rfind('.').unwrap()]) {
                // We don't load the file, but there is no error
                return Ok(());
            }
            let loaded = self.load_file(module_dir.clone(), &file).map_err(|err| {
                DriverError::UnableToLoadFile {
                    path: module_dir.join(file),
                    source: err,
                }
            })?;
            self.add_file_handle_imports(import_path.path(), loaded)?;
        }
        Ok(())
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
    fn list_wasome_files_in_dir<'b>(
        &'b self,
        dir: &'b Path,
    ) -> Result<impl Iterator<Item = String> + 'b, Error> {
        Ok(self
            .load_from
            .loader()
            .list_files(self.program_information.path().join(dir))?
            // Skip files with non-UTF8 filenames
            // They might be non-wasome files so we don't want to hard-fail
            .filter_map(|file_name| file_name.into_string().ok())
            .filter(|file| {
                WASOME_FILE_ENDINGS
                    .iter()
                    .any(|ending| file.ends_with(ending))
            }))
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
    fn load_file(&mut self, mut module_path: PathBuf, file_name: &str) -> Result<FileID, Error> {
        module_path.push(file_name);
        self.load_file_combined_path(&module_path)
    }

    /// Like [`Self::load_file`], but the filepath is already combined
    fn load_file_combined_path(&mut self, module_path: &Path) -> Result<FileID, Error> {
        self.load_from.load_file(module_path)
    }
}

/// Information about an import
///
/// Used for loading imported files and handling errors that might occur during this process
///
/// May only exist in combination with a [`SourceMap`]
///     - This attachment only exists conceptually and is not represented in the data structure
struct ImportInformation {
    /// A path to the referenced Module
    /// This does **not** have to be valid and may reference a non-existent module
    path: ModulePath,
    /// Must be inside the attached [`SourceMap`]
    span: Span,
}

impl ImportInformation {
    /// Created a new instance of Self
    ///
    /// Note that no constraints are checked and that this is the responsibility of the caller
    const fn new(path: ModulePath, span: Span) -> Self {
        Self { path, span }
    }

    pub const fn path(&self) -> &ModulePath {
        &self.path
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    /// Extracts all module paths referenced by imports of a file
    ///
    /// # Parameters
    ///
    /// - **`file`** - The file to analyze
    /// - **`file_module`** - The module path of the file itself
    ///
    /// # Return
    ///
    /// A list of information of all imports inside the file
    ///     - They will be attached to the [`SourceMap`] `file` was loaded with.
    ///       See [`ImportInformation`] for more information about attachment.
    pub fn from_file(file: &File<UntypedAST>, file_module: &ModulePath) -> Vec<Self> {
        file.imports()
            .iter()
            .map(|import| {
                // A file may not have empty imports relative to the root
                // [`Self::from_import_information`] has this as its sole error condition
                // Therefore, this will never panic
                Self::new(
                    ModulePath::from_import_information(
                        file_module,
                        import.root(),
                        import.path().to_vec(),
                    )
                    .unwrap(),
                    *import.position(),
                )
            })
            .collect::<Vec<_>>()
    }
}
