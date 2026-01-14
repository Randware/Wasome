use crate::directory_builder::DirectoryBuilder;
use crate::module_path::{ModulePath, ModulePathProjectRelative};
use ast::file::File;
use ast::{AST, ASTNode, UntypedAST};
use io::FullIO;
use parser::{FileInformation, parse};
use shared::program_information::ProgramInformation;
use source::SourceMap;
use source::types::FileID;
use std::path::PathBuf;

/// All valid wasome file extensions
const WASOME_FILE_ENDINGS: &'static [&'static str] = &[".waso", ".✨"];

/// Builds an entire untyped AST
#[derive(Debug)]
pub(crate) struct ASTBuilder<'a, Loader: FullIO> {
    root: DirectoryBuilder,
    load_from: &'a mut SourceMap<Loader>,
    program_information: &'a ProgramInformation,
}

impl<'a, Loader: FullIO> ASTBuilder<'a, Loader> {
    /// Creates a new ASTBuilder
    ///
    /// The resulting ASTBuilder will be ready to be [`Self::build`]t
    /// # Parameters
    ///
    /// - **from** - Information about the program being compiled
    /// - **load_from** - Where the data should be loaded from
    ///
    /// # Return
    ///
    /// - **None** - If there was an error during building
    /// - **Some(Self)** - Otherwise
    pub fn new(from: &'a ProgramInformation, load_from: &'a mut SourceMap<Loader>) -> Option<Self> {
        let mut to_ret = Self {
            root: DirectoryBuilder::new(from.name().to_owned(), from.path().to_owned()),
            load_from,
            program_information: from,
        };
        let main_file_location = Self::extract_main_file(from)?;
        let main_file_id = to_ret.load_from.load_file(from.path()).ok()?;
        to_ret.add_file_handle_imports(&main_file_location, main_file_id)?;
        Some(to_ret)
    }

    fn extract_main_file(from: &ProgramInformation) -> Option<ModulePath> {
        let mut main_file_location = from
            .main_file()
            .iter()
            .map(|file_name| file_name.to_os_string().into_string().ok())
            .collect::<Option<Vec<_>>>()?;
        main_file_location.pop();
        let main_file_location = ModulePath::new(
            ModulePathProjectRelative::new(main_file_location),
            from.main_project().to_owned(),
        );
        Some(main_file_location)
    }

    pub fn build(self) -> AST<UntypedAST> {
        // This will never panic as all imports must be valid as we add all imports and error if they
        // are invalid
        AST::new(self.root.build()).unwrap()
    }

    fn add_file_handle_imports(
        &mut self,
        file_location: &ModulePath,
        to_add: FileID,
    ) -> Option<()> {
        let imports_information = self.handle_file(file_location, to_add)?;
        imports_information
            .into_iter()
            .map(|path| self.handle_import(path))
            .fold(Some(()), |a, b| a.zip(b).map(|_| ()))
    }

    fn handle_file(
        &mut self,
        file_location: &ModulePath,
        to_add: FileID,
    ) -> Option<Vec<ModulePath>> {
        let parsed = self.parse_file(&file_location, to_add)?;
        let imports_information = ModulePath::from_file(&parsed, &file_location);
        self.add_file(file_location, parsed);
        Some(imports_information)
    }

    fn add_file(&mut self, file_location: &ModulePath, file: File<UntypedAST>) -> Option<()> {
        let mut file_path = file_location.build_path_buf(self.program_information.projects())?;
        file_path.push(file.name().to_owned());
        self.root
            .add_file(ASTNode::new(file, file_path), &file_location.elements());
        Some(())
    }

    fn parse_file(
        &mut self,
        file_location: &ModulePath,
        to_add: FileID,
    ) -> Option<File<UntypedAST>> {
        let last = file_location.elements().pop()?;
        let file_information = FileInformation::new(to_add, &last, self.load_from)?;
        let parsed = parse(file_information)?;
        Some(parsed)
    }

    fn handle_import(&mut self, path: ModulePath) -> Option<()> {
        let module_dir = path.build_path_buf(self.program_information.projects())?;

        if Self::list_wasome_files_in_dir(&module_dir)?
            .all(|file| {
                // Only load the file if it isn't loaded, yet
                // We can't use an entire module at once as the main file is loaded alone
                if self.does_file_exist(&path, &file) {
                    // We don't load the file, but there is no error
                    return true;
                }
                let loaded = match self.load_file(module_dir.clone(), file) {
                    Some(value) => value,
                    None => return false,
                };

                self.add_file_handle_imports(&path, loaded);

                true
            })
        {
            Some(())
        } else {
            None
        }
    }

    fn does_file_exist(&mut self, path: &ModulePath, filename: &str) -> bool {
        self.root.subdir_by_path_nonmutating(&path.elements())
            .is_some_and(|module| module.file_by_name(filename).is_some())
    }

    fn list_wasome_files_in_dir(dir: &PathBuf) -> Option<impl Iterator<Item=String>> {
        Some(Loader::list_files(dir).ok()?
            .map(|file_name| file_name.into_string().ok())
            .collect::<Option<Vec<_>>>()?
            .into_iter()
            .filter(|file| {
                WASOME_FILE_ENDINGS
                    .iter()
                    .all(|ending| file.ends_with(ending))
            }))
    }

    fn load_file(&mut self, mut file_path: PathBuf, file: String) -> Option<FileID> {
        file_path.push(file);
        self.load_from.load_file(file_path).ok()
    }
}
