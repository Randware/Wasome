use crate::directory_builder::DirectoryBuilder;
use ast::{AST, ASTNode, UntypedAST};
use io::FullIO;
use parser::{FileInformation, parse};
use shared::program_information::ProgramInformation;
use source::SourceMap;
use source::types::FileID;
use ast::file::File;
use crate::module_path::{ModulePath, ModulePathProjectRelative};

/// All valid wasome file extensions
const WASOME_FILE_ENDINGS: &'static [&'static str] = &[".waso", ".✨"];

/// Builds an entire untyped AST
#[derive(Debug)]
pub(crate) struct ASTBuilder<'a, Loader: FullIO> {
    root: DirectoryBuilder,
    load_from: &'a mut SourceMap<Loader>,
    program_information: &'a ProgramInformation
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
            program_information: from
        };
        let mut main_file_location = from
            .main_file()
            .iter()
            .map(|file_name| file_name.to_os_string().into_string().ok())
            .collect::<Option<Vec<_>>>()?;
        main_file_location.pop();
        let main_file_location = ModulePath::new(ModulePathProjectRelative::new(main_file_location), from.main_project().to_owned());
        let main_file_id = to_ret.load_from.load_file(from.path()).ok()?;
        to_ret.add_file_handle_imports(
            &main_file_location,
            main_file_id,
        )?;
        Some(to_ret)
    }

    pub fn build(self) -> AST<UntypedAST> {
        // This will never panic as all imports must be valid as we add all imports and error if they
        // are invalid
        AST::new(self.root.build()).unwrap()
    }

    fn add_file_handle_imports(&mut self, file_location: &ModulePath, to_add: FileID) -> Option<()> {
        let imports_information = self.handle_file(file_location, to_add)?;
        imports_information
            .into_iter()
            .map(|path| self.add_import(path))
            .fold(Some(()), |a, b| a.zip(b).map(|_| ()))
    }

    fn handle_file(&mut self, file_location: &ModulePath, to_add: FileID) -> Option<Vec<ModulePath>> {
        let parsed = self.load_file(&file_location, to_add)?;
        let imports_information = ModulePath::from_file(&parsed, &file_location);
        self.add_file(file_location, parsed);
        Some(imports_information)
    }

    fn add_file(&mut self, file_location: &ModulePath, file: File<UntypedAST>) -> Option<()> {
        let mut file_path = file_location.build_path_buf(self.program_information.projects())?;
        file_path.push(file.name().to_owned());
        self.root.add_file(
            ASTNode::new(
                file,
                file_path,
            ),
            &file_location.elements(),
        );
        Some(())
    }

    fn load_file(&mut self, file_location: &ModulePath, to_add: FileID) -> Option<File<UntypedAST>> {
        let last = file_location.elements().pop()?;
        let file_information = FileInformation::new(to_add, &last, self.load_from)?;
        let parsed = parse(file_information)?;
        Some(parsed)
    }

    fn add_import(
        &mut self,
        path: ModulePath
    ) -> Option<()> {
        let path_buf = path.build_path_buf(self.program_information.projects())?;

        let files = Loader::list_files(&path_buf).ok()?;
        if files
            .map(|file_name| file_name.into_string().ok())
            .collect::<Option<Vec<_>>>()?
            .into_iter()
            .filter(|file| {
                WASOME_FILE_ENDINGS
                    .iter()
                    .all(|ending| file.ends_with(ending))
            })
            .all(|file| {
                let mut file_path = path_buf.clone();
                file_path.push(file);
                let loaded = match self.load_from.load_file(file_path) {
                    Ok(val) => val,
                    Err(_) => return false,
                };
                if self.root.file_by_path(&path.elements()).is_none() {
                    self.add_file_handle_imports(&path, loaded);
                }
                true
            })
        {
            Some(())
        } else {
            None
        }
    }
}
