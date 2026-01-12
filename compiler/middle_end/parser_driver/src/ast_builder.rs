use crate::directory_builder::DirectoryBuilder;
use ast::top_level::ImportRoot;
use ast::{AST, ASTNode, UntypedAST};
use io::FullIO;
use parser::{FileInformation, parse};
use shared::program_information::ProgramInformation;
use source::SourceMap;
use source::types::FileID;
use std::path::PathBuf;

const WASOME_FILE_ENDINGS: &'static [&'static str] = &[".waso", ".✨"];
#[derive(Debug)]
pub(crate) struct ASTBuilder<'a, Loader: FullIO> {
    root: DirectoryBuilder,
    load_from: &'a mut SourceMap<Loader>,
}

impl<'a, Loader: FullIO> ASTBuilder<'a, Loader> {
    pub fn new(from: &ProgramInformation, load_from: &'a mut SourceMap<Loader>) -> Option<Self> {
        let mut to_ret = Self {
            root: DirectoryBuilder::new(from.name().to_owned(), from.path().to_owned()),
            load_from,
        };
        let main_file_location = from
            .main_file()
            .iter()
            .map(|file_name| file_name.to_os_string().into_string().ok())
            .collect::<Option<Vec<_>>>()?;
        let main_file_id = to_ret.load_from.load_file(from.path()).ok()?;
        to_ret.add_file_handle_imports(
            &main_file_location[..main_file_location.len() - 1],
            main_file_id,
        )?;
        Some(to_ret)
    }

    pub fn build(self) -> AST<UntypedAST> {
        // This will never panic as all imports must be valid as we add all imports and error if they
        // are invalid
        AST::new(self.root.build()).unwrap()
    }

    fn add_file_handle_imports(&mut self, file_location: &[String], to_add: FileID) -> Option<()> {
        let imports_information = self.add_file(file_location, to_add)?;
        imports_information
            .into_iter()
            .map(|(root, path)| self.add_import(file_location, root, path))
            .fold(Some(()), |a, b| a.zip(b).map(|_| ()))
    }

    fn add_file(&mut self, file_location: &[String], to_add: FileID) -> Option<Vec<(ImportRoot, Vec<String>)>> {
        let file_information = FileInformation::new(to_add, file_location.last()?, self.load_from)?;
        let parsed = parse(file_information)?;
        let imports_information = parsed
            .imports()
            .iter()
            .map(|import| (import.root().clone(), import.path().clone()))
            .collect::<Vec<_>>();
        self.root.add_file(
            ASTNode::new(
                parsed,
                file_location.iter().fold(PathBuf::new(), |mut acc, elem| {
                    acc.push(elem);
                    acc
                }),
            ),
            file_location,
        );
        Some(imports_information)
    }

    fn add_import(
        &mut self,
        container_file_location: &[String],
        import_root: ImportRoot,
        import_path: Vec<String>,
    ) -> Option<()> {
        let path = canonicalize_import_path(container_file_location, import_root, import_path);

        let path_buf = path.iter().fold(PathBuf::new(), |mut acc, elem| {
            acc.push(elem);
            acc
        });

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
                if self.root.file_by_path(&path).is_none() {
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

fn canonicalize_import_path(container_file_location: &[String], import_root: ImportRoot, import_path: Vec<String>) -> Vec<String> {
    let mut path = Vec::new();
    match import_root {
        ImportRoot::CurrentModule => container_file_location
            .iter()
            .for_each(|elem| path.push(elem.clone())),
        ImportRoot::Root => (),
    }
    import_path.iter().for_each(|elem| path.push(elem.clone()));
    path
}
