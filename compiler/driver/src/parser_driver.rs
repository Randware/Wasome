//! This part of the driver is responsible for loading programs from a [`SourceMap`] and
//! using the parser to turn it into an untyped AST.
//!
//! The parsing and loading can't be separated as the imports of a program, which will only be known
//! after parsing, decide what to load next.

use crate::error::DriverError;
use crate::program_information::ProgramInformation;
use crate::source_collector::{collect_program, CollectionError};
use crate::source_collector::source_element::{
    WasomeProgram, WasomeSourceDirectory, WasomeSourceFile,
};
use ast::directory::Directory;
use ast::file::File;
use ast::{ASTNode, UntypedAST, AST};
use error::diagnostic::Diagnostic;
use io::FullIO;
use parser::{parse, FileInformation};
use source::types::FileID;
use source::SourceMap;
use std::path::PathBuf;

/// Generates an entire untyped ast by loading it from the provided [`SourceMap`]
///
/// # Parameters
///
/// - **`program_info`** - Information about the program to generate an AST for
/// - **`load_from`** - The [`SourceMap`] to load from
///
/// # Return
///
/// The AST
///
/// # Errors
///
/// There are many error conditions, mainly:
/// - Syntax errors
/// - Unresolved imports
/// - File system errors
///     - Including paths from `program_info` being unresolved
pub fn generate_untyped_ast<Loader: FullIO>(
    program_info: &ProgramInformation,
    load_from: &mut SourceMap<Loader>,
) -> Result<AST<UntypedAST>, Diagnostic> {
    let program = collect_program(program_info, load_from).map_err(|err| 
    match err {
        CollectionError::Io(err) => DriverError::Io {source: err},
        CollectionError::WasomeSourceDirectoryCreationError(_) => unreachable!()
    })?;
    program.into_ast(load_from).map_err(Into::into)
}

impl WasomeProgram {
    fn into_ast(self, to_load_with: &SourceMap<impl FullIO>) -> Result<AST<UntypedAST>, DriverError> {
        let (location, projects) = self.destructure();
        let (name, path) = location.destructure();
        let root_dir = ASTNode::new(
            Directory::new(
                name,
                projects
                    .into_iter()
                    .map(|project| project.1.into_ast_dir(project.0, to_load_with))
                    .collect::<Result<Vec<_>, DriverError>>()?,
                Vec::new(),
            ),
            path,
        );
        AST::new(root_dir).map_err(|ui| DriverError::UnresolvedImport {
            span: *ui.unresolved_imports()[0].position(),
        })
    }
}

impl WasomeSourceDirectory {
    fn into_ast_dir(
        self,
        name: String,
        to_load_with: &SourceMap<impl FullIO>,
    ) -> Result<ASTNode<Directory<UntypedAST>, PathBuf>, DriverError> {
        let (location, subdirs, files) = self.destructure();
        let (_, path) = location.destructure();
        let files = files
            .into_iter()
            .map(|file| file.into_ast_file(&name, to_load_with))
            .collect::<Result<Vec<_>, DriverError>>()?;
        Ok(ASTNode::new(
            Directory::new(
                name,
                subdirs
                    .into_iter()
                    .map(|subdir| {
                        let name = subdir.location().name().to_string();
                        subdir.into_ast_dir(name, to_load_with)
                    })
                    .collect::<Result<Vec<_>, DriverError>>()?,
                files,
            ),
            path,
        ))
    }
}

impl WasomeSourceFile {
    fn into_ast_file(
        self,
        mod_name: &str,
        to_load_with: &SourceMap<impl FullIO>,
    ) -> Result<ASTNode<File<UntypedAST>, FileID>, DriverError> {
        Ok(ASTNode::new(
            parse(&FileInformation::new(self.file(), mod_name, to_load_with).unwrap())?,
            self.file(),
        ))
    }
}
