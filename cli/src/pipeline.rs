use std::path::PathBuf;

use crate::error::{CliError, CliResult};
use crate::workspace::Workspace;
use driver::pipeline::Pipeline;
use driver::program_information::BinaryProgramInformation;
use driver::source_element::{WasomeProgram, WasomeSourceDirectory};
use error::diagnostic::{Diagnostic, Level};
use source::types::FileID;

pub fn check(workspace: &mut Workspace) -> CliResult<()> {
    match driver::syntax_check(&workspace.info, &mut workspace.source) {
        Ok(_) => {
            Diagnostic::builder()
                .level(Level::Info)
                .message("Check was successful")
                .build()
                .print()?;
            Ok(())
        }
        Err(d) => {
            let _ = d.print_snippets(&workspace.source);
            Diagnostic::builder()
                .level(Level::Error)
                .message("Check was not successful")
                .build()
                .print()?;
            Err(CliError::CompilationFailed)
        }
    }
}

pub fn load(workspace: &mut Workspace) -> CliResult<WasomeProgram> {
    match driver::load_pipeline().process((&workspace.info, &mut workspace.source)) {
        Ok((program, _, _)) => Ok(program),
        Err(d) => {
            let _ = d.print_snippets(&workspace.source);
            Diagnostic::builder()
                .level(Level::Error)
                .message("Project loading was not successful")
                .build()
                .print()?;
            Err(CliError::ProjectLoadingFailed)
        }
    }
}

pub fn fmt(workspace: &mut Workspace, program: &WasomeProgram) -> CliResult<()> {
    let source_directory = program
        .projects()
        .get(workspace.info.main_project())
        .expect("Project map should always contain main project");

    let mut files = Vec::new();

    fn collect(dir: &WasomeSourceDirectory, out: &mut Vec<FileID>) {
        out.extend(dir.files().iter().map(|f| f.file()));
        dir.subdirs().iter().for_each(|d| collect(d, out));
    }

    if let Some(src_dir) = source_directory
        .subdirs()
        .iter()
        .find(|d| d.location().name() == "src")
    {
        collect(src_dir, &mut files);
    }

    let mut formatted_files: Vec<(PathBuf, String)> = Vec::new();
    let mut errors: Vec<PathBuf> = Vec::new();

    for file_id in files {
        let file = workspace
            .source
            .get_file(&file_id)
            .expect("Source map should always contain all source files");

        let unformatted = file.content();

        match formatter::format(unformatted.to_string()) {
            Ok(formatted) => formatted_files.push((file.path().clone(), formatted)),
            Err(_) => errors.push(file.path().clone()),
        }
    }

    if !errors.is_empty() {
        for path in errors {
            Diagnostic::builder()
                .level(Level::Error)
                .message(format!("Failed to format {}", path.display()))
                .build()
                .print()?;
        }
        return Err(CliError::FormattingFailed);
    }

    for (path, content) in formatted_files {
        if let Err(e) = std::fs::write(&path, content) {
            Diagnostic::builder()
                .level(Level::Error)
                .message(format!("Failed to write to file {}: {}", path.display(), e))
                .build()
                .print()?;
            return Err(CliError::FormattingFailed);
        }
    }

    Ok(())
}
