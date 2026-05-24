use std::path::PathBuf;

use crate::command::Profile;
use crate::error::{CliError, CliResult};
use crate::manifest;
use crate::stdlib;
use crate::workspace::Workspace;
use driver::pipeline::Pipeline;
use driver::program_information::{BinaryProgramInformation, Project};
use driver::source_element::{WasomeProgram, WasomeSourceDirectory};
use error::diagnostic::{Diagnostic, Level};
use linker::LinkableFile;
use source::types::FileID;

pub fn check(workspace: &mut Workspace) -> CliResult<()> {
    let info = workspace.info();
    match driver::syntax_check(&info, &mut workspace.source) {
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
    let info = workspace.info();
    match driver::load_pipeline().process((&info, &mut workspace.source)) {
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

pub fn build(
    workspace: Workspace,
    profile: Profile,
    target: &stdlib::Target,
    extra_paths: &[PathBuf],
) -> CliResult<LinkableFile> {
    let mut link_files = Vec::new();

    // Load the pre-compiled stdlib archives
    link_files.extend(target.load_archives()?);

    // Load any additional user-provided files
    for path in extra_paths {
        let file =
            LinkableFile::from_path(path).map_err(|e| CliError::LinkFileError(path.clone(), e))?;
        link_files.push(file);
    }

    let stdlib_project = Project::new(
        stdlib::STDLIB_PROJECT_NAME.to_string(),
        target.wasome_dir().join(manifest::SRC_DIR),
    );
    let opt_level = profile.to_opt_level();

    let (info, mut source) = workspace.into_program_info(opt_level, vec![stdlib_project]);

    match driver::compile_link_pipeline().process((&info, &mut source, link_files)) {
        Ok(output) => Ok(output),
        Err(d) => {
            let _ = d.print_snippets(&source);
            Diagnostic::builder()
                .level(Level::Error)
                .message("Compilation failed")
                .build()
                .print()?;
            Err(CliError::LinkingFailed)
        }
    }
}

pub fn fmt(workspace: &mut Workspace, program: &WasomeProgram) -> CliResult<()> {
    let info = workspace.info();
    let source_directory = program
        .projects()
        .get(info.main_project())
        .expect("Project map should always contain main project");

    let mut files = Vec::new();

    fn collect(dir: &WasomeSourceDirectory, out: &mut Vec<FileID>) {
        out.extend(dir.files().iter().map(|f| f.file()));
        dir.subdirs().iter().for_each(|d| collect(d, out));
    }

    if let Some(src_dir) = source_directory
        .subdirs()
        .iter()
        .find(|d| d.location().name() == manifest::SRC_DIR)
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
