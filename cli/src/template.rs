use include_dir::{Dir, include_dir};
use std::fs;
use std::path::{Path, PathBuf};

static TEMPLATE_BIN: Dir = include_dir!("templates/bin");
static TEMPLATE_LIB: Dir = include_dir!("templates/lib");

type Replacements = Vec<(&'static str, String)>;

/// Represents the final project structure in memory
pub struct LoadedTemplate {
    files: Vec<(PathBuf, Vec<u8>)>,
}

pub struct Template;

impl Template {
    /// Create a library project template
    pub fn lib(project_name: impl ToString) -> LoadedTemplate {
        let replacements = vec![("{{PROJECT_NAME}}", project_name.to_string())];

        LoadedTemplate::build(&TEMPLATE_LIB, &replacements)
    }

    /// Create a binary project template
    pub fn bin(project_name: impl ToString) -> LoadedTemplate {
        let replacements = vec![("{{PROJECT_NAME}}", project_name.to_string())];

        LoadedTemplate::build(&TEMPLATE_BIN, &replacements)
    }
}

impl LoadedTemplate {
    /// Walks the directory and applies the replacements
    fn build(dir: &Dir, replacements: &Replacements) -> Self {
        let mut files = Vec::new();

        collect_files(dir, replacements, &mut files);

        Self { files }
    }

    /// Write the template to disk
    ///
    /// - If target is missing: Creates it atomically
    /// - If target exists: Writes files directly into it
    pub fn write(&self, target: impl AsRef<Path>) -> std::io::Result<()> {
        let target = target.as_ref();

        if target.exists() {
            // If we fail here, we stop, since rolling back changes to an
            // existing user directory is risky
            for (rel_path, content) in &self.files {
                let full_path = target.join(rel_path);

                if full_path.exists() {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::AlreadyExists,
                        format!("File already exists: {}", full_path.display()),
                    ));
                }

                if let Some(p) = full_path.parent() {
                    fs::create_dir_all(p)?;
                }

                fs::write(full_path, content)?;
            }
            Ok(())
        } else {
            let temp_dir = target.with_extension("tmp");

            // If we already have a tmp directory, we clean it, since this could break our atomic
            // creation logic
            if temp_dir.exists() {
                fs::remove_dir_all(&temp_dir)?;
            }

            fs::create_dir_all(&temp_dir)?;

            // We wrap our logic inside a closure, so we can catch any errors and later perform
            // clean up
            let write_result = || -> std::io::Result<()> {
                for (rel_path, content) in &self.files {
                    let full_path = temp_dir.join(rel_path);

                    if let Some(p) = full_path.parent() {
                        fs::create_dir_all(p)?;
                    }

                    fs::write(full_path, content)?;
                }
                Ok(())
            }();

            // If we ran into any issues, we clean up here
            if let Err(e) = write_result {
                let _ = fs::remove_dir_all(&temp_dir);
                return Err(e);
            }

            // If everything went well, we then rename our temp directoy to the correct name
            match fs::rename(&temp_dir, target) {
                Ok(_) => Ok(()),
                Err(e) => {
                    // If renaming fails, we clean up the tmp directory
                    let _ = fs::remove_dir_all(&temp_dir);
                    Err(e)
                }
            }
        }
    }
}

/// Takes text and a list of replacements, returns new text.
fn apply_replacements(input: &str, replacements: &Replacements) -> String {
    let mut output = input.to_string();

    for (key, value) in replacements {
        output = output.replace(key, value);
    }

    output
}

/// Recursive helper to traverse and collect
fn collect_files(dir: &Dir, replacements: &Replacements, acc: &mut Vec<(PathBuf, Vec<u8>)>) {
    for file in dir.files() {
        let path = file.path().to_path_buf();

        let content = match file.contents_utf8() {
            Some(text) => apply_replacements(text, replacements).into_bytes(),
            None => file.contents().to_vec(),
        };

        acc.push((path, content));
    }

    for subdir in dir.dirs() {
        collect_files(subdir, replacements, acc);
    }
}
