use ast::UntypedAST;
use ast::file::File;
use ast::top_level::ImportRoot;
use crate::program_information::Project;
use std::path::PathBuf;

/// Represents the fully qualified path to a module
///
/// This path identifies a module uniquely across the entire program
#[derive(Debug)]
pub(crate) struct ModulePath {
    /// The path relative to the project root
    relative_to_project: ModulePathProjectRelative,
    /// The name of the project this module belongs to
    project: String,
}

impl ModulePath {
    /// Creates a new `ModulePath`
    ///
    /// # Parameters
    ///
    /// - **relative_to_project** - The path within the project
    /// - **project** - The name of the project
    pub fn new(relative_to_project: ModulePathProjectRelative, project: String) -> Self {
        Self {
            relative_to_project,
            project,
        }
    }

    /// Creates a `ModulePath` from import information
    ///
    /// The resulting `ModulePath` will reference the module being imported.
    /// Calculates the target module path based on where the import happens and what is imported
    ///
    /// # Parameters
    ///
    /// - **container_file_location** - The location of the file containing the import
    /// - **import_root** - The root of the import
    /// - **import_path** - The import path
    ///
    /// # Return
    ///
    /// The created import path
    ///
    /// # Errors
    /// If `import_path.empty() && import_root == ImportRoot::Root`
    ///
    /// All errors are represented by a return of `None`
    pub fn from_import_information(
        container_file_location: &Self,
        import_root: ImportRoot,
        import_path: Vec<String>,
    ) -> Option<Self> {
        let project = match import_root {
            ImportRoot::CurrentModule => &container_file_location.project,
            ImportRoot::Root => import_path.first()?,
        }
        .to_owned();
        // We already checked for the error condition
        let path = combine_import_path(
            &container_file_location.relative_to_project,
            import_root,
            import_path,
        )
        .unwrap();
        Some(Self {
            relative_to_project: ModulePathProjectRelative::new(path),
            project,
        })
    }

    /// Extracts all module paths referenced by imports of a file
    ///
    /// # Parameters
    ///
    /// - **file** - The file to analyze
    /// - **file_module** - The module path of the file itself
    ///
    /// # Return
    ///
    /// A list of all resolved import paths
    pub fn from_file(file: &File<UntypedAST>, file_module: &Self) -> Vec<Self> {
        file.imports()
            .iter()
            .map(|import| {
                // A file may not have empty imports relative to the root
                // [`Self::from_import_information`] has this as its sole error condition
                // Therefore, this will never panic
                Self::from_import_information(
                    file_module,
                    import.root().clone(),
                    import.path().clone(),
                )
                .unwrap()
            })
            .collect::<Vec<_>>()
    }

    /// Constructs the physical file system path for this module
    ///
    /// The resulting path will be relative to the program root
    ///
    /// # Parameters
    ///
    /// - **projects** - The list of available projects to resolve the project name
    ///
    /// # Return
    ///
    /// The physical path of the module
    ///
    /// # Errors
    ///
    /// If the project specified in this path does not exist in the provided list
    ///
    /// All errors are represented by a return of `None`
    pub fn build_path_buf(&self, projects: &[Project]) -> Option<PathBuf> {
        Some(
            projects
                .iter()
                .find(|project| project.name() == self.project)?
                .path()
                .iter()
                .chain(self.relative_to_project.build_path_buf().iter())
                .collect::<PathBuf>(),
        )
    }

    /// Returns the path components as a list of strings
    ///
    /// The first element is the project name
    ///
    /// # Return
    ///
    /// **Vec<String>** - The components of the path
    pub fn elements(&self) -> Vec<String> {
        let inner_elements = self.relative_to_project.elements();
        let mut elements = Vec::with_capacity(inner_elements.len() + 1);
        elements.push(self.project.to_owned());
        inner_elements
            .iter()
            .for_each(|elem| elements.push(elem.to_owned()));
        elements
    }
}

/// Represents a module path relative to a project root
#[derive(Debug)]
pub(crate) struct ModulePathProjectRelative {
    /// The path elements (segments)
    ///
    /// May be empty
    elements: Vec<String>,
}

impl ModulePathProjectRelative {
    /// Creates a new `ModulePathProjectRelative`
    ///
    /// # Parameter
    ///
    /// - **elements** - The path segments
    pub fn new(elements: Vec<String>) -> Self {
        Self { elements }
    }

    /// Gets the path elements
    ///
    /// # Return
    ///
    /// **&[String]** - The path segments
    pub fn elements(&self) -> &[String] {
        &self.elements
    }

    /// Converts the relative path to a `PathBuf`
    ///
    /// # Return
    ///
    /// **PathBuf** - The constructed path
    pub fn build_path_buf(&self) -> PathBuf {
        self.elements.iter().fold(PathBuf::new(), |mut acc, elem| {
            acc.push(elem);
            acc
        })
    }
}

/// Combines a container location and an import path
///
/// Determines the resulting path segments based on the import root type
///
/// # Parameters
///
/// - **container_file_location** - The path of the module containing the import
/// - **import_root** - The root of the import path
/// - **import_path** - The path being imported
///
/// # Return
///
/// **Vec<String>** - The combined path segments
///
/// # Errors
/// If `import_path.empty() && import_root == ImportRoot::Root`
///
/// All errors are represented by a return of `None`
fn combine_import_path(
    container_file_location: &ModulePathProjectRelative,
    import_root: ImportRoot,
    mut import_path: Vec<String>,
) -> Option<Vec<String>> {
    Some(match import_root {
        ImportRoot::CurrentModule => {
            let mut path = container_file_location.elements.to_vec();
            path.append(&mut import_path);
            path
        }
        ImportRoot::Root => {
            // Checks if the import path is empty
            import_path.first()?;
            // Remove the project name from the import path as it's handled separately
            import_path.remove(0);
            import_path
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::top_level::ImportRoot;
    use crate::program_information::Project;
    use std::path::PathBuf;

    #[test]
    fn test_new() {
        let relative = ModulePathProjectRelative::new(vec!["foo".to_string(), "bar".to_string()]);
        let module_path = ModulePath::new(relative, "MyProject".to_string());

        assert_eq!(module_path.project, "MyProject");
        assert_eq!(module_path.relative_to_project.elements(), &["foo", "bar"]);
    }

    #[test]
    fn test_elements() {
        let relative = ModulePathProjectRelative::new(vec!["a".to_string(), "b".to_string()]);
        let module_path = ModulePath::new(relative, "Proj".to_string());

        let elements = module_path.elements();
        assert_eq!(elements, vec!["Proj", "a", "b"]);
    }

    #[test]
    fn test_from_import_information_current_module() {
        let container_relative = ModulePathProjectRelative::new(vec!["src".to_string()]);
        let container = ModulePath::new(container_relative, "CurrentProj".to_string());

        let import_path = vec!["utils".to_string(), "helper".to_string()];

        let result =
            ModulePath::from_import_information(&container, ImportRoot::CurrentModule, import_path)
                .expect("Should successfully create path");

        assert_eq!(result.project, "CurrentProj");
        assert_eq!(
            result.relative_to_project.elements(),
            &["src", "utils", "helper"]
        );
    }

    #[test]
    fn test_from_import_information_current_module_empty_import() {
        let container_relative = ModulePathProjectRelative::new(vec!["src".to_string()]);
        let container = ModulePath::new(container_relative, "CurrentProj".to_string());

        let import_path: Vec<String> = vec![];

        let result =
            ModulePath::from_import_information(&container, ImportRoot::CurrentModule, import_path)
                .expect("Should successfully create path");

        assert_eq!(result.project, "CurrentProj");
        assert_eq!(result.relative_to_project.elements(), &["src"]);
    }

    #[test]
    fn test_from_import_information_root() {
        let container_relative = ModulePathProjectRelative::new(vec!["src".to_string()]);
        let container = ModulePath::new(container_relative, "CurrentProj".to_string());

        let import_path = vec![
            "OtherProj".to_string(),
            "lib".to_string(),
            "math".to_string(),
        ];

        let result = ModulePath::from_import_information(&container, ImportRoot::Root, import_path)
            .expect("Should successfully create path");

        assert_eq!(result.project, "OtherProj");
        // "OtherProj" is stripped from relative path
        assert_eq!(result.relative_to_project.elements(), &["lib", "math"]);
    }

    #[test]
    fn test_from_import_information_root_project_only() {
        let container_relative = ModulePathProjectRelative::new(vec!["src".to_string()]);
        let container = ModulePath::new(container_relative, "CurrentProj".to_string());

        let import_path = vec!["OtherProj".to_string()];

        let result = ModulePath::from_import_information(&container, ImportRoot::Root, import_path)
            .expect("Should successfully create path");

        assert_eq!(result.project, "OtherProj");
        assert!(result.relative_to_project.elements().is_empty());
    }

    #[test]
    fn test_from_import_information_root_empty_fails() {
        let container_relative = ModulePathProjectRelative::new(vec!["src".to_string()]);
        let container = ModulePath::new(container_relative, "CurrentProj".to_string());

        let import_path: Vec<String> = vec![];

        let result = ModulePath::from_import_information(&container, ImportRoot::Root, import_path);

        assert!(result.is_none());
    }

    #[test]
    fn test_build_path_buf() {
        let proj_a_path = PathBuf::from("/path/to/A");
        let proj_b_path = PathBuf::from("/path/to/B");

        let projects = vec![
            Project::new("ProjectA".to_string(), proj_a_path.clone()),
            Project::new("ProjectB".to_string(), proj_b_path.clone()),
        ];

        // Case 1: Existing project, with relative path
        let relative_a =
            ModulePathProjectRelative::new(vec!["src".to_string(), "main".to_string()]);
        let module_a = ModulePath::new(relative_a, "ProjectA".to_string());

        let path_a = module_a
            .build_path_buf(&projects)
            .expect("Should find path");
        // /path/to/A/src/main
        let expected_a = proj_a_path.join("src").join("main");
        assert_eq!(path_a, expected_a);

        // Case 2: Existing project, empty relative path
        let relative_b = ModulePathProjectRelative::new(vec![]);
        let module_b = ModulePath::new(relative_b, "ProjectB".to_string());

        let path_b = module_b
            .build_path_buf(&projects)
            .expect("Should find path");
        assert_eq!(path_b, proj_b_path);

        // Case 3: Non-existent project
        let relative_c = ModulePathProjectRelative::new(vec!["foo".to_string()]);
        let module_c = ModulePath::new(relative_c, "ProjectC".to_string());

        let path_c = module_c.build_path_buf(&projects);
        assert!(path_c.is_none());
    }

    #[test]
    fn test_module_path_project_relative() {
        let elements = vec!["one".to_string(), "two".to_string()];
        let relative = ModulePathProjectRelative::new(elements.clone());

        assert_eq!(relative.elements(), &elements);

        let path_buf = relative.build_path_buf();
        let expected = PathBuf::from("one").join("two");
        assert_eq!(path_buf, expected);
    }
}
