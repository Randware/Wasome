use std::fmt::{Display, Formatter};
use std::path::PathBuf;

/** This is a file that contains code.
Any path strings provided by this will be relative to the root of the project in the OSes native format
*/
#[derive(Debug, Eq, PartialEq, Clone)]
#[deprecated(
    note = "Superseded by the `source` crate. The new implementation handles path canonicalization correctly and should be preferred."
)]
pub struct CodeFile {
    filepath: PathBuf,
}

impl CodeFile {
    /** Constructs a new CodeFile with the specified PathBuf <br>
    It is assumed to be relative the project root
    */
    pub fn new(filepath: PathBuf) -> Self {
        Self { filepath }
    }

    pub fn filepath(&self) -> &PathBuf {
        &self.filepath
    }
}

impl Display for CodeFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.filepath.display())
    }
}

#[cfg(test)]
mod tests {
    use crate::code_file::CodeFile;
    use std::path::PathBuf;

    #[test]
    fn code_find_create_display() {
        let code_file = CodeFile::new(PathBuf::from("test/test"));
        assert_eq!("test/test".to_string(), code_file.to_string())
    }
}
