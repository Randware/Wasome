use crate::code_file::CodeFile;
use std::cmp::Ordering;

/** A location in some code.
Identified by line and char.
Both line and char are zero-based
*/
#[derive(Debug, PartialEq, Eq)]
pub struct CodeLocation {
    line: usize,
    char: usize,
}

impl CodeLocation {
    pub fn new(line: usize, char: usize) -> Self {
        Self { line, char }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn char(&self) -> usize {
        self.char
    }
}

impl PartialOrd for CodeLocation {
    // CodeLocations can always be compared, so we use the implementation from Ord
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CodeLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line() {
            line if line < other.line() => return Ordering::Less,
            line if line > other.line() => return Ordering::Greater,
            _ => (),
        }
        match self.char() {
            line if line < other.char() => return Ordering::Less,
            line if line > other.char() => return Ordering::Greater,
            _ => (),
        }
        Ordering::Equal
    }
}

/** A area of code represented by start and end and a file where it is located
The start is inclusive
The line of the end is inclusive, the char exclusive
*/
#[derive(Debug, PartialEq, Eq)]
pub struct CodeArea {
    start: CodeLocation,
    end: CodeLocation,
    file: CodeFile,
}

impl CodeArea {
    /** Creates a new [`CodeArea`]
    Returns some if start is not before end
    else None
    */
    pub fn new(start: CodeLocation, end: CodeLocation, file: CodeFile) -> Option<Self> {
        if start > end {
            return None;
        }
        Some(Self { start, end, file })
    }

    pub fn start(&self) -> &CodeLocation {
        &self.start
    }

    pub fn end(&self) -> &CodeLocation {
        &self.end
    }

    pub fn file(&self) -> &CodeFile {
        &self.file
    }
}

#[cfg(test)]
mod tests {
    use crate::code_file::CodeFile;
    use crate::code_reference::{CodeArea, CodeLocation};
    use std::cmp::Ordering;
    use std::path::PathBuf;

    fn test_code_file() -> CodeFile {
        CodeFile::new(PathBuf::from("test/test"))
    }
    #[test]
    fn create_codearea() {
        let codearea = CodeArea::new(
            CodeLocation::new(5, 5),
            CodeLocation::new(10, 0),
            test_code_file(),
        )
        .unwrap();
        assert_eq!(codearea.start(), &CodeLocation::new(5, 5));
        assert_eq!(codearea.file.to_string(), "test/test".to_string())
    }

    #[test]
    fn create_codearea_start_not_before_end_should_fail() {
        assert_eq!(
            None,
            CodeArea::new(
                CodeLocation::new(10, 5),
                CodeLocation::new(10, 0),
                test_code_file()
            )
        );
        assert_eq!(
            None,
            CodeArea::new(
                CodeLocation::new(15, 0),
                CodeLocation::new(10, 10),
                test_code_file()
            )
        );
    }

    #[test]
    fn compare_codelocations() {
        let small = CodeLocation::new(5, 5);
        let big = CodeLocation::new(10, 5);

        assert_eq!(Ordering::Less, small.cmp(&big));
        assert_eq!(small.partial_cmp(&big), Some(small.cmp(&big)));
    }

    #[test]
    fn compare_codelocations_2() {
        let small = CodeLocation::new(10, 4);
        let big = CodeLocation::new(10, 5);

        assert_eq!(Ordering::Less, small.cmp(&big));
        assert_eq!(small.partial_cmp(&big), Some(small.cmp(&big)));
    }

    #[test]
    fn compare_codelocations_3() {
        let cl = CodeLocation::new(5, 5);

        assert_eq!(Ordering::Equal, cl.cmp(&cl));
        assert_eq!(cl.partial_cmp(&cl), Some(cl.cmp(&cl)));
    }
}
