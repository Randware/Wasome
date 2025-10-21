/** A location in some code.
Identified by line and char.
Both line and char are zero-based
*/
#[derive(Debug, PartialEq)]
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

/** A area of code represented by start and end.
The start is inclusive
The line of the end is inclusive, the char exclusive
*/
#[derive(Debug, PartialEq)]
pub struct CodeArea {
    start: CodeLocation,
    end: CodeLocation,
}

impl CodeArea {
    /** Creates a new [`CodeArea`]
    Returns some if start is not before end
    else None
    */
    pub fn new(start: CodeLocation, end: CodeLocation) -> Option<Self> {
        if start.line > end.line || (start.line == end.line && start.char >= end.char) {
            return None;
        }
        Some(Self { start, end })
    }

    pub fn start(&self) -> &CodeLocation {
        &self.start
    }

    pub fn end(&self) -> &CodeLocation {
        &self.end
    }
}

#[cfg(test)]
mod tests {
    use crate::code_reference::{CodeArea, CodeLocation};

    #[test]
    fn create_codearea() {
        let codearea = CodeArea::new(CodeLocation::new(5, 5), CodeLocation::new(10, 0)).unwrap();
        // Rustrover detects a ghost syntax error here
        assert_eq!(codearea.start(), &CodeLocation::new(5, 5));
    }

    #[test]
    fn create_codearea_start_not_before_end_should_fail() {
        assert_eq!(
            None,
            CodeArea::new(CodeLocation::new(10, 5), CodeLocation::new(10, 0))
        );
        assert_eq!(
            None,
            CodeArea::new(CodeLocation::new(15, 0), CodeLocation::new(10, 10))
        );
    }
}
