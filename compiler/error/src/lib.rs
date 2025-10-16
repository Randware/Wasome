#![forbid(unsafe_code)]

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use std::fmt::Debug;

const ERROR_CONTEXT_LINES: usize = 3;
/** A syntax error
This struct is used for storing and displaying syntax errors
*/
#[derive(Debug)]

pub struct SyntaxError {
    area: CodeArea,
    file_location: String,
    error_type: Box<dyn ErrorType>,
}

impl SyntaxError {
    /** Creates a new syntax error with the specified fields
       Only intended for internal use.
       To create a SyntaxError elsewhere, use a [`SyntaxErrorBuilder`]
    */
    fn new(area: CodeArea, file_location: String, error_type: Box<dyn ErrorType>) -> Self {
        Self {
            area,
            file_location,
            error_type,
        }
    }
}

/** A builder for syntax errors
Expects the user to set every field before building
*/
#[derive(Debug)]

pub struct SyntaxErrorBuilder {
    area: Option<CodeArea>,
    file_location: Option<String>,
    error_type: Option<Box<dyn ErrorType>>,
}

impl Default for SyntaxErrorBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl SyntaxErrorBuilder {
    /** Creates a new and empty [`SyntaxErrorBuilder`]
     */
    pub fn new() -> Self {
        Self {
            area: None,
            file_location: None,
            error_type: None,
        }
    }

    /** Sets an error area
     */
    pub fn with_area(mut self, area: CodeArea) -> Self {
        self.area = Some(area);
        self
    }

    /** Sets the path of the file that contains the erroneous code
       This is for display purposes only
    */
    pub fn with_file_location(mut self, file_location: String) -> Self {
        self.file_location = Some(file_location);
        self
    }

    /** Sets the type of error
     */
    pub fn with_error_type(mut self, error_type: impl ErrorType + 'static) -> Self {
        self.error_type = Some(Box::new(error_type));
        self
    }

    /** Builds the [`SyntaxError`]
    # Panics
    Panics if not all fields are set
    */
    pub fn build(self) -> SyntaxError {
        if self.file_location.is_none() || self.error_type.is_none() || self.area.is_none() {
            panic!("Not all fields are set!");
        }
        SyntaxError::new(
            self.area.unwrap(),
            self.file_location.unwrap(),
            self.error_type.unwrap(),
        )
    }
}

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
       # Panics
       Panics if start is not before end
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

impl SyntaxError {
    /** Prints the error to stdout
    # Panic
    May panic if the area is referencing to locations that don't exist in the the provided code
    */
    pub fn print(&self, code: &str) {
        // Orange
        let error_color = Color::Rgb(255,102,17);
        let error_msg_color = Color::BrightRed;
        let code_color = Color::Rgb(220,240,255);

        let mut line_starting_pos = 0;
        // The start and end
        let error_start_line = self.area.start().line();
        let error_end_line = self.area.end().line();
        // The start and end including the padding
        let display_end = error_end_line + ERROR_CONTEXT_LINES;
        let display_start = if error_start_line < ERROR_CONTEXT_LINES {
            0
        } else {
            error_start_line - ERROR_CONTEXT_LINES
        };

        // The processed lines with annotations
        let mut lines = Vec::new();
        // Where the error begins, relative to the start of code
        let mut error_start_char = 0;
        let mut error_end_char = 0;
        // Process the codelines
        for (line_num, line) in code.lines().enumerate() {
            // If the code line is out of the window to print, skip it
            if line_num < display_start || line_num > display_end {
                //
                Self::update_line_starting_pos(code, &mut line_starting_pos);
                continue;
            }
            // Update the error start and end
            if line_num == error_start_line {
                error_start_char = line_starting_pos + self.area.start().char();
            }
            if line_num == error_end_line {
                error_end_char = line_starting_pos + line.len() + self.area.end().char();
            }
            // Where the error begins and where it ends in the current line
            // Used for making the text white or yellow
            #[allow(clippy::comparison_chain)]
            // Lines without annotations always have the lf selected
            let line_error_start_char = if line_num < error_start_line || line_num > error_end_line
            {
                line.len()
            }
            //Before the error lines, so it doesn't begin at all
            else if line_num == error_start_line {
                self.area.start().char()
            } else {
                0
            };

            // Using a match makes this hard to read
            #[allow(clippy::comparison_chain)]
            let line_error_end_char = if line_num > error_end_line {
                // Lines without annotations always have the lf selected
                line.len()
            }
            //After the error lines, so it end immediately
            else if line_num == error_end_line {
                self.area.end().char()
            } else {
                line.len()
            };

            let line_label = Label::new((
                &self.file_location,
                line_starting_pos..line_starting_pos + line.len(),
            )).with_color(code_color);
            let mut error_label = Label::new((
                &self.file_location,
                line_starting_pos + line_error_start_char..line_starting_pos + line_error_end_char,
            )).with_priority(1)
            .with_color(error_color);
            if line_num == self.area.end().line() {
                error_label = error_label.with_message(
                    format!("{}", self.error_type.to_string().fg(error_msg_color)))
            }
            lines.push(line_label);
            lines.push(error_label);
            Self::update_line_starting_pos(code, &mut line_starting_pos);
        }

        let report = Report::build(
            ReportKind::Error,
            (&self.file_location, error_start_char..error_end_char),
        )
        .with_message("A syntax error was found during compilation".fg(Color::BrightWhite))
        .with_labels(lines);
        // Prints Error and the error message
        report
            .finish()
            .print((&self.file_location, Source::from(code)))
            .unwrap();
    }

    fn update_line_starting_pos(code: &str, line_starting_pos: &mut usize) {
        if let Some(new_start) = advance_till_next_line(code, *line_starting_pos) {
            *line_starting_pos = new_start;
        }
    }
}

fn advance_till_next_line(to_advance: &str, current_index: usize) -> Option<usize> {
    let mut found_lf = false;
    let mut found_cr = false;
    for (char_index, char) in to_advance[current_index..to_advance.len()].char_indices() {
        if char == '\n' && !found_lf {
            found_lf = true;
            continue;
        }
        if char == '\r' && !found_cr {
            found_cr = true;
            continue;
        }
        if found_lf || found_cr {
            return Some(char_index + current_index);
        }
    }
    None
}

pub trait ErrorType: Debug {
    fn to_string(&self) -> String;
}

#[cfg(test)]
mod tests {
    use crate::{CodeArea, CodeLocation, ErrorType, SyntaxErrorBuilder};

    #[derive(Debug)]
    pub struct ExampleError(String);
    impl ErrorType for ExampleError {
        fn to_string(&self) -> String {
            format!(
                "\"{}\" is an invalid data type at this point in the programm",
                self.0
            )
        }
    }
    #[test]
    fn error() {
        let error = SyntaxErrorBuilder::new()
            .with_area(CodeArea::new(CodeLocation::new(12, 10), CodeLocation::new(12, 18)).unwrap())
            .with_error_type(ExampleError("CodeArea".to_string()))
            .with_file_location("main.waso".to_string())
            .build();

        error.print(include_str!("lib.rs"));
    }

    #[test]
    fn error_multiline() {
        let error = SyntaxErrorBuilder::new()
            .with_area(CodeArea::new(CodeLocation::new(6, 0), CodeLocation::new(8, 2)).unwrap())
            .with_error_type(ExampleError("CodeArea".to_string()))
            .with_file_location("main.waso".to_string())
            .build();

        error.print(include_str!("lib.rs"));
    }

    #[test]
    fn codearea_invalid_should_return_none() {
        let codearea = CodeArea::new(CodeLocation::new(10, 10), CodeLocation::new(5, 10));

        assert_eq!(None, codearea);

        let codearea = CodeArea::new(CodeLocation::new(10, 10), CodeLocation::new(10, 10));

        assert_eq!(None, codearea);
    }
}
