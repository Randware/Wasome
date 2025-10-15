#![forbid(unsafe_code)]

use std::fmt::Debug;
use std::ops::Add;
use colored::{Color, Colorize};

const ERROR_CONTEXT_LINES: usize = 3;
/** A syntax error
This struct is used for storing and displaying syntax errors
*/
#[derive(Debug)]

pub struct SyntaxError
{
    area: CodeArea,
    file_location: String,
    error_type: Box<dyn ErrorType>
}

impl SyntaxError
{
    /** Creates a new syntax error with the specified fields
       Only intended for internal use.
       To create a SyntaxError elsewhere, use a [`SyntaxErrorBuilder`]
    */
    fn new(area: CodeArea, file_location: String, error_type: Box<dyn ErrorType>) -> Self {
        Self { area, file_location, error_type }
    }
}

/** A builder for syntax errors
Expects the user to set every field before building
*/
#[derive(Debug)]

pub struct SyntaxErrorBuilder
{
    area: Option<CodeArea>,
    file_location: Option<String>,
    error_type: Option<Box<dyn ErrorType>>
}

impl Default for SyntaxErrorBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl SyntaxErrorBuilder
{
    /** Creates a new and empty [`SyntaxErrorBuilder`]
    */
    pub fn new() -> Self {
        Self { area: None, file_location: None, error_type: None }
    }

    /** Sets an error area
    */
    pub fn with_area(mut self, area: CodeArea) -> Self
    {
        self.area = Some(area);
        self
    }

    /** Sets the path of the file that contains the erroneous code
       This is for display purposes only
    */
    pub fn with_file_location(mut self, file_location: String) -> Self
    {
        self.file_location = Some(file_location);
        self
    }

    /** Sets the type of error
    */
    pub fn with_error_type(mut self, error_type: impl ErrorType+'static) -> Self
    {
        self.error_type = Some(Box::new(error_type));
        self
    }

    /** Builds the [`SyntaxError`]
    # Panics
    Panics if not all fields are set
    */
    pub fn build(self) -> SyntaxError
    {
        if self.file_location.is_none() ||
            self.error_type.is_none() ||
            self.area.is_none()
        {
            panic!("Not all fields are set!");
        }
        SyntaxError::new(self.area.unwrap(),
                                     self.file_location.unwrap(),
                                     self.error_type.unwrap())
    }
}

/** A location in some code.
Identified by line and char.
Both line and char are zero-based
*/
#[derive(Debug, PartialEq)]
pub struct CodeLocation
{
    line: usize,
    char: usize
}

impl CodeLocation
{
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
pub struct CodeArea
{
    start: CodeLocation,
    end: CodeLocation
}

impl CodeArea
{
    /** Creates a new [`CodeArea`]
       # Panics
       Panics if start is not before end
    */
    pub fn new(start: CodeLocation, end: CodeLocation) -> Option<Self> {
        if start.line > end.line
            || (start.line == end.line && start.char >= end.char)
        {
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

const ERROR_CODE_COLOR: Color = Color::TrueColor {
    r: 255,
    g: 127,
    b: 0,
};

impl SyntaxError
{
    /** Prints the error to stdout
    # Panic
    Will panic if the area is referencing to locations that don't exist in the the provided code
    */
    pub fn print(&self, code: &str)
    {
        // Prints Error and the error message
        eprintln!("{} {}", "Error:".red(), self.error_type.to_string());
        eprintln!();
        eprintln!("{}{}{}", "[".bright_black().bold(), self.file_location.bright_black().bold(), "]".bright_black().bold());

        // The start and end
        let error_start_line = self.area.start.line();
        let error_end_line = self.area.end.line();
        // The start and end including the padding
        let display_end = error_end_line+ERROR_CONTEXT_LINES;
        let display_start = if error_start_line < ERROR_CONTEXT_LINES {0} else {error_start_line-ERROR_CONTEXT_LINES};
        // Print the codelines
        for (index, line) in code.lines().enumerate()
        {
            // If the code line is out of the window to print, skip it
            if index < display_start || index > display_end
            {
                continue;
            }

            // Where the error begins and where it ends in the current line
            // Used for making the text white or yellow
            #[allow(clippy::comparison_chain)]
            let error_start_char =
                if index < error_start_line {line.len()} //Before the error lines, so it doesn't begin at all
                else if index == error_start_line {self.area.start.char()}
                else {0};

            // Using a match makes this hard to read
            #[allow(clippy::comparison_chain)]
            let error_end_char =
                if index > error_end_line {0} //After the error lines, so it end immediately
                else if index == error_end_line {self.area.end.char()}
                else {line.len()};

            // Print the line
            // The line is zero-based
            eprint!("{}", (index+1).to_string().add(": \t").bright_blue().bold());
            if error_start_char != 0 // If the error starts at the beginning of the line, don't include a white portion
            {
                eprint!("{}", &line[0..error_start_char]);
            }
            eprint!("{}", &line[error_start_char..error_end_char].color(ERROR_CODE_COLOR));
            if error_end_char != line.len()
            {
                eprint!("{}", &line[error_end_char..line.len()]);
            }
            eprintln!()
        }

    }
}

pub trait ErrorType: Debug
{
    fn to_string(&self) -> String;

}


#[cfg(test)]
mod tests {
    use crate::{CodeArea, CodeLocation, ErrorType, SyntaxErrorBuilder};

    #[derive(Debug)]
    pub struct ExampleError(String);
    impl ErrorType for ExampleError
    {
        fn to_string(&self) -> String {
            format!("\"{}\" is an invalid data type at this point in the programm", self.0)
        }
    }
    #[test]
    fn error()
    {
        let error = SyntaxErrorBuilder::new()
            .with_area(CodeArea::new(CodeLocation::new(14,10), CodeLocation::new(14,18)).unwrap())
            .with_error_type(ExampleError("CodeArea".to_string()))
            .with_file_location("main.waso".to_string())
            .build();

        error.print(include_str!("lib.rs"));
    }

    fn codearea_invalid_should_return_none()
    {
        let codearea = CodeArea::new(CodeLocation::new(10,10),
        CodeLocation::new(5,10));

        assert_eq!(None, codearea);

        let codearea = CodeArea::new(CodeLocation::new(10,10),
                                     CodeLocation::new(10,10));

        assert_eq!(None, codearea);
    }
}