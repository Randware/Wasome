#![forbid(unsafe_code)]

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use std::fmt::Debug;
use std::io::{stderr, Write};

const DEFAULT_ERROR_CONTEXT_LINES: usize = 3;

/** Decided details of how errors should be outputted
*/
pub struct ErrorOutputConfig
{
    // This uses a trait object as PrintConfig having a generic parameter would expose its implementation
    // details
    /// Where the error should be written to
    output: Box<dyn Write>,
    /// The context for the error message
    context: usize
}

impl ErrorOutputConfig
{
    pub fn new(output: impl Write+'static, context: usize) -> Self {
        Self { output: Box::new(output), context }
    }



    fn output(self) -> Box<dyn Write> {
        self.output
    }

    fn context(&self) -> usize {
        self.context
    }

    pub fn set_output(&mut self, output: Box<dyn Write>) {
        self.output = output;
    }

    pub fn set_context(&mut self, context: usize) {
        self.context = context;
    }

    pub fn with_output(mut self, output: Box<dyn Write>) -> Self {
        self.set_output(output);
        self
    }

    pub fn with_context(mut self, context: usize) -> Self {
        self.set_context(context);
        self
    }
}

impl Default for ErrorOutputConfig
{
    fn default() -> Self {
        ErrorOutputConfig::new(stderr(), DEFAULT_ERROR_CONTEXT_LINES)
    }
}
/** A syntax error
This struct is used for storing and displaying syntax errors
*/
#[derive(Debug)]
pub struct SyntaxError {
    area: CodeArea,
    file_location: String,
    error_type: ErrorType,
    error_msg: String,
    code: String
}

impl SyntaxError {
    /** Creates a new syntax error with the specified fields
       Only intended for internal use.
       To create a SyntaxError elsewhere, use a [`SyntaxErrorBuilder`]
    */
    fn new(area: CodeArea, file_location: String, error_type: ErrorType, error_msg: String, code: String) -> Self {
        Self {
            area,
            file_location,
            error_type,
            error_msg,
            code
        }
    }

    pub fn builder() -> SyntaxErrorBuilder<NotSet, NotSet, NotSet, NotSet, NotSet> {
        SyntaxErrorBuilder::new()
    }

    /** Prints the error to stderr with the default context
          # Panic
          May panic if the area is referencing to locations that don't exist in the provided code
    */
    pub fn print_to_stderr_default_context(&self) {
        self.write(ErrorOutputConfig::default())
    }

    /** Writes the error to the specified output
       # Panic
       May panic if the area is referencing to locations that don't exist in the provided code
    */
    pub fn write(&self, print_config: ErrorOutputConfig) {
        let error_context = print_config.context();
        // Orange
        let error_code_color = Color::BrightWhite;
        let error_msg_color = Color::Red;
        let syntax_error_color = Color::BrightWhite;
        let code_color = Color::Primary;

        let mut line_starting_pos = 0;
        // The start and end
        let error_start_line = self.area.start().line();
        let error_end_line = self.area.end().line();
        // The start and end including the padding
        let display_end = error_end_line + error_context;
        let display_start = if error_start_line < error_context {
            0
        } else {
            error_start_line - error_context
        };

        // The processed lines with annotations
        let mut lines = Vec::new();
        // Where the error begins, relative to the start of code
        let mut error_start_char = 0;
        let mut error_end_char = 0;
        // Process the codelines
        for (line_num, line) in self.code.lines().enumerate() {
            // If the code line is out of the window to print, skip it
            if line_num < display_start || line_num > display_end {
                //
                Self::update_line_starting_pos(&self.code, &mut line_starting_pos);
                continue;
            }
            // Update the error start and end
            if line_num == error_start_line {
                error_start_char = line_starting_pos + self.area.start().char();
            }
            if line_num == error_end_line {
                error_end_char = line_starting_pos + self.area.end().char();
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

            // Add the line
            let line_label = Label::new((
                &self.file_location,
                line_starting_pos..line_starting_pos + line.len(),
            ))
                .with_color(code_color);
            // Mark the error
            let mut error_label = Label::new((
                &self.file_location,
                line_starting_pos + line_error_start_char..line_starting_pos + line_error_end_char,
            ))
                .with_priority(1)
                .with_color(error_code_color);
            // Add the error message
            if line_num == self.area.end().line() {
                error_label =
                    error_label.with_message((&self.error_msg).fg(error_msg_color))
            }
            lines.push(line_label);
            lines.push(error_label);
            Self::update_line_starting_pos(&self.code, &mut line_starting_pos);
        }

        let report = Report::build(
            self.error_type.as_report_kind(),
            (&self.file_location, error_start_char..error_end_char),
        )
            .with_message(self.error_type.as_msg().fg(syntax_error_color))
            .with_labels(lines);
        // Prints Error and the error message
        report
            .finish()
            .write((&self.file_location, Source::from(&self.code)), print_config.output())
            .unwrap();
    }

    fn update_line_starting_pos(code: &str, line_starting_pos: &mut usize) {
        if let Some(new_start) = advance_till_next_line(code, *line_starting_pos) {
            *line_starting_pos = new_start;
        }
    }
}

pub trait AttributeStatus<T> {
    type Attribute;
}
pub struct NotSet;
impl<T> AttributeStatus<T> for NotSet {
    // Not set, no data
    type Attribute = ();
}
pub struct Set;

impl<T> AttributeStatus<T> for Set {
    type Attribute = T;
}

/** A builder for syntax errors
Expects the user to set every field before building
*/
#[derive(Debug)]

pub struct SyntaxErrorBuilder<
    AreaStatus: AttributeStatus<CodeArea>,
    FileLocationStatus: AttributeStatus<String>,
    ErrorTypeStatus: AttributeStatus<ErrorType>,
    ErrorMsgStatus: AttributeStatus<String>,
    CodeStatus: AttributeStatus<String>,
> {
    area: AreaStatus::Attribute,
    file_location: FileLocationStatus::Attribute,
    error_type: ErrorTypeStatus::Attribute,
    error_msg: ErrorMsgStatus::Attribute,
    code: CodeStatus::Attribute
}

impl Default for SyntaxErrorBuilder<NotSet, NotSet, NotSet, NotSet, NotSet> {
    fn default() -> Self {
        Self::new()
    }
}

impl SyntaxErrorBuilder<NotSet, NotSet, NotSet, NotSet, NotSet> {
    /** Creates a new and empty [`SyntaxErrorBuilder`]
     */
    fn new() -> Self {
        Self {
            area: (),
            file_location: (),
            error_type: (),
            error_msg: (),
            code: ()
        }
    }
}

impl<
    FileLocationStatus: AttributeStatus<String>,
    ErrorTypeStatus: AttributeStatus<ErrorType>,
    ErrorMsgStatus: AttributeStatus<String>,
    CodeStatus: AttributeStatus<String>,
> SyntaxErrorBuilder<NotSet, FileLocationStatus, ErrorTypeStatus, ErrorMsgStatus, CodeStatus>
{
    /** Sets an error area
     */
    pub fn with_area(
        self,
        area: CodeArea,
    ) -> SyntaxErrorBuilder<Set, FileLocationStatus, ErrorTypeStatus, ErrorMsgStatus, CodeStatus> {
        SyntaxErrorBuilder {
            area,
            file_location: self.file_location,
            error_type: self.error_type,
            error_msg: self.error_msg,
            code: self.code
        }
    }
}

impl<
    AreaStatus: AttributeStatus<CodeArea>,
    ErrorTypeStatus: AttributeStatus<ErrorType>,
    ErrorMsgStatus: AttributeStatus<String>,
    CodeStatus: AttributeStatus<String>,
>
    SyntaxErrorBuilder<AreaStatus, NotSet, ErrorTypeStatus, ErrorMsgStatus, CodeStatus>
{
    /** Sets the path of the file that contains the erroneous code
          This is for display purposes only
    */
    pub fn with_file_location(
        self,
        file_location: String,
    ) -> SyntaxErrorBuilder<AreaStatus, Set, ErrorTypeStatus, ErrorMsgStatus, CodeStatus> {
        SyntaxErrorBuilder {
            area: self.area,
            file_location,
            error_type: self.error_type,
            error_msg: self.error_msg,
            code: self.code
        }
    }
}

impl<
    AreaStatus: AttributeStatus<CodeArea>,
    FileLocationStatus: AttributeStatus<String>,
    ErrorMsgStatus: AttributeStatus<String>,
    CodeStatus: AttributeStatus<String>,
>
    SyntaxErrorBuilder<AreaStatus, FileLocationStatus, NotSet, ErrorMsgStatus, CodeStatus>
{
    /** Sets the type of error
     */
    pub fn with_error_type(
        self,
        error_type: ErrorType,
    ) -> SyntaxErrorBuilder<AreaStatus, FileLocationStatus, Set, ErrorMsgStatus, CodeStatus> {
        SyntaxErrorBuilder {
            area: self.area,
            file_location: self.file_location,
            error_type,
            error_msg: self.error_msg,
            code: self.code
        }
    }
}

impl<
    AreaStatus: AttributeStatus<CodeArea>,
    FileLocationStatus: AttributeStatus<String>,
    ErrorTypeStatus: AttributeStatus<ErrorType>,
    CodeStatus: AttributeStatus<String>,
>
SyntaxErrorBuilder<AreaStatus, FileLocationStatus, ErrorTypeStatus, NotSet, CodeStatus>
{
    /** Sets the error message
     */
    pub fn with_error_msg(
        self,
        error_msg: String,
    ) -> SyntaxErrorBuilder<AreaStatus, FileLocationStatus, ErrorTypeStatus, Set, CodeStatus> {
        SyntaxErrorBuilder {
            area: self.area,
            file_location: self.file_location,
            error_type: self.error_type,
            error_msg,
            code: self.code
        }
    }
}

impl<
    AreaStatus: AttributeStatus<CodeArea>,
    FileLocationStatus: AttributeStatus<String>,
    ErrorTypeStatus: AttributeStatus<ErrorType>,
    ErrorMsgStatus: AttributeStatus<String>,
>
SyntaxErrorBuilder<AreaStatus, FileLocationStatus, ErrorTypeStatus, ErrorMsgStatus, NotSet>
{
    /** Sets the error message
     */
    pub fn with_code(
        self,
        code: String,
    ) -> SyntaxErrorBuilder<AreaStatus, FileLocationStatus, ErrorTypeStatus, ErrorMsgStatus, Set> {
        SyntaxErrorBuilder {
            area: self.area,
            file_location: self.file_location,
            error_type: self.error_type,
            error_msg: self.error_msg,
            code
        }
    }
}
impl SyntaxErrorBuilder<Set, Set, Set, Set, Set> {
    /** Builds the [`SyntaxError`]
    # Panics
    Panics if not all fields are set
    */
    pub fn build(self) -> SyntaxError {
        SyntaxError::new(self.area, self.file_location, self.error_type, self.error_msg, self.code)
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

#[derive(Debug)]
pub enum ErrorType {
    Error,
    Warning,
    Info
}

impl ErrorType
{
    fn as_report_kind(&self) -> ReportKind
    {
        match self
        {
            ErrorType::Error => ReportKind::Error,
            ErrorType::Warning => ReportKind::Warning,
            ErrorType::Info => ReportKind::Advice
        }
    }

    fn as_msg(&self) -> String
    {
        format!("A{} was found during compilation", match self
        {
            ErrorType::Error => " syntax error",
            ErrorType::Warning => " warning",
            ErrorType::Info => "n information"
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{CodeArea, CodeLocation, ErrorOutputConfig, ErrorType, SyntaxError};
    #[test]
    fn error() {
        let error = SyntaxError::builder()
            .with_area(CodeArea::new(CodeLocation::new(66, 10), CodeLocation::new(66, 18)).unwrap())
            .with_error_type(ErrorType::Error)
            .with_error_msg("CodeArea is an invalid data type at this point in the programm".to_string())
            .with_file_location("main.waso".to_string())
            .with_code(include_str!("lib.rs").to_string())
            .build();

        error.print_to_stderr_default_context();
    }

    #[test]
    fn error_long_context() {
        let error = SyntaxError::builder()
            .with_area(CodeArea::new(CodeLocation::new(66, 10), CodeLocation::new(66, 18)).unwrap())
            .with_error_type(ErrorType::Error)
            .with_error_msg("CodeArea is an invalid data type at this point in the programm".to_string())
            .with_file_location("main.waso".to_string())
            .with_code(include_str!("lib.rs").to_string())
            .build();

        error.write(ErrorOutputConfig::default().with_context(20));
    }

    #[test]
    fn error_multiline() {
        let error = SyntaxError::builder()
            .with_area(CodeArea::new(CodeLocation::new(2, 0), CodeLocation::new(4, 29)).unwrap())
            .with_error_type(ErrorType::Error)
            .with_error_msg("CodeArea is an invalid data type at this point in the programm".to_string())
            .with_file_location("main.waso".to_string())
            .with_code(include_str!("lib.rs").to_string())
            .build();

        error.print_to_stderr_default_context();
    }

    #[test]
    fn codearea_invalid_should_return_none() {
        let codearea = CodeArea::new(CodeLocation::new(10, 10), CodeLocation::new(5, 10));

        assert_eq!(None, codearea);

        let codearea = CodeArea::new(CodeLocation::new(10, 10), CodeLocation::new(10, 10));

        assert_eq!(None, codearea);
    }
}
