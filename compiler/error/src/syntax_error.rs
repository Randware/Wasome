use std::ops::Add;
use colored::{Color, Colorize};

pub struct SyntaxError
{
    start: CodeLocation,
    end: CodeLocation,
    file_location: String,
    error_type: Box<dyn ErrorType>
}

impl SyntaxError
{
    fn new(start: CodeLocation, end: CodeLocation, file_location: String, error_type: Box<dyn ErrorType>) -> Self {
        Self { start, end, file_location, error_type }
    }
}

pub struct SyntaxErrorBuilder
{
    start: Option<CodeLocation>,
    end: Option<CodeLocation>,
    file_location: Option<String>,
    error_type: Option<Box<dyn ErrorType>>
}

impl SyntaxErrorBuilder
{
    pub fn new() -> Self {
        Self { start: None, end: None, file_location: None, error_type: None }
    }

    pub fn with_start(mut self, start: CodeLocation) -> Self
    {
        self.start = Some(start);
        self
    }

    pub fn with_end(mut self, end: CodeLocation) -> Self
    {
        self.end = Some(end);
        self
    }

    pub fn with_file_location(mut self, file_location: String) -> Self
    {
        self.file_location = Some(file_location);
        self
    }

    pub fn with_error_type(mut self, error_type: impl ErrorType+'static) -> Self
    {
        self.error_type = Some(Box::new(error_type));
        self
    }

    pub fn build(self) -> SyntaxError
    {
        if self.file_location.is_none() ||
            self.error_type.is_none() ||
            self.start.is_none() ||
            self.end.is_none()
        {
            panic!("Not all fields are set!");
        }
        let error = SyntaxError::new(self.start.unwrap(),
                         self.end.unwrap(),
                         self.file_location.unwrap(),
                         self.error_type.unwrap());
        if error.start.line > error.end.line
            || (error.start.line == error.end.line && error.start.char >= error.end.char)
        {
            panic!("The error does not include at least parts of one line!");
        }
        error
    }
}

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

impl SyntaxError
{
    pub fn print(&self, code: &str)
    {
        println!("{} {}", "Error:".red(), self.error_type.to_string());
        println!();
        println!("{}{}{}", "[".bright_black().bold(), self.file_location.bright_black().bold(), "]".bright_black().bold());

        let display_end = self.end.line()+3;
        let display_start = if self.start.line() < 3 {0} else {self.start.line()-3};
        let error_start_line = self.start.line();
        let error_end_line = self.end.line();
        for (index, line) in code.lines().enumerate()
        {
            if index < display_start || index > display_end
            {
                continue;
            }

            let error_start_char =
            if index < error_start_line {line.len()} //Before the error lines, so it doesn't begin at all
            else if index == error_start_line {self.start.char()}
            else {0};

            let error_end_char =
                if index > error_end_line {0} //After the error lines, so it end immediately
                else if index == error_end_line {self.end.char()}
                else {line.len()};

            print!("{}", index.to_string().add(": ").bright_blue().bold());
            if error_start_char != 0
            {
                print!("{}", &line[0..error_start_char]);
            }
            print!("{}", &line[error_start_char..error_end_char].color(Color::TrueColor {
                r: 255,
                g: 127,
                b: 0,
            }));
            if error_end_char != line.len()
            {
                print!("{}", &line[error_end_char..line.len()]);
            }
            println!()
        }

    }
}

pub trait ErrorType
{
    fn to_string(&self) -> String;

}


#[cfg(test)]
mod tests {
    use crate::syntax_error::{CodeLocation, ErrorType, SyntaxErrorBuilder};

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
            .with_start(CodeLocation::new(6,8))
            .with_end(CodeLocation::new(6,21))
            .with_error_type(ExampleError("CodeLocation".to_string()))
            .with_file_location("main.waso".to_string())
            .build();

        error.print(include_str!("syntax_error.rs"));
    }
}