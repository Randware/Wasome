use std::borrow::Cow;
use logos::{Lexer, Logos};
use std::num::{ParseFloatError, ParseIntError};
use std::ops::Range;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct LexError {
    pub byte_pos: Range<usize>,
    pub inner: LexErrorType
}

impl LexError {
    fn from_lexer(lex: &mut Lexer<'_, TokenType>) -> Self {
        Self::from_lexer_and_type(lex, LexErrorType::InvalidToken(lex.slice().to_string()))
    }

    fn from_lexer_and_type(lex: &mut Lexer<'_, TokenType>, error_type: LexErrorType) -> Self {
        LexError {
            byte_pos: lex.span().into(),
            inner: error_type,
        }
    }
}
#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexErrorType {
    #[default]
    Unknown,
    InvalidToken(String),
    Int(String),
    Float(String),
    InvalidChar(String),
}

impl LexErrorType {
    pub fn to_string(self) -> String {
        match self {
            LexErrorType::Unknown => "Invalid Token".to_string(),
            LexErrorType::InvalidToken(tok) => format!("{tok} is not a valid token"),
            LexErrorType::Int(val) => format!("{val} is not a valid integer"),
            LexErrorType::Float(val) => format!("{val} is not a valid floating-point number"),
            LexErrorType::InvalidChar(val) => format!("{val} is not a valid character"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub span: Range<usize>,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error(LexError, LexError::from_lexer))]
#[logos(skip r"[ \t\r\f]+")]
pub enum TokenType {
    // Datatypes
    #[token("s8")]
    S8,
    #[token("s16")]
    S16,
    #[token("s32")]
    S32,
    #[token("s64")]
    S64,
    #[token("u8")]
    U8,
    #[token("u16")]
    U16,
    #[token("u32")]
    U32,
    #[token("u64")]
    U64,
    #[token("f32")]
    F32,
    #[token("f64")]
    F64,
    #[token("bool")]
    Bool,
    #[token("char")]
    Char,
    #[token("self")]
    SelfType,

    // Values
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r#""(\\.|[^\\"])*""#, |lex| lex.slice().to_string())]
    String(String),
    #[regex(r"\d+\.\d+", lex_float)]
    Decimal(f64),
    #[regex(r"\d+", lex_int)]
    Integer(i64),
    #[regex(r"'(\\.|[^\\'])'", char_callback)]
    CharLiteral(char),
    #[token("true")]
    True,
    #[token("false")]
    False,

    // Math Operators
    #[token("+")]
    Addition,
    #[token("-")]
    Subtraction,
    #[token("*")]
    Multiplication,
    #[token("%")]
    Modulo,

    // '/' is a bit extra, since we will be using it for both Math and Filepaths.
    #[token("/")]
    Slash,

    // Logic Operators
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessThanEqual,
    #[token(">=")]
    GreaterThanEqual,
    #[token("!=")]
    NotEqual,
    #[token("==")]
    Comparison,
    // L and R Shift is just < or > twice
    // Having it as its own token would interfere with the parsing of generics
    #[token("|")]
    BitOr,
    #[token("||")]
    Or,
    #[token("&")]
    BitAnd,
    #[token("&&")]
    And,
    #[token("!")]
    Not,

    // Brackets
    #[token("{")]
    OpenScope,
    #[token("}")]
    CloseScope,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    // Keywords
    #[token("fn")]
    Function,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("loop")]
    Loop,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("as")]
    As,
    #[token("pub")]
    Public,
    #[token("new")]
    New,
    #[token("import")]
    Import,
    #[token("let")]
    Let,

    // Symbols
    #[token("->")]
    Return,
    #[token("<-")]
    Assign,
    #[token("::")]
    PathSeparator,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[regex(r"\r?\n")]
    StatementSeparator,
    #[token(",")]
    ArgumentSeparator,
    #[regex(r"//[^\n]*", |lex| lex.slice().to_string())]
    Comment(String),
}

fn lex_float(lex: &mut Lexer<'_, TokenType>) -> Result<f64, LexError> {
    lex.slice().parse().map_err(|_| LexError::from_lexer_and_type(lex, LexErrorType::Float(lex.slice().to_string())))
}

fn lex_int(lex: &mut Lexer<'_, TokenType>) -> Result<i64, LexError> {
    lex.slice().parse().map_err(|_| LexError::from_lexer_and_type(lex, LexErrorType::Int(lex.slice().to_string())))
}

impl TokenType {
    pub fn token_to_string(&self) -> Cow<'_, str> {
        use TokenType::*;

        match self {
            // Primitive types
            S8 => "s8".into(),
            S16 => "s16".into(),
            S32 => "s32".into(),
            S64 => "s64".into(),
            U8 => "u8".into(),
            U16 => "u16".into(),
            U32 => "u32".into(),
            U64 => "u64".into(),
            F32 => "f32".into(),
            F64 => "f64".into(),
            Bool => "bool".into(),
            Char => "char".into(),
            SelfType => "self".into(),

            // Literals - require owned strings
            Identifier(s) | String(s) | Comment(s) => Cow::Borrowed(s),
            Decimal(f) => {
                let s = f.to_string();
                if s.contains('.') {
                    Cow::Owned(s)
                } else {
                    Cow::Owned(format!("{}.0", s))
                }
            }
            Integer(i) => Cow::Owned(i.to_string()),
            CharLiteral(c) => Cow::Owned(format!("'{}'", Self::escape_char(*c))),
            True => "true".into(),
            False => "false".into(),

            // Operators
            Addition => "+".into(),
            Subtraction => "-".into(),
            Multiplication => "*".into(),
            Modulo => "%".into(),
            Slash => "/".into(),
            LessThan => "<".into(),
            GreaterThan => ">".into(),
            LessThanEqual => "<=".into(),
            GreaterThanEqual => ">=".into(),
            NotEqual => "!=".into(),
            Comparison => "==".into(),
            BitOr => "|".into(),
            Or => "||".into(),
            BitAnd => "&".into(),
            And => "&&".into(),
            Not => "!".into(),

            // Delimiters
            OpenScope => "{".into(),
            CloseScope => "}".into(),
            OpenParen => "(".into(),
            CloseParen => ")".into(),
            Dot => ".".into(),
            Semicolon => ";".into(),
            PathSeparator => "::".into(),
            ArgumentSeparator => ",".into(),
            StatementSeparator => "".into(),

            // Keywords
            Function => "fn".into(),
            If => "if".into(),
            Else => "else".into(),
            Loop => "loop".into(),
            Struct => "struct".into(),
            Enum => "enum".into(),
            As => "as".into(),
            Public => "pub".into(),
            New => "new".into(),
            Import => "import".into(),
            Let => "let".into(),
            Return => "->".into(),
            Assign => "<-".into(),
        }
    }

    fn escape_char(c: char) -> String {
        match c {
            '\n' => "\\n".into(),
            '\t' => "\\t".into(),
            '\r' => "\\r".into(),
            '\0' => "\\0".into(),
            '\\' => "\\\\".into(),
            '\'' => "\\'".into(),
            _ => c.to_string(),
        }
    }
}

/**
This function is called when any character is detected.
It handles escape sequences and ensures that only valid characters are processed.
*/
fn char_callback(lex: &mut Lexer<TokenType>) -> Result<char, LexError> {
    let s = lex.slice();
    let content = &s[1..s.len() - 1];

    let num_chars = content.chars().count();
    if num_chars != 0 && num_chars != (1 + content.starts_with('\\') as usize) {
        return Err(LexError::from_lexer_and_type(lex, LexErrorType::InvalidChar(content.to_string())));
    }

    let mut chars = content.chars();
    let first_char = chars.next().unwrap();

    let value = match first_char {
        '\\' => {
            let second_char = chars.next().unwrap();
            match second_char {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '0' => '\0',
                '\\' => '\\',
                '\'' => '\'',
                '"' => '"',
                _ => return Err(LexError::from_lexer_and_type(lex, LexErrorType::InvalidChar(content.to_string()))),
            }
        }
        _ => first_char,
    };

    Ok(value)
}
