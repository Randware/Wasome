use logos::Logos;
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexError {
    #[default]
    Unknown,
    Int(ParseIntError),
    Float(ParseFloatError),
    InvalidChar(String),
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexError)]
#[logos(skip r"[\t\f]+")]
pub enum Token {
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

    #[regex(r"\d+\.\d+", |lex| lex.slice().parse().map_err(LexError::Float))]
    Decimal(f64),

    #[regex(r"\d+", |lex| lex.slice().parse().map_err(LexError::Int))]
    Integer(i64),

    #[regex(r"'(\\.|[^\\'])'", parse_char_literal)]
    CharLiteral(char),

    // Math Operators
    #[token("+")]
    Addition,
    #[token("-")]
    Subtraction,
    #[token("*")]
    Multiplication,
    #[token("/")]
    Division,
    #[token("%")]
    Modulo,

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
    #[token("<<")]
    LShift,
    #[token(">>")]
    RShift,
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

    //Keywords
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
}

fn parse_char_literal(lex: &mut logos::Lexer<Token>) -> Result<char, LexError> {
    let s = lex.slice();
    let content = &s[1..s.len() - 1];

    let num_chars = content.chars().count();
    if num_chars != 0 && num_chars != (1 + content.starts_with('\\') as usize) {
        return Err(LexError::InvalidChar(content.to_string()));
    }

    let mut chars = content.chars();
    let first_char = chars.next().unwrap();
    Ok(match first_char {
        '\\' => {
            let second_char = chars.next().unwrap();
            match second_char {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '0' => '\0',
                '\\' => '\\',
                _ => return Result::Err(LexError::InvalidChar(content.to_string())),
            }
        }
        _ => first_char,
    })
}
