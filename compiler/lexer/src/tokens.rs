use logos::Logos;
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexError {
    #[default]
    Unknown, 
    Int(ParseIntError),
    Float(ParseFloatError),
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
pub enum Token {
    #[regex(r"[ \t\f]+", logos::skip)] // <--- skip whitespace
    _Skip,
    // --- Datatypes ---
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

    // --- Values ---
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r"\d+\.\d+", |lex| lex.slice().parse().map_err(LexError::Float))]
    Decimal(f64),

    #[regex(r"\d+", |lex| lex.slice().parse().map_err(LexError::Int))]
    Integer(i64),

    // --- Math Operators ---
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

    // --- Logic Operators ---
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

    // --- Brackets ---
    #[token("{")]
    OpenScope,
    #[token("}")]
    CloseScope,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    // --- Keywords ---
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
    #[token("->")]
    Return,
    #[token("<-")]
    Assign,
    #[token("enum")]
    Enum,
    #[token("as")]
    As,
    #[token("pub")]
    Public,
    #[token("new")]
    New,
    #[token("::")]
    PathSeperator,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[regex(r"\r?\n")]
    StatementSeparator,

}
