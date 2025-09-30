use logos::Logos;
#[derive(Logos, Debug)]
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
    #[regex("[a-zA-Z]+")]
    Identifier,
    #[regex(r"\d+\.\d+")]
    Decimal,
    #[regex(r"\d+")]
    Integer,
    
    // Math-Operators
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
    
    // Logic-Operators
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
    #[token("=")] // noch zu besprechen ob "=" oder "=="
    Comparison,
    #[token("<<")]
    LShift,
    #[token(">>")]
    RShift,
    #[token("|")]
    BitOr,
    #[token("&")]
    BitAnd,
    #[token("||")]
    Or,
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
}
