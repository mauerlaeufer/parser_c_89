use logos::Logos;
use std::fmt; // to implement the Display trait
use std::num::ParseIntError;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
// this skips certain tokens.
#[logos(
    //Whitespace is skipped
    skip r"[ \t\n\f]+", 
    // Single line comments with #
    skip r"\/\/.*\n?",
    // This allows multi line comments with /* ... */
    skip r"\/\*[^*]*(?:\*[^/*][^*]*)*\*\/",
    error = LexicalError)]
pub enum Token {
    #[token("return")]
    KeywordReturn,
    #[token("...")]
    KeywordEllipsis,
    #[token("enum")]
    KeywordEnum,
    #[token("struct")]
    KeywordStruct,
    #[token("union")]
    KeywordUnion,
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,
    #[token("while")]
    KeywordWhile,
    #[token("do")]
    KeywordDo,
    #[token("typedef")]
    KeywordTypeDef,
    #[token("for")]
    KeywordFor,
    #[token("extern")]
    KeywordExtern,
    #[token("static")]
    KeywordStatic,
    #[token("auto")]
    KeywordAuto,
    #[token("break")]
    KeywordBreak,
    #[token("register")]
    KeywordRegister,
    #[token("case")]
    KeywordCase,
    #[token("default")]
    KeywordDefault,
    #[token("continue")]
    KeywordContinue,
    #[token("switch")]
    KeywordSwitch,
    #[token("goto")]
    KeywordGoto,

    // Type Specifier
    #[token("void")]
    KeywordVoid,
    #[token("char")]
    KeywordChar,
    #[token("short")]
    KeywordShort,
    #[token("int")]
    KeywordInt,
    #[token("long")]
    KeywordLong,
    #[token("float")]
    KeywordFloat,
    #[token("double")]
    KeywordDouble,
    #[token("signed")]
    KeywordSigned,
    #[token("unsigned")]
    KeywordUnsigned,

    #[token("sizeof")]
    KeywordSizeof,


    // Type Qualifier
    #[token("const")]
    KeywordConst,
    #[token("volatile")]
    KeywordVolatile,

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("\"[^\"]*\"", |lex| lex.slice().to_string())]
    StringLiteral(String),
    #[regex("'[^']'", |lex| lex.slice().chars().nth(1).unwrap())]
    CharLiteral(char),
    #[regex("0|[1-9][0-9]*", |lex| lex.slice().parse())]
    Integer(i64),
    // Pattern to match octal numbers (starting with 0 followed by digits 0-7)
    #[regex(r"0[0-7]+", |lex| i64::from_str_radix(lex.slice(), 8))]
    Octal(i64),
    // Pattern to match hexadecimal numbers (starting with 0x or 0X followed by digits 0-9 or a-f or A-F)
    #[regex(r"0[xX][0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16))]
    Hexadecimal(i64),
    // Note: This isn't handwritten and may contain errors.
    #[regex(r"[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[fFlL]?|[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?[fFlL]?|[0-9]+([eE][+-]?[0-9]+)[fFlL]?"
    , |lex| lex.slice().to_string())]
    Float(String),
    
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    // Special Characters
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("?")]
    QuestionMark,

    #[token("||")]
    LogicalOr,
    #[token("^")]
    Xor,
    #[token("&&")]
    LogicalAnd,
    #[token("!")]
    Not,
    #[token("|")]
    BitwiseOr,
    #[token("~")]
    BitwiseNot,
    #[token("&")]
    BitwiseAnd,
    #[token("++")]
    Increment,
    #[token("<<")]
    LeftOp,
    #[token(">>")]
    RightOp,
    #[token("--")]
    Decrement,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("/=")]
    DivideAssign,
    #[token("*=")]
    MultiplyAssign,
    #[token("%=")]
    ModAssign,
    #[token(">=")]
    GreaterEqual,
    #[token("<=")]
    LesserEqual,
    #[token("&=")]
    AndAssign,
    #[token("<<=")]
    LeftShiftAssign,
    #[token(">>=")]
    RightShiftAssign,
    #[token("^=")]
    XORAssign,
    #[token("|=")]
    ORAssign,



    // Mathy Operators
    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,
    #[token("%")]
    OperatorModulo,
    #[token("==")]
    OperatorEqual,
    #[token("!=")]
    OperatorNotEqual,
    #[token("<")]
    OperatorLesser,
    #[token(">")]
    OperatorGreater,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
