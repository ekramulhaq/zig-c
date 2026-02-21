const std = @import("std");

/// Represents the type of a lexical token.
pub const TokenType = enum {
    Number,
    Identifier,
    
    // Arithmetic operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusPlus,
    MinusMinus,
    
    // Bitwise operators
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    LessLess,
    GreaterGreater,
    
    // Assignment
    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    
    // Comparison
    EqualEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    
    // Logical
    AmpersandAmpersand,
    PipePipe,
    Bang,
    
    // Delimiters
    Semicolon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Dot,
    Arrow,
    Question,
    Colon,
    
    // Keywords
    IntKeyword,
    CharKeyword,
    VoidKeyword,
    ReturnKeyword,
    IfKeyword,
    ElseKeyword,
    WhileKeyword,
    ForKeyword,
    DoKeyword,
    BreakKeyword,
    ContinueKeyword,
    SwitchKeyword,
    CaseKeyword,
    DefaultKeyword,
    StructKeyword,
    EnumKeyword,
    TypedefKeyword,
    SizeofKeyword,
    
    // Literals
    StringLiteral,
    
    EOF,
};

/// Represents a lexical token with its type, raw value, and source location.
pub const Token = struct {
    type: TokenType,
    value: []const u8,
    line: usize,
    col: usize,
};
