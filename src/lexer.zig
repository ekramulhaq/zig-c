const std = @import("std");

/// Represents the type of a token.
pub const TokenType = enum {
    Number,
    Identifier,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    LessLess,
    GreaterGreater,
    Semicolon,
    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    EqualEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    AmpersandAmpersand,
    PipePipe,
    Bang,
    IntKeyword,
    ReturnKeyword,
    IfKeyword,
    ElseKeyword,
    WhileKeyword,
    ForKeyword,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    StringLiteral,
    PlusPlus,
    MinusMinus,
    LBracket,
    RBracket,
    EOF,
};

/// Represents a lexical token with a type and its raw text value.
pub const Token = struct {
    type: TokenType,
    value: []const u8,
};

/// Lexer converts source code into a stream of tokens.
pub const Lexer = struct {
    source: []const u8,
    pos: usize,

    /// Initializes a new Lexer with the given source code.
    pub fn init(source: []const u8) Lexer {
        return Lexer{ .source = source, .pos = 0 };
    }

    /// Returns the next token from the source code.
    pub fn nextToken(self: *Lexer) Token {
        while (self.pos < self.source.len) {
            if (std.ascii.isWhitespace(self.source[self.pos])) {
                self.pos += 1;
                continue;
            }
            // Skip comments
            if (self.pos + 1 < self.source.len and self.source[self.pos] == '/' and self.source[self.pos + 1] == '/') {
                self.pos += 2;
                while (self.pos < self.source.len and self.source[self.pos] != '\n') {
                    self.pos += 1;
                }
                continue;
            }
            break;
        }
        if (self.pos >= self.source.len) {
            return Token{ .type = .EOF, .value = "" };
        }
        const c = self.source[self.pos];
        self.pos += 1;
        switch (c) {
            '+' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .PlusEqual, .value = "+=" };
                }
                if (self.pos < self.source.len and self.source[self.pos] == '+') {
                    self.pos += 1;
                    return Token{ .type = .PlusPlus, .value = "++" };
                }
                return Token{ .type = .Plus, .value = "+" };
            },
            '-' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .MinusEqual, .value = "-=" };
                }
                if (self.pos < self.source.len and self.source[self.pos] == '-') {
                    self.pos += 1;
                    return Token{ .type = .MinusMinus, .value = "--" };
                }
                return Token{ .type = .Minus, .value = "-" };
            },
            '*' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .StarEqual, .value = "*=" };
                }
                return Token{ .type = .Star, .value = "*" };
            },
            '/' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .SlashEqual, .value = "/=" };
                }
                return Token{ .type = .Slash, .value = "/" };
            },
            '%' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .PercentEqual, .value = "%=" };
                }
                return Token{ .type = .Percent, .value = "%" };
            },
            '&' => {
                if (self.pos < self.source.len and self.source[self.pos] == '&') {
                    self.pos += 1;
                    return Token{ .type = .AmpersandAmpersand, .value = "&&" };
                }
                return Token{ .type = .Ampersand, .value = "&" };
            },
            '|' => {
                if (self.pos < self.source.len and self.source[self.pos] == '|') {
                    self.pos += 1;
                    return Token{ .type = .PipePipe, .value = "||" };
                }
                return Token{ .type = .Pipe, .value = "|" };
            },
            '^' => return Token{ .type = .Caret, .value = "^" },
            '~' => return Token{ .type = .Tilde, .value = "~" },
            '>' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .GreaterEqual, .value = ">=" };
                }
                if (self.pos < self.source.len and self.source[self.pos] == '>') {
                    self.pos += 1;
                    return Token{ .type = .GreaterGreater, .value = ">>" };
                }
                return Token{ .type = .Greater, .value = ">" };
            },
            '<' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .LessEqual, .value = "<=" };
                }
                if (self.pos < self.source.len and self.source[self.pos] == '<') {
                    self.pos += 1;
                    return Token{ .type = .LessLess, .value = "<<" };
                }
                return Token{ .type = .Less, .value = "<" };
            },
            ';' => return Token{ .type = .Semicolon, .value = ";" },
            '=' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .EqualEqual, .value = "==" };
                }
                return Token{ .type = .Equal, .value = "=" };
            },
            '!' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .NotEqual, .value = "!=" };
                }
                return Token{ .type = .Bang, .value = "!" };
            },
            '(' => return Token{ .type = .LParen, .value = "(" },
            ')' => return Token{ .type = .RParen, .value = ")" },
            '{' => return Token{ .type = .LBrace, .value = "{" },
            '}' => return Token{ .type = .RBrace, .value = "}" },
            ',' => return Token{ .type = .Comma, .value = "," },
            '[' => return Token{ .type = .LBracket, .value = "[" },
            ']' => return Token{ .type = .RBracket, .value = "]" },
            '"' => {
                const start = self.pos;
                while (self.pos < self.source.len and self.source[self.pos] != '"') {
                    self.pos += 1;
                }
                const value = self.source[start..self.pos];
                if (self.pos < self.source.len) self.pos += 1; // skip closing quote
                return Token{ .type = .StringLiteral, .value = value };
            },
            else => {
                if (std.ascii.isDigit(c)) {
                    const start = self.pos - 1;
                    while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
                        self.pos += 1;
                    }
                    return Token{ .type = .Number, .value = self.source[start..self.pos] };
                }
                if (std.ascii.isAlphabetic(c)) {
                    const start = self.pos - 1;
                    while (self.pos < self.source.len and (std.ascii.isAlphanumeric(self.source[self.pos]) or self.source[self.pos] == '_')) {
                        self.pos += 1;
                    }
                    const value = self.source[start..self.pos];
                    if (std.mem.eql(u8, value, "int")) return Token{ .type = .IntKeyword, .value = value };
                    if (std.mem.eql(u8, value, "return")) return Token{ .type = .ReturnKeyword, .value = value };
                    if (std.mem.eql(u8, value, "if")) return Token{ .type = .IfKeyword, .value = value };
                    if (std.mem.eql(u8, value, "else")) return Token{ .type = .ElseKeyword, .value = value };
                    if (std.mem.eql(u8, value, "while")) return Token{ .type = .WhileKeyword, .value = value };
                    if (std.mem.eql(u8, value, "for")) return Token{ .type = .ForKeyword, .value = value };
                    return Token{ .type = .Identifier, .value = value };
                }
                @panic("Invalid character");
            },
        }
    }
};
