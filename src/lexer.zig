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
    Dot,
    Arrow,
    Question,
    Colon,
    EOF,
};

/// Represents a lexical token with a type and its raw text value.
pub const Token = struct {
    type: TokenType,
    value: []const u8,
    line: usize,
    col: usize,
};

pub const LexerError = error{
    InvalidCharacter,
};

/// Lexer converts source code into a stream of tokens.
pub const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: usize,
    col: usize,

    /// Initializes a new Lexer with the given source code.
    pub fn init(source: []const u8) Lexer {
        return Lexer{ .source = source, .pos = 0, .line = 1, .col = 1 };
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        if (c == '\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        return c;
    }

    fn peek(self: *Lexer) ?u8 {
        if (self.pos >= self.source.len) return null;
        return self.source[self.pos];
    }

    fn expectChar(self: *Lexer, c: u8, msg: []const u8) !void {
        if (self.peek() == c) {
            _ = self.advance();
        } else {
            std.debug.print("Lexer Error at line {}, col {}: {s}\n", .{ self.line, self.col, msg });
            return error.InvalidCharacter;
        }
    }

    /// Returns the next token from the source code.
    pub fn nextToken(self: *Lexer) LexerError!Token {
        while (self.pos < self.source.len) {
            const c = self.peek() orelse break;
            if (std.ascii.isWhitespace(c)) {
                _ = self.advance();
                continue;
            }
            // Skip comments
            if (self.pos + 1 < self.source.len and self.source[self.pos] == '/' and self.source[self.pos + 1] == '/') {
                _ = self.advance(); // /
                _ = self.advance(); // /
                while (self.pos < self.source.len and self.peek() != '\n') {
                    _ = self.advance();
                }
                continue;
            }
            break;
        }

        const start_line = self.line;
        const start_col = self.col;

        if (self.pos >= self.source.len) {
            return Token{ .type = .EOF, .value = "", .line = start_line, .col = start_col };
        }

        const c = self.advance();
        switch (c) {
            '+' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .PlusEqual, .value = "+=", .line = start_line, .col = start_col };
                }
                if (self.peek() == '+') {
                    _ = self.advance();
                    return Token{ .type = .PlusPlus, .value = "++", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Plus, .value = "+", .line = start_line, .col = start_col };
            },
            '-' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .MinusEqual, .value = "-=", .line = start_line, .col = start_col };
                }
                if (self.peek() == '-') {
                    _ = self.advance();
                    return Token{ .type = .MinusMinus, .value = "--", .line = start_line, .col = start_col };
                }
                if (self.peek() == '>') {
                    _ = self.advance();
                    return Token{ .type = .Arrow, .value = "->", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Minus, .value = "-", .line = start_line, .col = start_col };
            },
            '*' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .StarEqual, .value = "*=", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Star, .value = "*", .line = start_line, .col = start_col };
            },
            '/' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .SlashEqual, .value = "/=", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Slash, .value = "/", .line = start_line, .col = start_col };
            },
            '%' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .PercentEqual, .value = "%=", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Percent, .value = "%", .line = start_line, .col = start_col };
            },
            '&' => {
                if (self.peek() == '&') {
                    _ = self.advance();
                    return Token{ .type = .AmpersandAmpersand, .value = "&&", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Ampersand, .value = "&", .line = start_line, .col = start_col };
            },
            '|' => {
                if (self.peek() == '|') {
                    _ = self.advance();
                    return Token{ .type = .PipePipe, .value = "||", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Pipe, .value = "|", .line = start_line, .col = start_col };
            },
            '^' => return Token{ .type = .Caret, .value = "^", .line = start_line, .col = start_col },
            '~' => return Token{ .type = .Tilde, .value = "~", .line = start_line, .col = start_col },
            '>' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .GreaterEqual, .value = ">=", .line = start_line, .col = start_col };
                }
                if (self.peek() == '>') {
                    _ = self.advance();
                    return Token{ .type = .GreaterGreater, .value = ">>", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Greater, .value = ">", .line = start_line, .col = start_col };
            },
            '<' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .LessEqual, .value = "<=", .line = start_line, .col = start_col };
                }
                if (self.peek() == '<') {
                    _ = self.advance();
                    return Token{ .type = .LessLess, .value = "<<", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Less, .value = "<", .line = start_line, .col = start_col };
            },
            ';' => return Token{ .type = .Semicolon, .value = ";", .line = start_line, .col = start_col },
            '=' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .EqualEqual, .value = "==", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Equal, .value = "=", .line = start_line, .col = start_col };
            },
            '!' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .NotEqual, .value = "!=", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Bang, .value = "!", .line = start_line, .col = start_col };
            },
            '(' => return Token{ .type = .LParen, .value = "(", .line = start_line, .col = start_col },
            ')' => return Token{ .type = .RParen, .value = ")", .line = start_line, .col = start_col },
            '{' => return Token{ .type = .LBrace, .value = "{", .line = start_line, .col = start_col },
            '}' => return Token{ .type = .RBrace, .value = "}", .line = start_line, .col = start_col },
            ',' => return Token{ .type = .Comma, .value = ",", .line = start_line, .col = start_col },
            '[' => return Token{ .type = .LBracket, .value = "[", .line = start_line, .col = start_col },
            ']' => return Token{ .type = .RBracket, .value = "]", .line = start_line, .col = start_col },
            '.' => return Token{ .type = .Dot, .value = ".", .line = start_line, .col = start_col },
            '?' => return Token{ .type = .Question, .value = "?", .line = start_line, .col = start_col },
            ':' => return Token{ .type = .Colon, .value = ":", .line = start_line, .col = start_col },
            '\'' => {
                var val: i64 = 0;
                if (self.peek() == '\\') {
                    _ = self.advance();
                    const escape = self.advance();
                    val = switch (escape) {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '\'' => '\'',
                        '0' => 0,
                        else => escape,
                    };
                } else {
                    val = self.advance();
                }
                try self.expectChar('\'', "Expected ' after character literal");
                // Convert value to string to keep Token struct consistent.
                // Using a small buffer for numeric value
                var buf: [32]u8 = undefined;
                const val_str = std.fmt.bufPrint(&buf, "{}", .{val}) catch return error.InvalidCharacter;
                // We need to duplicate the string because the buffer is local
                const duplicated = std.heap.page_allocator.dupe(u8, val_str) catch return error.InvalidCharacter;
                return Token{ .type = .Number, .value = duplicated, .line = start_line, .col = start_col };
            },
            '"' => {
                const start_pos = self.pos;
                while (self.peek()) |pc| {
                    if (pc == '"') break;
                    _ = self.advance();
                }
                const value = self.source[start_pos..self.pos];
                if (self.peek() == '"') _ = self.advance(); // skip closing quote
                return Token{ .type = .StringLiteral, .value = value, .line = start_line, .col = start_col };
            },
            else => {
                if (std.ascii.isDigit(c)) {
                    const start_pos = self.pos - 1;
                    while (self.peek()) |pc| {
                        if (!std.ascii.isDigit(pc)) break;
                        _ = self.advance();
                    }
                    return Token{ .type = .Number, .value = self.source[start_pos..self.pos], .line = start_line, .col = start_col };
                }
                if (std.ascii.isAlphabetic(c)) {
                    const start_pos = self.pos - 1;
                    while (self.peek()) |pc| {
                        if (!std.ascii.isAlphanumeric(pc) and pc != '_') break;
                        _ = self.advance();
                    }
                    const value = self.source[start_pos..self.pos];
                    if (std.mem.eql(u8, value, "int")) return Token{ .type = .IntKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "char")) return Token{ .type = .CharKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "void")) return Token{ .type = .VoidKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "return")) return Token{ .type = .ReturnKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "if")) return Token{ .type = .IfKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "else")) return Token{ .type = .ElseKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "while")) return Token{ .type = .WhileKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "for")) return Token{ .type = .ForKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "do")) return Token{ .type = .DoKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "break")) return Token{ .type = .BreakKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "continue")) return Token{ .type = .ContinueKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "switch")) return Token{ .type = .SwitchKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "case")) return Token{ .type = .CaseKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "default")) return Token{ .type = .DefaultKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "struct")) return Token{ .type = .StructKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "enum")) return Token{ .type = .EnumKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "typedef")) return Token{ .type = .TypedefKeyword, .value = value, .line = start_line, .col = start_col };
                    return Token{ .type = .Identifier, .value = value, .line = start_line, .col = start_col };
                }
                return error.InvalidCharacter;
            },
        }
    }
};
