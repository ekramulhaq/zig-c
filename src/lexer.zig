const std = @import("std");
const token_pkg = @import("token.zig");

pub const Token = token_pkg.Token;
pub const TokenType = token_pkg.TokenType;

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
            if (self.pos + 1 < self.source.len and self.source[self.pos] == '/') {
                if (self.source[self.pos + 1] == '/') {
                    // Single-line comment
                    _ = self.advance(); // /
                    _ = self.advance(); // /
                    while (self.pos < self.source.len and self.peek() != '\n') {
                        _ = self.advance();
                    }
                    continue;
                } else if (self.source[self.pos + 1] == '*') {
                    // Block comment /* ... */
                    _ = self.advance(); // /
                    _ = self.advance(); // *
                    while (self.pos + 1 < self.source.len) {
                        if (self.source[self.pos] == '*' and self.source[self.pos + 1] == '/') {
                            _ = self.advance(); // *
                            _ = self.advance(); // /
                            break;
                        }
                        _ = self.advance();
                    }
                    continue;
                }
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
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .AmpersandEqual, .value = "&=", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Ampersand, .value = "&", .line = start_line, .col = start_col };
            },
            '|' => {
                if (self.peek() == '|') {
                    _ = self.advance();
                    return Token{ .type = .PipePipe, .value = "||", .line = start_line, .col = start_col };
                }
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .PipeEqual, .value = "|=", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Pipe, .value = "|", .line = start_line, .col = start_col };
            },
            '^' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .CaretEqual, .value = "^=", .line = start_line, .col = start_col };
                }
                return Token{ .type = .Caret, .value = "^", .line = start_line, .col = start_col };
            },
            '~' => return Token{ .type = .Tilde, .value = "~", .line = start_line, .col = start_col },
            '>' => {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return Token{ .type = .GreaterEqual, .value = ">=", .line = start_line, .col = start_col };
                }
                if (self.peek() == '>') {
                    _ = self.advance();
                    if (self.peek() == '=') {
                        _ = self.advance();
                        return Token{ .type = .GreaterGreaterEqual, .value = ">>=", .line = start_line, .col = start_col };
                    }
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
                    if (self.peek() == '=') {
                        _ = self.advance();
                        return Token{ .type = .LessLessEqual, .value = "<<=", .line = start_line, .col = start_col };
                    }
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
            '.' => {
                if (self.peek() == '.') {
                    _ = self.advance();
                    if (self.peek() == '.') {
                        _ = self.advance();
                        return Token{ .type = .Ellipsis, .value = "...", .line = start_line, .col = start_col };
                    }
                }
                return Token{ .type = .Dot, .value = ".", .line = start_line, .col = start_col };
            },
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
            '\"' => {
                var str_buf = std.ArrayList(u8).init(std.heap.page_allocator);
                while (self.peek()) |pc| {
                    if (pc == '\"') break;
                    if (pc == '\\') {
                        _ = self.advance(); // skip backslash
                        const esc = self.advance();
                        switch (esc) {
                            'n' => str_buf.append('\n') catch return error.InvalidCharacter,
                            't' => str_buf.append('\t') catch return error.InvalidCharacter,
                            'r' => str_buf.append('\r') catch return error.InvalidCharacter,
                            '\\' => str_buf.append('\\') catch return error.InvalidCharacter,
                            '"' => str_buf.append('"') catch return error.InvalidCharacter,
                            '0' => str_buf.append(0) catch return error.InvalidCharacter,
                            'a' => str_buf.append(7) catch return error.InvalidCharacter,
                            'b' => str_buf.append(8) catch return error.InvalidCharacter,
                            'f' => str_buf.append(12) catch return error.InvalidCharacter,
                            else => str_buf.append(esc) catch return error.InvalidCharacter,
                        }
                    } else {
                        str_buf.append(self.advance()) catch return error.InvalidCharacter;
                    }
                }
                if (self.peek() == '"') _ = self.advance(); // skip closing quote
                const value = str_buf.toOwnedSlice() catch return error.InvalidCharacter;
                return Token{ .type = .StringLiteral, .value = value, .line = start_line, .col = start_col };
            },
            else => {
                if (std.ascii.isDigit(c)) {
                    const start_pos = self.pos - 1;
                    // Check for hex (0x/0X) or octal (0) prefix
                    if (c == '0' and self.peek() != null) {
                        const next = self.peek().?;
                        if (next == 'x' or next == 'X') {
                            // Hex literal: 0xFF
                            _ = self.advance(); // skip 'x'
                            while (self.peek()) |pc| {
                                if (!std.ascii.isHex(pc)) break;
                                _ = self.advance();
                            }
                            const hex_str = self.source[(start_pos + 2)..self.pos]; // skip "0x"
                            const val = std.fmt.parseInt(i64, hex_str, 16) catch 0;
                            var buf: [32]u8 = undefined;
                            const val_str = std.fmt.bufPrint(&buf, "{}", .{val}) catch return error.InvalidCharacter;
                            const duplicated = std.heap.page_allocator.dupe(u8, val_str) catch return error.InvalidCharacter;
                            return Token{ .type = .Number, .value = duplicated, .line = start_line, .col = start_col };
                        } else if (std.ascii.isDigit(next) and next != '8' and next != '9') {
                            // Octal literal: 077
                            while (self.peek()) |pc| {
                                if (!std.ascii.isDigit(pc) or pc == '8' or pc == '9') break;
                                _ = self.advance();
                            }
                            // Check if it's actually a float like 0.5
                            if (self.peek() == '.') {
                                // Fall through to float parsing below
                            } else {
                                const oct_str = self.source[(start_pos + 1)..self.pos]; // skip leading '0'
                                const val = std.fmt.parseInt(i64, oct_str, 8) catch 0;
                                var buf: [32]u8 = undefined;
                                const val_str = std.fmt.bufPrint(&buf, "{}", .{val}) catch return error.InvalidCharacter;
                                const duplicated = std.heap.page_allocator.dupe(u8, val_str) catch return error.InvalidCharacter;
                                return Token{ .type = .Number, .value = duplicated, .line = start_line, .col = start_col };
                            }
                        }
                    }
                    var is_float = false;
                    while (self.peek()) |pc| {
                        if (pc == '.') {
                            if (is_float) break;
                            is_float = true;
                            _ = self.advance();
                            continue;
                        }
                        if (!std.ascii.isDigit(pc)) break;
                        _ = self.advance();
                    }
                    if (is_float) {
                        return Token{ .type = .FloatLiteral, .value = self.source[start_pos..self.pos], .line = start_line, .col = start_col };
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
                    if (std.mem.eql(u8, value, "float")) return Token{ .type = .FloatKeyword, .value = value, .line = start_line, .col = start_col };
                    if (std.mem.eql(u8, value, "double")) return Token{ .type = .DoubleKeyword, .value = value, .line = start_line, .col = start_col };
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
                    if (std.mem.eql(u8, value, "sizeof")) return Token{ .type = .SizeofKeyword, .value = value, .line = start_line, .col = start_col };
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
