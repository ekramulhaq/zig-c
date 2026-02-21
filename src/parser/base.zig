const std = @import("std");
const lexer = @import("../lexer.zig");
const ast = @import("../ast.zig");
const type_system = @import("../type_system.zig");

const Token = lexer.Token;
const TokenType = lexer.TokenType;
const TypeSystem = type_system.TypeSystem;

pub const ParserBase = struct {
    tokens: []Token,
    pos: usize,
    allocator: std.mem.Allocator,
    type_system: *TypeSystem,

    pub fn init(tokens: []Token, allocator: std.mem.Allocator, ts: *TypeSystem) ParserBase {
        return .{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
            .type_system = ts,
        };
    }

    pub fn current(self: *ParserBase) ?Token {
        if (self.pos < self.tokens.len) return self.tokens[self.pos];
        return null;
    }

    pub fn advance(self: *ParserBase) void {
        self.pos += 1;
    }

    pub fn errorAt(self: *ParserBase, token: Token, msg: []const u8) anyerror {
        _ = self;
        std.debug.print("Error at line {}, col {}: {s} (got '{s}')\n", .{ token.line, token.col, msg, token.value });
        return error.ParseError;
    }

    pub fn expect(self: *ParserBase, token_type: TokenType, msg: []const u8) !void {
        if (self.current()) |t| {
            if (t.type == token_type) {
                self.advance();
                return;
            }
            return self.errorAt(t, msg);
        } else {
            std.debug.print("Error: {s} (got EOF)\n", .{msg});
            return error.UnexpectedEOF;
        }
    }

    pub fn consume(self: *ParserBase, token_type: TokenType) bool {
        if (self.current()) |t| {
            if (t.type == token_type) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    pub fn expectIdentifier(self: *ParserBase, msg: []const u8) anyerror![]const u8 {
        const token = self.current() orelse return error.UnexpectedEOF;
        if (token.type != .Identifier) return self.errorAt(token, msg);
        self.advance();
        return token.value;
    }

    pub fn parseType(self: *ParserBase) anyerror!ast.Type {
        const token = self.current() orelse return error.UnexpectedEOF;
        if (token.type == .IntKeyword) {
            self.advance();
            return .{ .id = .Int };
        } else if (token.type == .CharKeyword) {
            self.advance();
            return .{ .id = .Char };
        } else if (token.type == .VoidKeyword) {
            self.advance();
            return .{ .id = .Void };
        } else if (token.type == .StructKeyword) {
            self.advance();
            const name = try self.expectIdentifier("Expected struct name");
            return .{ .id = .Int, .struct_name = name };
        } else if (token.type == .Identifier) {
            if (self.type_system.typedefs.get(token.value)) |t| {
                self.advance();
                return t;
            }
        }
        return self.errorAt(token, "Expected type name");
    }
};
