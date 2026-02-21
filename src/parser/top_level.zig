const std = @import("std");
const ast = @import("../ast.zig");
const token_pkg = @import("../token.zig");
const base = @import("base.zig");
const expr_parser = @import("expr.zig");
const stmt_parser = @import("stmt.zig");

const Node = ast.Node;
const TokenType = token_pkg.TokenType;
const ParserBase = base.ParserBase;
const ExprParser = expr_parser.ExprParser;
const StmtParser = stmt_parser.StmtParser;

pub const TopLevelParser = struct {
    base: *ParserBase,
    expr_p: *ExprParser,
    stmt_p: *StmtParser,

    pub fn init(p_base: *ParserBase, ep: *ExprParser, sp: *StmtParser) TopLevelParser {
        return .{ .base = p_base, .expr_p = ep, .stmt_p = sp };
    }

    pub fn parseProgram(self: *TopLevelParser) anyerror![]*Node {
        var items = std.ArrayList(*Node).init(self.base.allocator);
        while (self.base.current()) |token| {
            if (token.type == .EOF) break;
            
            if (token.type == .StructKeyword) {
                const i = self.base.pos + 2;
                if (i < self.base.tokens.len and self.base.tokens[i].type == .LBrace) {
                    try items.append(try self.parseStruct());
                    continue;
                }
            }

            var is_func = false;
            var i = self.base.pos + 1;
            if (self.base.tokens[self.base.pos].type == .StructKeyword) {
                i += 1;
            }
            while (i < self.base.tokens.len and self.base.tokens[i].type == .Star) : (i += 1) {}
            i += 1; // skip identifier
            if (i < self.base.tokens.len and self.base.tokens[i].type == .LParen) {
                is_func = true;
            }

            if (is_func) {
                try items.append(try self.parseFunction());
            } else {
                if (try self.stmt_p.parseStmt()) |stmt| {
                    try items.append(stmt);
                }
            }
        }
        return try items.toOwnedSlice();
    }

    fn parseStruct(self: *TopLevelParser) anyerror!*Node {
        try self.base.expect(.StructKeyword, "Expected 'struct'");
        const ident = try self.base.expectIdentifier("Expected struct name");
        try self.base.expect(.LBrace, "Expected '{' after struct name");
        var members = std.ArrayList(*Node).init(self.base.allocator);
        while (self.base.current()) |t| {
            if (t.type == .RBrace) break;
            if (try self.stmt_p.parseStmt()) |stmt| {
                try members.append(stmt);
            }
        }
        try self.base.expect(.RBrace, "Expected '}' after struct members");
        try self.base.expect(.Semicolon, "Expected ';' after struct definition");
        
        const member_slice = try members.toOwnedSlice();
        try self.base.type_system.addStruct(ident, member_slice);
        
        const node = try self.base.allocator.create(Node);
        node.* = Node{ .type = .StructDecl, .name = ident, .members = member_slice };
        return node;
    }

    fn parseFunction(self: *TopLevelParser) anyerror!*Node {
        const ret_token = self.base.current() orelse return error.UnexpectedEOF;
        var ret_type: ast.DataType = .Int;
        var ret_struct_name: ?[]const u8 = null;
        if (ret_token.type == .StructKeyword) {
            self.base.advance();
            ret_struct_name = try self.base.expectIdentifier("Expected struct name");
        } else {
            const t = try self.base.parseType();
            ret_type = t.id;
            ret_struct_name = t.struct_name;
        }

        var ret_pointer_level: usize = 0;
        while (self.base.consume(.Star)) { ret_pointer_level += 1; }
        const ret_is_pointer = (ret_pointer_level > 0);

        const ident = try self.base.expectIdentifier("Expected function name");
        try self.base.expect(.LParen, "Expected ( after function name");
        var params = std.ArrayList([]const u8).init(self.base.allocator);
        var params_types = std.ArrayList(ast.DataType).init(self.base.allocator);
        var params_is_pointer = std.ArrayList(bool).init(self.base.allocator);
        var params_struct_names = std.ArrayList(?[]const u8).init(self.base.allocator);
        if (!self.base.consume(.RParen)) {
            while (true) {
                const param_type_token = self.base.current() orelse return error.UnexpectedEOF;
                var param_type: ast.DataType = .Int;
                var param_struct_name: ?[]const u8 = null;
                if (param_type_token.type == .StructKeyword) {
                    self.base.advance();
                    param_struct_name = try self.base.expectIdentifier("Expected struct name");
                } else {
                    const t = try self.base.parseType();
                    param_type = t.id;
                    param_struct_name = t.struct_name;
                }
                try params_types.append(param_type);
                try params_struct_names.append(param_struct_name);

                var param_pointer_level: usize = 0;
                while (self.base.consume(.Star)) { param_pointer_level += 1; }
                try params_is_pointer.append(param_pointer_level > 0);

                try params.append(try self.base.expectIdentifier("Expected parameter name"));
                if (!self.base.consume(.Comma)) break;
            }
            try self.base.expect(.RParen, "Expected ) after function parameters");
        }
        try self.base.expect(.LBrace, "Expected { to start function body");
        var body = std.ArrayList(*Node).init(self.base.allocator);
        while (self.base.current()) |t| {
            if (t.type == .RBrace) break;
            if (try self.stmt_p.parseStmt()) |stmt| {
                try body.append(stmt);
            }
        }
        try self.base.expect(.RBrace, "Expected } to end function body");
        const node = try self.base.allocator.create(Node);
        node.* = Node{ .type = .Function, .data_type = ret_type, .is_pointer = ret_is_pointer, .struct_name = ret_struct_name, .name = ident, .params = try params.toOwnedSlice(), .params_types = try params_types.toOwnedSlice(), .params_is_pointer = try params_is_pointer.toOwnedSlice(), .params_struct_names = try params_struct_names.toOwnedSlice(), .body = try body.toOwnedSlice() };
        return node;
    }
};
