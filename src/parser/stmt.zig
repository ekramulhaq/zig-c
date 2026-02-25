const std = @import("std");
const ast = @import("../ast.zig");
const token_pkg = @import("../token.zig");
const base = @import("base.zig");
const expr_parser = @import("expr.zig");

const Node = ast.Node;
const TokenType = token_pkg.TokenType;
const ParserBase = base.ParserBase;
const ExprParser = expr_parser.ExprParser;

pub const StmtParser = struct {
    base: *ParserBase,
    expr_p: *ExprParser,

    pub fn init(p_base: *ParserBase, ep: *ExprParser) StmtParser {
        return .{ .base = p_base, .expr_p = ep };
    }

    pub fn parseBlock(self: *StmtParser) anyerror![]*Node {
        if (self.base.consume(.LBrace)) {
            var body = std.ArrayList(*Node).init(self.base.allocator);
            while (self.base.current()) |t| {
                if (t.type == .RBrace) break;
                if (try self.parseStmt()) |stmt| {
                    try body.append(stmt);
                }
            }
            try self.base.expect(.RBrace, "Expected } after block");
            return try body.toOwnedSlice();
        } else {
            var body = std.ArrayList(*Node).init(self.base.allocator);
            if (try self.parseStmt()) |stmt| {
                try body.append(stmt);
            }
            return try body.toOwnedSlice();
        }
    }

    pub fn parseStmt(self: *StmtParser) anyerror!?*Node {
        const token = self.base.current() orelse return error.UnexpectedEOF;
        
        // Typedef
        if (token.type == .TypedefKeyword) {
            self.base.advance();
            const original_type = try self.base.parseType();
            while (self.base.consume(.Star)) {}
            const alias = try self.base.expectIdentifier("Expected alias name after typedef");
            try self.base.expect(.Semicolon, "Expected ; after typedef");
            try self.base.type_system.typedefs.put(alias, original_type);
            return null;
        }

        // Enum
        if (token.type == .EnumKeyword) {
            const i = self.base.pos + 2;
            if (i < self.base.tokens.len and self.base.tokens[i].type == .LBrace) {
                try self.parseEnum();
                return null;
            }
        }

        // Variable declarations or Type aliases
        var alias_type: ?ast.Type = null;
        if (token.type == .Identifier) {
            alias_type = self.base.type_system.typedefs.get(token.value);
        }

        if (token.type == .IntKeyword or token.type == .CharKeyword or token.type == .FloatKeyword or token.type == .DoubleKeyword or token.type == .VoidKeyword or token.type == .StructKeyword or token.type == .EnumKeyword or alias_type != null) {
            const is_struct = (token.type == .StructKeyword);
            const is_enum = (token.type == .EnumKeyword);
            var data_type: ast.DataType = .Int;
            var struct_name: ?[]const u8 = null;

            if (alias_type) |at| {
                data_type = at.id;
                struct_name = at.struct_name;
                self.base.advance();
            } else {
                if (token.type == .CharKeyword) {
                    data_type = .Char;
                } else if (token.type == .FloatKeyword) {
                    data_type = .Float;
                } else if (token.type == .DoubleKeyword) {
                    data_type = .Double;
                } else if (token.type == .VoidKeyword) {
                    data_type = .Void;
                }
                self.base.advance();
                if (is_struct or is_enum) {
                    struct_name = try self.base.expectIdentifier("Expected name");
                }
            }
            
            var decls = std.ArrayList(*Node).init(self.base.allocator);
                            while (true) {
                                var pointer_level: usize = 0;
                                var is_func_ptr = false;
                                if (self.base.consume(.LParen)) {
                                    while (self.base.consume(.Star)) { pointer_level += 1; }
                                    is_func_ptr = true;
                                } else {
                                    while (self.base.consume(.Star)) { pointer_level += 1; }
                                }
                                const is_pointer = (pointer_level > 0);
            
                                const ident = self.base.current() orelse return self.base.errorAt(token, "Expected identifier");
                                if (ident.type != .Identifier) return self.base.errorAt(ident, "Expected identifier");
                                self.base.advance();

                                if (is_func_ptr) {
                                    try self.base.expect(.RParen, "Expected ) in function pointer declaration");
                                    try self.base.expect(.LParen, "Expected ( for function pointer definition arguments");
                                    while (self.base.current()) |t| {
                                        if (t.type == .RParen) {
                                            self.base.advance();
                                            break;
                                        }
                                        self.base.advance();
                                    }
                                }
                                
                                var node: *Node = undefined;
                                if (self.base.consume(.LBracket)) {
                                    const size_node = try self.expr_p.parseExpr();
                                    if (size_node.type != .Number) return self.base.errorAt(ident, "Array size must be a constant number");
                                    try self.base.expect(.RBracket, "Expected ] after array size");
                                    var total_size = size_node.value.?;
                                    // 2D array: char arr[N][M]  -->  treated as char arr[N*M]
                                    if (self.base.consume(.LBracket)) {
                                        const size2_node = try self.expr_p.parseExpr();
                                        if (size2_node.type != .Number) return self.base.errorAt(ident, "Array size must be a constant number");
                                        try self.base.expect(.RBracket, "Expected ] after 2D array size");
                                        total_size = total_size * size2_node.value.?;
                                    }
                                    // Build a Number node with the final size
                                    const flat_size_node = try self.base.allocator.create(Node);
                                    flat_size_node.* = Node{ .type = .Number, .value = total_size };
                                    node = try self.base.allocator.create(Node);
                                    node.* = Node{ .type = .ArrayDecl, .name = ident.value, .value = flat_size_node.value, .data_type = data_type, .is_pointer = is_pointer, .pointer_level = pointer_level, .struct_name = struct_name };

                                } else if (self.base.consume(.Equal)) {
                                    const expr = try self.expr_p.parseExpr();
                                    node = try self.base.allocator.create(Node);
                                    node.* = Node{ .type = .VarDecl, .name = ident.value, .right = expr, .data_type = data_type, .is_pointer = is_pointer, .pointer_level = pointer_level, .struct_name = struct_name };
                                    if (expr.type == .Number) {
                                        node.init_value = expr.value;
                                        node.finit_value = expr.fvalue;
                                    }
                                } else {
                                    node = try self.base.allocator.create(Node);
                                    node.* = Node{ .type = .VarDecl, .name = ident.value, .right = null, .data_type = data_type, .is_pointer = is_pointer, .pointer_level = pointer_level, .struct_name = struct_name };
                                }
                                try decls.append(node);
                            if (!self.base.consume(.Comma)) break;
            }
            try self.base.expect(.Semicolon, "Expected ; after declaration(s)");

            if (decls.items.len == 1) {
                return decls.items[0];
            } else {
                const node = try self.base.allocator.create(Node);
                node.* = Node{ .type = .Compound, .body = try decls.toOwnedSlice() };
                return node;
            }
        } else if (token.type == .ReturnKeyword) {
            self.base.advance();
            var expr: ?*Node = null;
            if (!self.base.consume(.Semicolon)) {
                expr = try self.expr_p.parseExpr();
                try self.base.expect(.Semicolon, "Expected ;");
            }
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Return, .right = expr };
            return node;
        } else if (token.type == .IfKeyword) {
            self.base.advance();
            try self.base.expect(.LParen, "Expected (");
            const cond = try self.expr_p.parseExpr();
            try self.base.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            var else_body: ?[]*Node = null;
            if (self.base.consume(.ElseKeyword)) {
                else_body = try self.parseBlock();
            }
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .If, .condition = cond, .body = body, .else_body = else_body };
            return node;
        } else if (token.type == .WhileKeyword) {
            self.base.advance();
            try self.base.expect(.LParen, "Expected (");
            const cond = try self.expr_p.parseExpr();
            try self.base.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .While, .condition = cond, .body = body };
            return node;
        } else if (token.type == .DoKeyword) {
            self.base.advance();
            const body = try self.parseBlock();
            try self.base.expect(.WhileKeyword, "Expected 'while' after do block");
            try self.base.expect(.LParen, "Expected (");
            const cond = try self.expr_p.parseExpr();
            try self.base.expect(.RParen, "Expected )");
            try self.base.expect(.Semicolon, "Expected ; after do-while");
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .DoWhile, .condition = cond, .body = body };
            return node;
        } else if (token.type == .BreakKeyword) {
            self.base.advance();
            try self.base.expect(.Semicolon, "Expected ; after break");
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Break };
            return node;
        } else if (token.type == .ContinueKeyword) {
            self.base.advance();
            try self.base.expect(.Semicolon, "Expected ; after continue");
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Continue };
            return node;
        } else if (token.type == .SwitchKeyword) {
            self.base.advance();
            try self.base.expect(.LParen, "Expected (");
            const cond = try self.expr_p.parseExpr();
            try self.base.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Switch, .condition = cond, .body = body };
            return node;
        } else if (token.type == .CaseKeyword) {
            self.base.advance();
            const val_node = try self.expr_p.parseExpr();
            if (val_node.type != .Number) return self.base.errorAt(token, "Case value must be a constant number");
            try self.base.expect(.Colon, "Expected : after case value");
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Case, .init_value = val_node.value };
            return node;
        } else if (token.type == .DefaultKeyword) {
            self.base.advance();
            try self.base.expect(.Colon, "Expected : after default");
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Case, .init_value = null }; 
            return node;
        } else if (token.type == .ForKeyword) {
            self.base.advance();
            try self.base.expect(.LParen, "Expected (");
            const init_stmt = (try self.parseStmt()) orelse @panic("Expected init statement in for loop");
            const cond = try self.expr_p.parseExpr();
            try self.base.expect(.Semicolon, "Expected ;");
            const update = try self.expr_p.parseExpr();
            try self.base.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .For, .init = init_stmt, .condition = cond, .update = update, .body = body };
            return node;
        } else if (token.type == .LBrace) {
            const body = try self.parseBlock();
            _ = body;
            return self.base.errorAt(token, "Standalone blocks not yet supported");
        } else {
            const expr = try self.expr_p.parseExpr();
            try self.base.expect(.Semicolon, "Expected ; after expression");
            return expr;
        }
    }

    pub fn parseEnum(self: *StmtParser) anyerror!void {
        try self.base.expect(.EnumKeyword, "Expected 'enum'");
        if (self.base.current()) |t| {
            if (t.type == .Identifier) self.base.advance();
        }
        try self.base.expect(.LBrace, "Expected { after enum");
        var val: i64 = 0;
        while (self.base.current()) |t| {
            if (t.type == .RBrace) break;
            const ident = try self.base.expectIdentifier("Expected enum member name");
            if (self.base.consume(.Equal)) {
                const num = try self.expr_p.parseExpr();
                if (num.type != .Number) return self.base.errorAt(t, "Enum value must be constant");
                val = num.value.?;
            }
            try self.base.type_system.enums.put(ident, val);
            val += 1;
            _ = self.base.consume(.Comma);
        }
        try self.base.expect(.RBrace, "Expected } after enum members");
        try self.base.expect(.Semicolon, "Expected ; after enum");
    }
};
