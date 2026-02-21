const std = @import("std");
const ast = @import("../ast.zig");
const token_pkg = @import("../token.zig");
const base = @import("base.zig");

const Node = ast.Node;
const TokenType = token_pkg.TokenType;
const ParserBase = base.ParserBase;

pub const ExprParser = struct {
    base: *ParserBase,

    pub fn init(p_base: *ParserBase) ExprParser {
        return .{ .base = p_base };
    }

    pub fn parseExpr(self: *ExprParser) anyerror!*Node {
        const left = try self.parseTernary();
        if (self.base.current()) |token| {
            switch (token.type) {
                .Equal, .PlusEqual, .MinusEqual, .StarEqual, .SlashEqual, .PercentEqual => {
                    if (left.type != .Identifier and left.type != .Deref and left.type != .Index and left.type != .MemberAccess) {
                        return self.base.errorAt(token, "Invalid lvalue for assignment");
                    }
                    self.base.advance();
                    const right = try self.parseExpr();
                    const node = try self.base.allocator.create(Node);
                    if (left.type == .Identifier) {
                        node.* = Node{ .type = .Assignment, .name = left.name, .op = token.type, .right = right };
                    } else {
                        node.* = Node{ .type = .Assignment, .left = left, .op = token.type, .right = right };
                    }
                    return node;
                },
                else => {},
            }
        }
        return left;
    }

    fn parseTernary(self: *ExprParser) anyerror!*Node {
        const cond = try self.parseLogicalOr();
        if (self.base.consume(.Question)) {
            const then_branch = try self.parseExpr();
            try self.base.expect(.Colon, "Expected : in ternary operator");
            const else_branch = try self.parseTernary();
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Ternary, .condition = cond, .left = then_branch, .right = else_branch };
            return node;
        }
        return cond;
    }

    fn parseLogicalOr(self: *ExprParser) anyerror!*Node {
        var node = try self.parseLogicalAnd();
        while (self.base.current()) |token| {
            if (token.type == .PipePipe) {
                self.base.advance();
                const right = try self.parseLogicalAnd();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .LogicalOp, .op = .PipePipe, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseLogicalAnd(self: *ExprParser) anyerror!*Node {
        var node = try self.parseBitwiseOr();
        while (self.base.current()) |token| {
            if (token.type == .AmpersandAmpersand) {
                self.base.advance();
                const right = try self.parseBitwiseOr();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .LogicalOp, .op = .AmpersandAmpersand, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseBitwiseOr(self: *ExprParser) anyerror!*Node {
        var node = try self.parseBitwiseXor();
        while (self.base.current()) |token| {
            if (token.type == .Pipe) {
                self.base.advance();
                const right = try self.parseBitwiseXor();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = .Pipe, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseBitwiseXor(self: *ExprParser) anyerror!*Node {
        var node = try self.parseBitwiseAnd();
        while (self.base.current()) |token| {
            if (token.type == .Caret) {
                self.base.advance();
                const right = try self.parseBitwiseAnd();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = .Caret, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseBitwiseAnd(self: *ExprParser) anyerror!*Node {
        var node = try self.parseEquality();
        while (self.base.current()) |token| {
            if (token.type == .Ampersand) {
                self.base.advance();
                const right = try self.parseEquality();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = .Ampersand, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseEquality(self: *ExprParser) anyerror!*Node {
        var node = try self.parseRelational();
        while (self.base.current()) |token| {
            if (token.type == .EqualEqual or token.type == .NotEqual) {
                self.base.advance();
                const right = try self.parseRelational();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .Comparison, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseRelational(self: *ExprParser) anyerror!*Node {
        var node = try self.parseShift();
        while (self.base.current()) |token| {
            if (token.type == .Greater or token.type == .Less or token.type == .GreaterEqual or token.type == .LessEqual) {
                self.base.advance();
                const right = try self.parseShift();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .Comparison, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseShift(self: *ExprParser) anyerror!*Node {
        var node = try self.parseAdditive();
        while (self.base.current()) |token| {
            if (token.type == .LessLess or token.type == .GreaterGreater) {
                self.base.advance();
                const right = try self.parseAdditive();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseAdditive(self: *ExprParser) anyerror!*Node {
        var node = try self.parseMultiplicative();
        while (self.base.current()) |token| {
            if (token.type == .Plus or token.type == .Minus) {
                self.base.advance();
                const right = try self.parseMultiplicative();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseMultiplicative(self: *ExprParser) anyerror!*Node {
        var node = try self.parseUnary();
        while (self.base.current()) |token| {
            if (token.type == .Star or token.type == .Slash or token.type == .Percent) {
                self.base.advance();
                const right = try self.parseUnary();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseUnary(self: *ExprParser) anyerror!*Node {
        if (self.base.current()) |token| {
            if (token.type == .Minus or token.type == .Bang or token.type == .Tilde or token.type == .PlusPlus or token.type == .MinusMinus) {
                self.base.advance();
                const right = try self.parseUnary();
                const node = try self.base.allocator.create(Node);
                node.* = Node{ .type = .UnaryOp, .op = token.type, .right = right };
                return node;
            }
            if (token.type == .Star) {
                self.base.advance();
                const right = try self.parseUnary();
                const node = try self.base.allocator.create(Node);
                node.* = Node{ .type = .Deref, .right = right };
                return node;
            }
            if (token.type == .Ampersand) {
                self.base.advance();
                const right = try self.parseUnary();
                const node = try self.base.allocator.create(Node);
                node.* = Node{ .type = .AddressOf, .right = right };
                return node;
            }
            if (token.type == .SizeofKeyword) {
                return self.parseSizeof();
            }
        }
        return try self.parsePostfix();
    }

    fn parseSizeof(self: *ExprParser) anyerror!*Node {
        try self.base.expect(.SizeofKeyword, "Expected 'sizeof'");
        try self.base.expect(.LParen, "Expected ( after sizeof");
        const t = try self.base.parseType();
        try self.base.expect(.RParen, "Expected ) after sizeof type");
        const node = try self.base.allocator.create(Node);
        node.* = Node{ .type = .Sizeof, .data_type = t.id, .struct_name = t.struct_name };
        return node;
    }

    fn parsePostfix(self: *ExprParser) anyerror!*Node {
        var node = try self.parseFactor();
        while (self.base.current()) |token| {
            if (token.type == .LBracket) {
                self.base.advance();
                const index = try self.parseExpr();
                try self.base.expect(.RBracket, "Expected ] after array index");
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .Index, .left = node, .right = index };
                node = new_node;
            } else if (token.type == .Dot) {
                self.base.advance();
                const member = try self.base.expectIdentifier("Expected member name after '.'");
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .MemberAccess, .left = node, .name = member };
                node = new_node;
            } else if (token.type == .Arrow) {
                self.base.advance();
                const member = try self.base.expectIdentifier("Expected member name after '->'");
                const deref = try self.base.allocator.create(Node);
                deref.* = Node{ .type = .Deref, .right = node };
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .MemberAccess, .left = deref, .name = member };
                node = new_node;
            } else if (token.type == .PlusPlus or token.type == .MinusMinus) {
                self.base.advance();
                const new_node = try self.base.allocator.create(Node);
                new_node.* = Node{ .type = .PostfixOp, .op = token.type, .right = node };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseFactor(self: *ExprParser) anyerror!*Node {
        const token = self.base.current() orelse return error.UnexpectedEOF;
        self.base.advance();
        if (token.type == .Number) {
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Number, .value = try std.fmt.parseInt(i64, token.value, 10) };
            return node;
        } else if (token.type == .Identifier) {
            if (self.base.type_system.enums.get(token.value)) |val| {
                const node = try self.base.allocator.create(Node);
                node.* = Node{ .type = .Number, .value = val };
                return node;
            }
            if (self.base.consume(.LParen)) {
                var args = std.ArrayList(*Node).init(self.base.allocator);
                if (!self.base.consume(.RParen)) {
                    while (true) {
                        try args.append(try self.parseExpr());
                        if (!self.base.consume(.Comma)) break;
                    }
                    try self.base.expect(.RParen, "Expected ) after function arguments");
                }
                const node = try self.base.allocator.create(Node);
                node.* = Node{ .type = .FunctionCall, .name = token.value, .args = try args.toOwnedSlice() };
                return node;
            }
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .Identifier, .name = token.value };
            return node;
        } else if (token.type == .StringLiteral) {
            const node = try self.base.allocator.create(Node);
            node.* = Node{ .type = .String, .data = token.value };
            return node;
        } else if (token.type == .LParen) {
            const expr = try self.parseExpr();
            try self.base.expect(.RParen, "Expected )");
            return expr;
        }
        return self.base.errorAt(token, "Expected number, identifier, or (");
    }
};
