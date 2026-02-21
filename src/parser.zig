const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const Token = lexer.Token;
const TokenType = lexer.TokenType;
const Node = ast.Node;
const NodeType = ast.NodeType;

/// Parser converts a stream of tokens into an Abstract Syntax Tree (AST).
pub const Parser = struct {
    tokens: []Token,
    pos: usize,
    allocator: std.mem.Allocator,

    /// Initializes a new Parser with the given tokens.
    pub fn init(tokens: []Token, allocator: std.mem.Allocator) Parser {
        return Parser{ .tokens = tokens, .pos = 0, .allocator = allocator };
    }

    fn current(self: *Parser) ?Token {
        if (self.pos < self.tokens.len) return self.tokens[self.pos];
        return null;
    }

    fn advance(self: *Parser) void {
        self.pos += 1;
    }

    fn expect(self: *Parser, token_type: TokenType, msg: []const u8) !void {
        if (self.current()) |t| {
            if (t.type == token_type) {
                self.advance();
                return;
            }
        }
        std.debug.print("Error: {s}\n", .{msg});
        return error.UnexpectedToken;
    }

    fn consume(self: *Parser, token_type: TokenType) bool {
        if (self.current()) |t| {
            if (t.type == token_type) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn parseExpr(self: *Parser) anyerror!*Node {
        const left = try self.parseLogicalOr();
        if (self.current()) |token| {
            switch (token.type) {
                .Equal, .PlusEqual, .MinusEqual, .StarEqual, .SlashEqual, .PercentEqual => {
                    if (left.type != .Identifier and left.type != .Deref and left.type != .Index) @panic("Invalid lvalue for assignment");
                    self.advance();
                    const right = try self.parseExpr();
                    const node = try self.allocator.create(Node);
                    if (left.type == .Identifier) {
                        node.* = Node{ .type = .Assignment, .name = left.name, .op = token.type, .right = right };
                    } else if (left.type == .Deref) {
                        node.* = Node{ .type = .Assignment, .left = left.right, .op = token.type, .right = right };
                    } else {
                        // Array indexing: a[i] = val
                        node.* = Node{ .type = .Assignment, .left = left, .op = token.type, .right = right };
                    }
                    return node;
                },
                else => {},
            }
        }
        return left;
    }

    fn parseLogicalOr(self: *Parser) anyerror!*Node {
        var node = try self.parseLogicalAnd();
        while (self.current()) |token| {
            if (token.type == .PipePipe) {
                self.advance();
                const right = try self.parseLogicalAnd();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .LogicalOp, .op = .PipePipe, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseLogicalAnd(self: *Parser) anyerror!*Node {
        var node = try self.parseBitwiseOr();
        while (self.current()) |token| {
            if (token.type == .AmpersandAmpersand) {
                self.advance();
                const right = try self.parseBitwiseOr();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .LogicalOp, .op = .AmpersandAmpersand, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseBitwiseOr(self: *Parser) anyerror!*Node {
        var node = try self.parseBitwiseXor();
        while (self.current()) |token| {
            if (token.type == .Pipe) {
                self.advance();
                const right = try self.parseBitwiseXor();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = .Pipe, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseBitwiseXor(self: *Parser) anyerror!*Node {
        var node = try self.parseBitwiseAnd();
        while (self.current()) |token| {
            if (token.type == .Caret) {
                self.advance();
                const right = try self.parseBitwiseAnd();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = .Caret, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseBitwiseAnd(self: *Parser) anyerror!*Node {
        var node = try self.parseEquality();
        while (self.current()) |token| {
            if (token.type == .Ampersand) {
                self.advance();
                const right = try self.parseEquality();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = .Ampersand, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseEquality(self: *Parser) anyerror!*Node {
        var node = try self.parseRelational();
        while (self.current()) |token| {
            if (token.type == .EqualEqual or token.type == .NotEqual) {
                self.advance();
                const right = try self.parseRelational();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .Comparison, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseRelational(self: *Parser) anyerror!*Node {
        var node = try self.parseShift();
        while (self.current()) |token| {
            if (token.type == .Greater or token.type == .Less or token.type == .GreaterEqual or token.type == .LessEqual) {
                self.advance();
                const right = try self.parseShift();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .Comparison, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseShift(self: *Parser) anyerror!*Node {
        var node = try self.parseAdditive();
        while (self.current()) |token| {
            if (token.type == .LessLess or token.type == .GreaterGreater) {
                self.advance();
                const right = try self.parseAdditive();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseAdditive(self: *Parser) anyerror!*Node {
        var node = try self.parseMultiplicative();
        while (self.current()) |token| {
            if (token.type == .Plus or token.type == .Minus) {
                self.advance();
                const right = try self.parseMultiplicative();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseMultiplicative(self: *Parser) anyerror!*Node {
        var node = try self.parseUnary();
        while (self.current()) |token| {
            if (token.type == .Star or token.type == .Slash or token.type == .Percent) {
                self.advance();
                const right = try self.parseUnary();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .BinaryOp, .op = token.type, .left = node, .right = right };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseUnary(self: *Parser) anyerror!*Node {
        if (self.current()) |token| {
            if (token.type == .Minus or token.type == .Bang or token.type == .Tilde or token.type == .PlusPlus or token.type == .MinusMinus) {
                self.advance();
                const right = try self.parseUnary();
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .UnaryOp, .op = token.type, .right = right };
                return node;
            }
            if (token.type == .Star) {
                self.advance();
                const right = try self.parseUnary();
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .Deref, .right = right };
                return node;
            }
            if (token.type == .Ampersand) {
                self.advance();
                const right = try self.parseUnary();
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .AddressOf, .right = right };
                return node;
            }
        }
        return try self.parsePostfix();
    }

    fn parsePostfix(self: *Parser) anyerror!*Node {
        var node = try self.parseFactor();
        while (self.current()) |token| {
            if (token.type == .LBracket) {
                self.advance();
                const index = try self.parseExpr();
                try self.expect(.RBracket, "Expected ] after array index");
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .Index, .left = node, .right = index };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn parseFactor(self: *Parser) anyerror!*Node {
        const token = self.current() orelse @panic("Unexpected EOF");
        self.advance();
        if (token.type == .Number) {
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Number, .value = try std.fmt.parseInt(i64, token.value, 10) };
            return node;
        } else if (token.type == .Identifier) {
            if (self.consume(.LParen)) {
                var args = std.ArrayList(*Node).init(self.allocator);
                if (!self.consume(.RParen)) {
                    while (true) {
                        try args.append(try self.parseExpr());
                        if (!self.consume(.Comma)) break;
                    }
                    try self.expect(.RParen, "Expected ) after function arguments");
                }
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .FunctionCall, .name = token.value, .args = try args.toOwnedSlice() };
                return node;
            }
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Identifier, .name = token.value };
            return node;
        } else if (token.type == .StringLiteral) {
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .String, .data = token.value };
            return node;
        } else if (token.type == .LParen) {
            const expr = try self.parseExpr();
            try self.expect(.RParen, "Expected )");
            return expr;
        }
        @panic("Expected number, identifier, or (");
    }

    fn parseBlock(self: *Parser) anyerror![]*Node {
        if (self.consume(.LBrace)) {
            var body = std.ArrayList(*Node).init(self.allocator);
            while (self.current()) |t| {
                if (t.type == .RBrace) break;
                try body.append(try self.parseStmt());
            }
            try self.expect(.RBrace, "Expected } after block");
            return try body.toOwnedSlice();
        } else {
            var body = std.ArrayList(*Node).init(self.allocator);
            try body.append(try self.parseStmt());
            return try body.toOwnedSlice();
        }
    }

    fn parseStmt(self: *Parser) anyerror!*Node {
        const token = self.current() orelse @panic("Unexpected EOF");
        if (token.type == .IntKeyword) {
            self.advance();
            while (self.consume(.Star)) {}
            const ident = self.current() orelse @panic("Expected identifier");
            if (ident.type != .Identifier) @panic("Expected identifier");
            self.advance();
            if (self.consume(.LBracket)) {
                const size_node = try self.parseExpr();
                if (size_node.type != .Number) @panic("Array size must be a constant number");
                try self.expect(.RBracket, "Expected ] after array size");
                try self.expect(.Semicolon, "Expected ; after array declaration");
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .ArrayDecl, .name = ident.value, .value = size_node.value };
                return node;
            }
            if (self.consume(.Equal)) {
                const expr = try self.parseExpr();
                try self.expect(.Semicolon, "Expected ; after variable declaration");
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .VarDecl, .name = ident.value, .right = expr };
                if (expr.type == .Number) {
                    node.init_value = expr.value;
                }
                return node;
            } else {
                try self.expect(.Semicolon, "Expected ; after variable declaration");
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .VarDecl, .name = ident.value, .right = null };
                return node;
            }
        } else if (token.type == .ReturnKeyword) {
            self.advance();
            const expr = try self.parseExpr();
            try self.expect(.Semicolon, "Expected ;");
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Return, .right = expr };
            return node;
        } else if (token.type == .IfKeyword) {
            self.advance();
            try self.expect(.LParen, "Expected (");
            const cond = try self.parseExpr();
            try self.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            var else_body: ?[]*Node = null;
            if (self.consume(.ElseKeyword)) {
                else_body = try self.parseBlock();
            }
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .If, .condition = cond, .body = body, .else_body = else_body };
            return node;
        } else if (token.type == .WhileKeyword) {
            self.advance();
            try self.expect(.LParen, "Expected (");
            const cond = try self.parseExpr();
            try self.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .While, .condition = cond, .body = body };
            return node;
        } else if (token.type == .ForKeyword) {
            self.advance();
            try self.expect(.LParen, "Expected (");
            const init_stmt = try self.parseStmt();
            const cond = try self.parseExpr();
            try self.expect(.Semicolon, "Expected ;");
            const update = try self.parseExpr();
            try self.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .For, .init = init_stmt, .condition = cond, .update = update, .body = body };
            return node;
        } else if (token.type == .LBrace) {
            const body = try self.parseBlock();
            // Since our NodeType doesn't have a Block type, we just return the first node?
            // Actually, we should probably add a Block NodeType.
            // For now, let's just use If with true condition as a hack, or fix it properly.
            // Better: parseBlock returning Node*.
            _ = body;
            @panic("Blocks as standalone statements not fully supported yet");
        } else {
            const expr = try self.parseExpr();
            try self.expect(.Semicolon, "Expected ; after expression");
            return expr;
        }
    }

    fn parseFunction(self: *Parser) anyerror!*Node {
        try self.expect(.IntKeyword, "Only int return type is supported");
        while (self.consume(.Star)) {}
        const ident = self.current() orelse @panic("Expected function name");
        self.advance();
        try self.expect(.LParen, "Expected ( after function name");
        var params = std.ArrayList([]const u8).init(self.allocator);
        if (!self.consume(.RParen)) {
            while (true) {
                try self.expect(.IntKeyword, "Only int parameters are supported");
                while (self.consume(.Star)) {}
                const p_ident = self.current() orelse @panic("Expected parameter name");
                self.advance();
                try params.append(p_ident.value);
                if (!self.consume(.Comma)) break;
            }
            try self.expect(.RParen, "Expected ) after function parameters");
        }
        try self.expect(.LBrace, "Expected { to start function body");
        var body = std.ArrayList(*Node).init(self.allocator);
        while (self.current()) |t| {
            if (t.type == .RBrace) break;
            try body.append(try self.parseStmt());
        }
        try self.expect(.RBrace, "Expected } to end function body");
        const node = try self.allocator.create(Node);
        node.* = Node{ .type = .Function, .name = ident.value, .params = try params.toOwnedSlice(), .body = try body.toOwnedSlice() };
        return node;
    }

    /// Parses the entire program from the token stream.
    pub fn parseProgram(self: *Parser) anyerror![]*Node {
        var items = std.ArrayList(*Node).init(self.allocator);
        while (self.current()) |token| {
            if (token.type == .EOF) break;
            
            // Peak ahead to see if it's a function or a global variable
            // Look for '(' after the identifier (handling possible pointer stars)
            var i = self.pos + 1;
            while (i < self.tokens.len and self.tokens[i].type == .Star) : (i += 1) {}
            // Now self.tokens[i] should be the Identifier
            i += 1;
            if (i < self.tokens.len and self.tokens[i].type == .LParen) {
                try items.append(try self.parseFunction());
            } else {
                try items.append(try self.parseStmt());
            }
        }
        return try items.toOwnedSlice();
    }
};
