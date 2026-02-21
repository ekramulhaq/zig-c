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
    structs: std.StringHashMap([]*Node), // struct name -> member declarations
    typedefs: std.StringHashMap(ast.DataType), // alias -> original type
    enums: std.StringHashMap(i64), // enum member -> value

    /// Initializes a new Parser with the given tokens.
    pub fn init(tokens: []Token, allocator: std.mem.Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
            .structs = std.StringHashMap([]*Node).init(allocator),
            .typedefs = std.StringHashMap(ast.DataType).init(allocator),
            .enums = std.StringHashMap(i64).init(allocator),
        };
    }

    fn current(self: *Parser) ?Token {
        if (self.pos < self.tokens.len) return self.tokens[self.pos];
        return null;
    }

    fn advance(self: *Parser) void {
        self.pos += 1;
    }

    fn errorAt(self: *Parser, token: Token, msg: []const u8) anyerror {
        _ = self;
        std.debug.print("Error at line {}, col {}: {s} (got '{s}')\n", .{ token.line, token.col, msg, token.value });
        return error.ParseError;
    }

    fn expect(self: *Parser, token_type: TokenType, msg: []const u8) !void {
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
        const left = try self.parseTernary();
        if (self.current()) |token| {
            switch (token.type) {
                .Equal, .PlusEqual, .MinusEqual, .StarEqual, .SlashEqual, .PercentEqual => {
                    if (left.type != .Identifier and left.type != .Deref and left.type != .Index and left.type != .MemberAccess) {
                        return self.errorAt(token, "Invalid lvalue for assignment");
                    }
                    self.advance();
                    const right = try self.parseExpr();
                    const node = try self.allocator.create(Node);
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

    fn parseTernary(self: *Parser) anyerror!*Node {
        const cond = try self.parseLogicalOr();
        if (self.consume(.Question)) {
            const then_branch = try self.parseExpr();
            try self.expect(.Colon, "Expected : in ternary operator");
            const else_branch = try self.parseTernary();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Ternary, .condition = cond, .left = then_branch, .right = else_branch };
            return node;
        }
        return cond;
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
            } else if (token.type == .Dot) {
                self.advance();
                const member = try self.expectIdentifier("Expected member name after '.'");
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .MemberAccess, .left = node, .name = member };
                node = new_node;
            } else if (token.type == .Arrow) {
                self.advance();
                const member = try self.expectIdentifier("Expected member name after '->'");
                // a->b is same as (*a).b
                const deref = try self.allocator.create(Node);
                deref.* = Node{ .type = .Deref, .right = node };
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .MemberAccess, .left = deref, .name = member };
                node = new_node;
            } else if (token.type == .PlusPlus or token.type == .MinusMinus) {
                self.advance();
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .type = .PostfixOp, .op = token.type, .right = node };
                node = new_node;
            } else break;
        }
        return node;
    }

    fn expectIdentifier(self: *Parser, msg: []const u8) anyerror![]const u8 {
        const token = self.current() orelse return error.UnexpectedEOF;
        if (token.type != .Identifier) return self.errorAt(token, msg);
        self.advance();
        return token.value;
    }

    fn parseType(self: *Parser) anyerror!ast.DataType {
        const token = self.current() orelse return error.UnexpectedEOF;
        if (token.type == .IntKeyword) {
            self.advance();
            return .Int;
        } else if (token.type == .CharKeyword) {
            self.advance();
            return .Char;
        } else if (token.type == .VoidKeyword) {
            self.advance();
            return .Void;
        } else if (token.type == .Identifier) {
            if (self.typedefs.get(token.value)) |dt| {
                self.advance();
                return dt;
            }
        }
        return self.errorAt(token, "Expected type name");
    }

    fn parseEnum(self: *Parser) anyerror!*Node {
        try self.expect(.EnumKeyword, "Expected 'enum'");
        // Skip enum name if present
        if (self.current()) |t| {
            if (t.type == .Identifier) self.advance();
        }
        try self.expect(.LBrace, "Expected { after enum");
        var val: i64 = 0;
        while (self.current()) |t| {
            if (t.type == .RBrace) break;
            const ident = try self.expectIdentifier("Expected enum member name");
            if (self.consume(.Equal)) {
                const num = try self.parseExpr();
                if (num.type != .Number) return self.errorAt(t, "Enum value must be constant");
                val = num.value.?;
            }
            try self.enums.put(ident, val);
            val += 1;
            _ = self.consume(.Comma);
        }
        try self.expect(.RBrace, "Expected } after enum members");
        try self.expect(.Semicolon, "Expected ; after enum");
        const node = try self.allocator.create(Node);
        node.* = Node{ .type = .EnumDecl };
        return node;
    }

    fn parseFactor(self: *Parser) anyerror!*Node {
        const token = self.current() orelse return error.UnexpectedEOF;
        self.advance();
        if (token.type == .Number) {
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Number, .value = try std.fmt.parseInt(i64, token.value, 10) };
            return node;
        } else if (token.type == .Identifier) {
            if (self.enums.get(token.value)) |val| {
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .Number, .value = val };
                return node;
            }
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
        return self.errorAt(token, "Expected number, identifier, or (");
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
        const token = self.current() orelse return error.UnexpectedEOF;
        if (token.type == .TypedefKeyword) {
            self.advance();
            const original_type = try self.parseType();
            while (self.consume(.Star)) {}
            const alias = try self.expectIdentifier("Expected alias name after typedef");
            try self.expect(.Semicolon, "Expected ; after typedef");
            try self.typedefs.put(alias, original_type);
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .EnumDecl }; // Just a dummy node
            return node;
        }
        if (token.type == .EnumKeyword) {
            // Check if it's enum declaration or variable declaration
            const i = self.pos + 2;
            if (i < self.tokens.len and self.tokens[i].type == .LBrace) {
                return self.parseEnum();
            }
        }
        var is_typedef_alias = false;
        if (token.type == .Identifier) {
            if (self.typedefs.contains(token.value)) {
                is_typedef_alias = true;
            }
        }

        if (token.type == .IntKeyword or token.type == .CharKeyword or token.type == .StructKeyword or token.type == .EnumKeyword or is_typedef_alias) {
            const is_struct = (token.type == .StructKeyword);
            const is_enum = (token.type == .EnumKeyword);
            var data_type: ast.DataType = .Int;
            if (token.type == .CharKeyword) {
                data_type = .Char;
            } else if (is_typedef_alias) {
                data_type = self.typedefs.get(token.value).?;
            }
            self.advance();
            var struct_name: ?[]const u8 = null;
            if (is_struct or is_enum) {
                struct_name = try self.expectIdentifier("Expected name");
            }
            
            while (self.consume(.Star)) {}
            const ident = self.current() orelse return self.errorAt(token, "Expected identifier");
            if (ident.type != .Identifier) return self.errorAt(ident, "Expected identifier");
            self.advance();
            
            if (self.consume(.LBracket)) {
                const size_node = try self.parseExpr();
                if (size_node.type != .Number) return self.errorAt(ident, "Array size must be a constant number");
                try self.expect(.RBracket, "Expected ] after array size");
                try self.expect(.Semicolon, "Expected ; after array declaration");
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .ArrayDecl, .name = ident.value, .value = size_node.value, .data_type = data_type };
                return node;
            }
            
            if (self.consume(.Equal)) {
                const expr = try self.parseExpr();
                try self.expect(.Semicolon, "Expected ; after variable declaration");
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .VarDecl, .name = ident.value, .right = expr, .data_type = data_type };
                if (expr.type == .Number) {
                    node.init_value = expr.value;
                }
                return node;
            } else {
                try self.expect(.Semicolon, "Expected ; after variable declaration");
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .VarDecl, .name = ident.value, .right = null, .data_type = data_type };
                // If it's a struct, we should probably mark it. For now, our compiler treats all vars as 8 bytes.
                return node;
            }
        } else if (token.type == .ReturnKeyword) {
            self.advance();
            var expr: ?*Node = null;
            if (!self.consume(.Semicolon)) {
                expr = try self.parseExpr();
                try self.expect(.Semicolon, "Expected ;");
            }
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
        } else if (token.type == .DoKeyword) {
            self.advance();
            const body = try self.parseBlock();
            try self.expect(.WhileKeyword, "Expected 'while' after do block");
            try self.expect(.LParen, "Expected (");
            const cond = try self.parseExpr();
            try self.expect(.RParen, "Expected )");
            try self.expect(.Semicolon, "Expected ; after do-while");
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .DoWhile, .condition = cond, .body = body };
            return node;
        } else if (token.type == .BreakKeyword) {
            self.advance();
            try self.expect(.Semicolon, "Expected ; after break");
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Break };
            return node;
        } else if (token.type == .ContinueKeyword) {
            self.advance();
            try self.expect(.Semicolon, "Expected ; after continue");
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Continue };
            return node;
        } else if (token.type == .SwitchKeyword) {
            self.advance();
            try self.expect(.LParen, "Expected (");
            const cond = try self.parseExpr();
            try self.expect(.RParen, "Expected )");
            const body = try self.parseBlock();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Switch, .condition = cond, .body = body };
            return node;
        } else if (token.type == .CaseKeyword) {
            self.advance();
            const val_node = try self.parseExpr();
            if (val_node.type != .Number) return self.errorAt(token, "Case value must be a constant number");
            try self.expect(.Colon, "Expected : after case value");
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Case, .init_value = val_node.value };
            return node;
        } else if (token.type == .DefaultKeyword) {
            self.advance();
            try self.expect(.Colon, "Expected : after default");
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Case, .init_value = null }; // null means default
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
            _ = body;
            return self.errorAt(token, "Standalone blocks not yet supported");
        } else {
            const expr = try self.parseExpr();
            try self.expect(.Semicolon, "Expected ; after expression");
            return expr;
        }
    }

    fn parseStruct(self: *Parser) anyerror!*Node {
        try self.expect(.StructKeyword, "Expected 'struct'");
        const ident = try self.expectIdentifier("Expected struct name");
        try self.expect(.LBrace, "Expected '{' after struct name");
        var members = std.ArrayList(*Node).init(self.allocator);
        while (self.current()) |t| {
            if (t.type == .RBrace) break;
            try members.append(try self.parseStmt());
        }
        try self.expect(.RBrace, "Expected '}' after struct members");
        try self.expect(.Semicolon, "Expected ';' after struct definition");
        
        const member_slice = try members.toOwnedSlice();
        try self.structs.put(ident, member_slice);
        
        const node = try self.allocator.create(Node);
        node.* = Node{ .type = .StructDecl, .name = ident, .members = member_slice };
        return node;
    }

    fn parseFunction(self: *Parser) anyerror!*Node {
        const ret_token = self.current() orelse return error.UnexpectedEOF;
        if (ret_token.type != .IntKeyword and ret_token.type != .CharKeyword and ret_token.type != .VoidKeyword) {
            return self.errorAt(ret_token, "Expected return type (int, char, or void)");
        }
        var ret_type: ast.DataType = .Int;
        if (ret_token.type == .CharKeyword) {
            ret_type = .Char;
        } else if (ret_token.type == .VoidKeyword) {
            ret_type = .Void;
        }
        self.advance();

        while (self.consume(.Star)) {}
        const ident = try self.expectIdentifier("Expected function name");
        try self.expect(.LParen, "Expected ( after function name");
        var params = std.ArrayList([]const u8).init(self.allocator);
        var params_types = std.ArrayList(ast.DataType).init(self.allocator);
        if (!self.consume(.RParen)) {
            while (true) {
                const param_type_token = self.current() orelse return error.UnexpectedEOF;
                if (param_type_token.type != .IntKeyword and param_type_token.type != .CharKeyword) {
                    return self.errorAt(param_type_token, "Expected parameter type (int or char)");
                }
                try params_types.append(if (param_type_token.type == .CharKeyword) .Char else .Int);
                self.advance();

                while (self.consume(.Star)) {}
                try params.append(try self.expectIdentifier("Expected parameter name"));
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
        node.* = Node{ .type = .Function, .data_type = ret_type, .name = ident, .params = try params.toOwnedSlice(), .params_types = try params_types.toOwnedSlice(), .body = try body.toOwnedSlice() };
        return node;
    }

    /// Parses the entire program from the token stream.
    pub fn parseProgram(self: *Parser) anyerror![]*Node {
        var items = std.ArrayList(*Node).init(self.allocator);
        while (self.current()) |token| {
            if (token.type == .EOF) break;
            
            if (token.type == .StructKeyword) {
                // Peak ahead: struct Name { ... }  vs struct Name var;
                // If there's a '{' soon, it's a definition.
                const i = self.pos + 2;
                if (i < self.tokens.len and self.tokens[i].type == .LBrace) {
                    try items.append(try self.parseStruct());
                    continue;
                }
            }

            var i = self.pos + 1;
            if (self.tokens[self.pos].type == .StructKeyword) {
                i += 1; // skip struct name
            }
            while (i < self.tokens.len and self.tokens[i].type == .Star) : (i += 1) {}
            i += 1; // skip identifier
            if (i < self.tokens.len and self.tokens[i].type == .LParen) {
                try items.append(try self.parseFunction());
            } else {
                try items.append(try self.parseStmt());
            }
        }
        return try items.toOwnedSlice();
    }
};
