const std = @import("std");
const builtin = @import("builtin");

const Arch = enum {
    x86_64,
    arm64,
};

const TokenType = enum {
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
    EOF,
};

const Token = struct {
    type: TokenType,
    value: []const u8,
};

const NodeType = enum {
    Number,
    Identifier,
    BinaryOp,
    Assignment,
    VarDecl,
    Return,
    If,
    While,
    For,
    Comparison,
    LogicalOp,
    UnaryOp,
};

const Node = struct {
    type: NodeType,
    value: ?i64 = null,
    name: ?[]const u8 = null,
    op: ?TokenType = null, // Store the TokenType of the operator
    left: ?*Node = null,
    right: ?*Node = null,
    condition: ?*Node = null,
    body: ?[]*Node = null,
    else_body: ?[]*Node = null,
    init: ?*Node = null,
    update: ?*Node = null,
};

const Lexer = struct {
    source: []const u8,
    pos: usize,

    fn init(source: []const u8) Lexer {
        return Lexer{ .source = source, .pos = 0 };
    }

    fn nextToken(self: *Lexer) Token {
        while (self.pos < self.source.len) {
            if (std.ascii.isWhitespace(self.source[self.pos])) {
                self.pos += 1;
                continue;
            }
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
                return Token{ .type = .Plus, .value = "+" };
            },
            '-' => {
                if (self.pos < self.source.len and self.source[self.pos] == '=') {
                    self.pos += 1;
                    return Token{ .type = .MinusEqual, .value = "-=" };
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

const Parser = struct {
    tokens: []Token,
    pos: usize,
    allocator: std.mem.Allocator,

    fn init(tokens: []Token, allocator: std.mem.Allocator) Parser {
        return Parser{ .tokens = tokens, .pos = 0, .allocator = allocator };
    }

    fn current(self: *Parser) ?Token {
        if (self.pos < self.tokens.len) return self.tokens[self.pos];
        return null;
    }

    fn advance(self: *Parser) void {
        self.pos += 1;
    }

    fn parseExpr(self: *Parser) anyerror!*Node {
        const left = try self.parseLogicalOr();
        if (self.current()) |token| {
            switch (token.type) {
                .Equal, .PlusEqual, .MinusEqual, .StarEqual, .SlashEqual, .PercentEqual => {
                    if (left.type != .Identifier) @panic("Invalid lvalue for assignment");
                    self.advance();
                    const right = try self.parseExpr();
                    const node = try self.allocator.create(Node);
                    node.* = Node{ .type = .Assignment, .name = left.name, .op = token.type, .right = right };
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
            if (token.type == .Minus or token.type == .Bang or token.type == .Tilde) {
                self.advance();
                const right = try self.parseUnary();
                const node = try self.allocator.create(Node);
                node.* = Node{ .type = .UnaryOp, .op = token.type, .right = right };
                return node;
            }
        }
        return try self.parseFactor();
    }

    fn parseFactor(self: *Parser) anyerror!*Node {
        const token = self.current() orelse @panic("Unexpected EOF");
        self.advance();
        if (token.type == .Number) {
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Number, .value = try std.fmt.parseInt(i64, token.value, 10) };
            return node;
        } else if (token.type == .Identifier) {
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Identifier, .name = token.value };
            return node;
        } else if (token.type == .LParen) {
            const expr = try self.parseExpr();
            if (self.current().?.type != .RParen) @panic("Expected )");
            self.advance();
            return expr;
        }
        @panic("Expected number, identifier, or (");
    }

    fn parseStmt(self: *Parser) anyerror!*Node {
        const token = self.current() orelse @panic("Unexpected EOF");
        if (token.type == .IntKeyword) {
            self.advance();
            const ident = self.current() orelse @panic("Expected identifier");
            if (ident.type != .Identifier) @panic("Expected identifier");
            self.advance();
            if (self.current().?.type != .Equal) @panic("Expected =");
            self.advance();
            const expr = try self.parseExpr();
            if (self.current().?.type != .Semicolon) @panic("Expected ;");
            self.advance();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .VarDecl, .name = ident.value, .right = expr };
            return node;
        } else if (token.type == .ReturnKeyword) {
            self.advance();
            const expr = try self.parseExpr();
            if (self.current().?.type != .Semicolon) @panic("Expected ;");
            self.advance();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .Return, .right = expr };
            return node;
        } else if (token.type == .IfKeyword) {
            self.advance();
            if (self.current().?.type != .LParen) @panic("Expected (");
            self.advance();
            const cond = try self.parseExpr();
            if (self.current().?.type != .RParen) @panic("Expected )");
            self.advance();
            if (self.current().?.type != .LBrace) @panic("Expected {");
            self.advance();
            var body = std.ArrayList(*Node).init(self.allocator);
            while (self.current().?.type != .RBrace) {
                try body.append(try self.parseStmt());
            }
            self.advance();
            var else_body: ?[]*Node = null;
            if (self.current()) |t| {
                if (t.type == .ElseKeyword) {
                    self.advance();
                    if (self.current().?.type != .LBrace) @panic("Expected {");
                    self.advance();
                    var else_stmts = std.ArrayList(*Node).init(self.allocator);
                    while (self.current().?.type != .RBrace) {
                        try else_stmts.append(try self.parseStmt());
                    }
                    self.advance();
                    else_body = try else_stmts.toOwnedSlice();
                }
            }
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .If, .condition = cond, .body = try body.toOwnedSlice(), .else_body = else_body };
            return node;
        } else if (token.type == .WhileKeyword) {
            self.advance();
            if (self.current().?.type != .LParen) @panic("Expected (");
            self.advance();
            const cond = try self.parseExpr();
            if (self.current().?.type != .RParen) @panic("Expected )");
            self.advance();
            if (self.current().?.type != .LBrace) @panic("Expected {");
            self.advance();
            var body = std.ArrayList(*Node).init(self.allocator);
            while (self.current().?.type != .RBrace) {
                try body.append(try self.parseStmt());
            }
            self.advance();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .While, .condition = cond, .body = try body.toOwnedSlice() };
            return node;
        } else if (token.type == .ForKeyword) {
            self.advance();
            if (self.current().?.type != .LParen) @panic("Expected (");
            self.advance();
            const init_stmt = try self.parseStmt();
            const cond = try self.parseExpr();
            if (self.current().?.type != .Semicolon) @panic("Expected ;");
            self.advance();
            const update = try self.parseExpr();
            if (self.current().?.type != .RParen) @panic("Expected )");
            self.advance();
            if (self.current().?.type != .LBrace) @panic("Expected {");
            self.advance();
            var body = std.ArrayList(*Node).init(self.allocator);
            while (self.current().?.type != .RBrace) {
                try body.append(try self.parseStmt());
            }
            self.advance();
            const node = try self.allocator.create(Node);
            node.* = Node{ .type = .For, .init = init_stmt, .condition = cond, .update = update, .body = try body.toOwnedSlice() };
            return node;
        } else if (token.type == .Identifier) {
            const expr = try self.parseExpr();
            if (self.current().?.type != .Semicolon) @panic("Expected ; after expression");
            self.advance();
            return expr;
        }
        @panic("Invalid statement");
    }

    fn parseProgram(self: *Parser) anyerror![]*Node {
        var stmts = std.ArrayList(*Node).init(self.allocator);
        while (self.current()) |token| {
            if (token.type == .EOF) break;
            try stmts.append(try self.parseStmt());
        }
        return try stmts.toOwnedSlice();
    }
};

const CodeGen = struct {
    writer: std.fs.File.Writer,
    label_count: usize,
    vars: std.StringHashMap(void),
    arch: Arch,

    fn init(writer: std.fs.File.Writer, allocator: std.mem.Allocator, arch: Arch) CodeGen {
        return CodeGen{ .writer = writer, .label_count = 0, .vars = std.StringHashMap(void).init(allocator), .arch = arch };
    }

    fn newLabel(self: *CodeGen) []const u8 {
        const label = std.fmt.allocPrint(std.heap.page_allocator, "L{}", .{self.label_count}) catch @panic("Allocation failed");
        self.label_count += 1;
        return label;
    }

    fn registerVar(self: *CodeGen, name: []const u8) !void {
        try self.vars.put(name, {});
    }

    fn genExpr(self: *CodeGen, node: *Node) !void {
        switch (node.type) {
            .Number => {
                if (self.arch == .arm64) {
                    try self.writer.print("    mov x0, #{}\n", .{node.value.?});
                } else {
                    try self.writer.print("    movq ${}, %rax\n", .{node.value.?});
                }
            },
            .Identifier => {
                if (self.arch == .arm64) {
                    try self.writer.print("    adrp x8, _{s}@PAGE\n", .{node.name.?});
                    try self.writer.print("    ldr x0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                } else {
                    try self.writer.print("    movq _{s}(%rip), %rax\n", .{node.name.?});
                }
            },
            .Assignment => {
                if (node.op == null or node.op.? == .Equal) {
                    try self.genExpr(node.right.?);
                } else {
                    if (self.arch == .arm64) {
                        try self.writer.print("    adrp x8, _{s}@PAGE\n", .{node.name.?});
                        try self.writer.print("    ldr x0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                        try self.writer.print("    str x0, [sp, #-16]!\n", .{});
                    } else {
                        try self.writer.print("    movq _{s}(%rip), %rax\n", .{node.name.?});
                        try self.writer.print("    pushq %rax\n", .{});
                    }
                    try self.genExpr(node.right.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    mov x1, x0\n", .{});
                        try self.writer.print("    ldr x0, [sp], #16\n", .{});
                        switch (node.op.?) {
                            .PlusEqual => try self.writer.print("    add x0, x0, x1\n", .{}),
                            .MinusEqual => try self.writer.print("    sub x0, x0, x1\n", .{}),
                            .StarEqual => try self.writer.print("    mul x0, x0, x1\n", .{}),
                            .SlashEqual => try self.writer.print("    sdiv x0, x0, x1\n", .{}),
                            .PercentEqual => {
                                try self.writer.print("    sdiv x2, x0, x1\n", .{});
                                try self.writer.print("    msub x0, x2, x1, x0\n", .{});
                            },
                            else => unreachable,
                        }
                    } else {
                        try self.writer.print("    movq %rax, %rbx\n", .{});
                        try self.writer.print("    popq %rax\n", .{});
                        switch (node.op.?) {
                            .PlusEqual => try self.writer.print("    addq %rbx, %rax\n", .{}),
                            .MinusEqual => try self.writer.print("    subq %rbx, %rax\n", .{}),
                            .StarEqual => try self.writer.print("    imulq %rbx, %rax\n", .{}),
                            .SlashEqual => {
                                try self.writer.print("    cqo\n", .{});
                                try self.writer.print("    idivq %rbx\n", .{});
                            },
                            .PercentEqual => {
                                try self.writer.print("    cqo\n", .{});
                                try self.writer.print("    idivq %rbx\n", .{});
                                try self.writer.print("    movq %rdx, %rax\n", .{});
                            },
                            else => unreachable,
                        }
                    }
                }
                if (self.arch == .arm64) {
                    try self.writer.print("    adrp x8, _{s}@PAGE\n", .{node.name.?});
                    try self.writer.print("    str x0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                } else {
                    try self.writer.print("    movq %rax, _{s}(%rip)\n", .{node.name.?});
                }
            },
            .BinaryOp => {
                try self.genExpr(node.left.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    str x0, [sp, #-16]!\n", .{});
                    try self.genExpr(node.right.?);
                    try self.writer.print("    mov x1, x0\n", .{});
                    try self.writer.print("    ldr x0, [sp], #16\n", .{});
                    switch (node.op.?) {
                        .Plus => try self.writer.print("    add x0, x0, x1\n", .{}),
                        .Minus => try self.writer.print("    sub x0, x0, x1\n", .{}),
                        .Star => try self.writer.print("    mul x0, x0, x1\n", .{}),
                        .Slash => try self.writer.print("    sdiv x0, x0, x1\n", .{}),
                        .Percent => {
                            try self.writer.print("    sdiv x2, x0, x1\n", .{});
                            try self.writer.print("    msub x0, x2, x1, x0\n", .{});
                        },
                        .Ampersand => try self.writer.print("    and x0, x0, x1\n", .{}),
                        .Pipe => try self.writer.print("    orr x0, x0, x1\n", .{}),
                        .Caret => try self.writer.print("    eor x0, x0, x1\n", .{}),
                        .LessLess => try self.writer.print("    lsl x0, x0, x1\n", .{}),
                        .GreaterGreater => try self.writer.print("    asr x0, x0, x1\n", .{}),
                        else => @panic("Invalid binary operator"),
                    }
                } else {
                    try self.writer.print("    pushq %rax\n", .{});
                    try self.genExpr(node.right.?);
                    try self.writer.print("    movq %rax, %rbx\n", .{});
                    try self.writer.print("    popq %rax\n", .{});
                    switch (node.op.?) {
                        .Plus => try self.writer.print("    addq %rbx, %rax\n", .{}),
                        .Minus => try self.writer.print("    subq %rbx, %rax\n", .{}),
                        .Star => try self.writer.print("    imulq %rbx, %rax\n", .{}),
                        .Slash => {
                            try self.writer.print("    cqo\n", .{});
                            try self.writer.print("    idivq %rbx\n", .{});
                        },
                        .Percent => {
                            try self.writer.print("    cqo\n", .{});
                            try self.writer.print("    idivq %rbx\n", .{});
                            try self.writer.print("    movq %rdx, %rax\n", .{});
                        },
                        .Ampersand => try self.writer.print("    andq %rbx, %rax\n", .{}),
                        .Pipe => try self.writer.print("    orq %rbx, %rax\n", .{}),
                        .Caret => try self.writer.print("    xorq %rbx, %rax\n", .{}),
                        .LessLess => {
                            try self.writer.print("    movq %rbx, %rcx\n", .{});
                            try self.writer.print("    shlq %cl, %rax\n", .{});
                        },
                        .GreaterGreater => {
                            try self.writer.print("    movq %rbx, %rcx\n", .{});
                            try self.writer.print("    sarq %cl, %rax\n", .{});
                        },
                        else => @panic("Invalid binary operator"),
                    }
                }
            },
            .Comparison => {
                try self.genExpr(node.left.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    str x0, [sp, #-16]!\n", .{});
                    try self.genExpr(node.right.?);
                    try self.writer.print("    mov x1, x0\n", .{});
                    try self.writer.print("    ldr x0, [sp], #16\n", .{});
                    try self.writer.print("    cmp x0, x1\n", .{});
                    const label_true = self.newLabel();
                    const label_end = self.newLabel();
                    switch (node.op.?) {
                        .Greater => try self.writer.print("    b.gt {s}\n", .{label_true}),
                        .Less => try self.writer.print("    b.lt {s}\n", .{label_true}),
                        .GreaterEqual => try self.writer.print("    b.ge {s}\n", .{label_true}),
                        .LessEqual => try self.writer.print("    b.le {s}\n", .{label_true}),
                        .EqualEqual => try self.writer.print("    b.eq {s}\n", .{label_true}),
                        .NotEqual => try self.writer.print("    b.ne {s}\n", .{label_true}),
                        else => @panic("Invalid comparison"),
                    }
                    try self.writer.print("    mov x0, #0\n", .{});
                    try self.writer.print("    b {s}\n", .{label_end});
                    try self.writer.print("{s}:\n", .{label_true});
                    try self.writer.print("    mov x0, #1\n", .{});
                    try self.writer.print("{s}:\n", .{label_end});
                } else {
                    try self.writer.print("    pushq %rax\n", .{});
                    try self.genExpr(node.right.?);
                    try self.writer.print("    movq %rax, %rbx\n", .{});
                    try self.writer.print("    popq %rax\n", .{});
                    try self.writer.print("    cmpq %rbx, %rax\n", .{});
                    const label_true = self.newLabel();
                    const label_end = self.newLabel();
                    switch (node.op.?) {
                        .Greater => try self.writer.print("    jg {s}\n", .{label_true}),
                        .Less => try self.writer.print("    jl {s}\n", .{label_true}),
                        .GreaterEqual => try self.writer.print("    jge {s}\n", .{label_true}),
                        .LessEqual => try self.writer.print("    jle {s}\n", .{label_true}),
                        .EqualEqual => try self.writer.print("    je {s}\n", .{label_true}),
                        .NotEqual => try self.writer.print("    jne {s}\n", .{label_true}),
                        else => @panic("Invalid comparison"),
                    }
                    try self.writer.print("    movq $0, %rax\n", .{});
                    try self.writer.print("    jmp {s}\n", .{label_end});
                    try self.writer.print("{s}:\n", .{label_true});
                    try self.writer.print("    movq $1, %rax\n", .{});
                    try self.writer.print("{s}:\n", .{label_end});
                }
            },
            .LogicalOp => {
                const label_end = self.newLabel();
                if (node.op.? == .AmpersandAmpersand) {
                    try self.genExpr(node.left.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n", .{});
                        try self.writer.print("    b.eq {s}\n", .{label_end});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n", .{});
                        try self.writer.print("    je {s}\n", .{label_end});
                    }
                    try self.genExpr(node.right.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n", .{});
                        try self.writer.print("    cset x0, ne\n", .{});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n", .{});
                        try self.writer.print("    setne %al\n", .{});
                        try self.writer.print("    movzbl %al, %eax\n", .{});
                    }
                } else if (node.op.? == .PipePipe) {
                    try self.genExpr(node.left.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n", .{});
                        try self.writer.print("    b.ne {s}\n", .{label_end});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n", .{});
                        try self.writer.print("    jne {s}\n", .{label_end});
                    }
                    try self.genExpr(node.right.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n", .{});
                        try self.writer.print("    cset x0, ne\n", .{});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n", .{});
                        try self.writer.print("    setne %al\n", .{});
                        try self.writer.print("    movzbl %al, %eax\n", .{});
                    }
                }
                try self.writer.print("{s}:\n", .{label_end});
            },
            .UnaryOp => {
                try self.genExpr(node.right.?);
                if (node.op.? == .Minus) {
                    if (self.arch == .arm64) {
                        try self.writer.print("    neg x0, x0\n", .{});
                    } else {
                        try self.writer.print("    negq %rax\n", .{});
                    }
                } else if (node.op.? == .Bang) {
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n", .{});
                        try self.writer.print("    cset x0, eq\n", .{});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n", .{});
                        try self.writer.print("    sete %al\n", .{});
                        try self.writer.print("    movzbl %al, %eax\n", .{});
                    }
                } else if (node.op.? == .Tilde) {
                    if (self.arch == .arm64) {
                        try self.writer.print("    mvn x0, x0\n", .{});
                    } else {
                        try self.writer.print("    notq %rax\n", .{});
                    }
                }
            },
            else => @panic("Invalid expression node"),
        }
    }

    fn genStmt(self: *CodeGen, node: *Node) !void {
        switch (node.type) {
            .VarDecl => {
                try self.registerVar(node.name.?);
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    adrp x8, _{s}@PAGE\n", .{node.name.?});
                    try self.writer.print("    str x0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                } else {
                    try self.writer.print("    movq %rax, _{s}(%rip)\n", .{node.name.?});
                }
            },
            .Assignment => {
                try self.genExpr(node);
            },
            .Return => {
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    ldp x29, x30, [sp], #16\n", .{});
                    try self.writer.print("    ret\n", .{});
                } else {
                    try self.writer.print("    movq %rax, %rdi\n", .{});
                    try self.writer.print("    movq $0x2000001, %rax\n", .{});
                    try self.writer.print("    syscall\n", .{});
                }
            },
            .If => {
                const else_label = self.newLabel();
                const end_label = self.newLabel();
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    cmp x0, #0\n", .{});
                    try self.writer.print("    b.eq {s}\n", .{else_label});
                } else {
                    try self.writer.print("    cmpq $0, %rax\n", .{});
                    try self.writer.print("    je {s}\n", .{else_label});
                }
                for (node.body.?) |stmt| {
                    try self.genStmt(stmt);
                }
                if (self.arch == .arm64) {
                    try self.writer.print("    b {s}\n", .{end_label});
                } else {
                    try self.writer.print("    jmp {s}\n", .{end_label});
                }
                try self.writer.print("{s}:\n", .{else_label});
                if (node.else_body) |else_stmts| {
                    for (else_stmts) |stmt| {
                        try self.genStmt(stmt);
                    }
                }
                try self.writer.print("{s}:\n", .{end_label});
            },
            .While => {
                const start_label = self.newLabel();
                const end_label = self.newLabel();
                try self.writer.print("{s}:\n", .{start_label});
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    cmp x0, #0\n", .{});
                    try self.writer.print("    b.eq {s}\n", .{end_label});
                } else {
                    try self.writer.print("    cmpq $0, %rax\n", .{});
                    try self.writer.print("    je {s}\n", .{end_label});
                }
                for (node.body.?) |stmt| {
                    try self.genStmt(stmt);
                }
                if (self.arch == .arm64) {
                    try self.writer.print("    b {s}\n", .{start_label});
                } else {
                    try self.writer.print("    jmp {s}\n", .{start_label});
                }
                try self.writer.print("{s}:\n", .{end_label});
            },
            .For => {
                const start_label = self.newLabel();
                const end_label = self.newLabel();
                try self.genStmt(node.init.?);
                try self.writer.print("{s}:\n", .{start_label});
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    cmp x0, #0\n", .{});
                    try self.writer.print("    b.eq {s}\n", .{end_label});
                } else {
                    try self.writer.print("    cmpq $0, %rax\n", .{});
                    try self.writer.print("    je {s}\n", .{end_label});
                }
                for (node.body.?) |stmt| {
                    try self.genStmt(stmt);
                }
                try self.genExpr(node.update.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    b {s}\n", .{start_label});
                } else {
                    try self.writer.print("    jmp {s}\n", .{start_label});
                }
                try self.writer.print("{s}:\n", .{end_label});
            },
            else => @panic("Invalid statement node"),
        }
    }

    fn genProgram(self: *CodeGen, nodes: []*Node) !void {
        if (self.arch == .arm64) {
            try self.writer.print(".text\n", .{});
            try self.writer.print(".globl _main\n", .{});
            try self.writer.print(".p2align 2\n", .{});
            try self.writer.print("_main:\n", .{});
            try self.writer.print("    stp x29, x30, [sp, #-16]!\n", .{});
            try self.writer.print("    mov x29, sp\n", .{});
            for (nodes) |node| {
                try self.genStmt(node);
            }
            try self.writer.print("    mov x0, #0\n", .{});
            try self.writer.print("    ldp x29, x30, [sp], #16\n", .{});
            try self.writer.print("    ret\n", .{});
            var it = self.vars.keyIterator();
            while (it.next()) |key| {
                try self.writer.print(".comm _{s}, 8\n", .{key.*});
            }
        } else {
            try self.writer.print(".text\n", .{});
            try self.writer.print(".globl _main\n", .{});
            try self.writer.print("_main:\n", .{});
            try self.writer.print("    pushq %rbp\n", .{});
            try self.writer.print("    movq %rsp, %rbp\n", .{});
            for (nodes) |node| {
                try self.genStmt(node);
            }
            try self.writer.print("    movq $0x2000001, %rax\n", .{});
            try self.writer.print("    movq $0, %rdi\n", .{});
            try self.writer.print("    syscall\n", .{});
            var it = self.vars.keyIterator();
            while (it.next()) |key| {
                try self.writer.print(".comm _{s}, 8, 3\n", .{key.*});
            }
        }
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var arch: Arch = if (builtin.cpu.arch == .aarch64) .arm64 else .x86_64;
    var input_file: ?[]const u8 = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--arch")) {
            i += 1;
            if (i < args.len) {
                if (std.mem.eql(u8, args[i], "x86_64")) {
                    arch = .x86_64;
                } else if (std.mem.eql(u8, args[i], "arm64")) {
                    arch = .arm64;
                } else {
                    std.debug.print("Unknown architecture: {s}\n", .{args[i]});
                    std.process.exit(1);
                }
            }
        } else {
            input_file = args[i];
        }
    }

    const source = if (input_file) |path| try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024) else
        \\int x = 0;
        \\for (int i = 0; i < 5; i = i + 1) {
        \\    x = x + i;
        \\}
        \\if (x > 9) {
        \\    x = 42;
        \\} else {
        \\    x = 0;
        \\}
        \\return x;
    ;
    var lexer = Lexer.init(source);
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    while (true) {
        const token = lexer.nextToken();
        try tokens.append(token);
        if (token.type == .EOF) break;
    }

    var parser = Parser.init(try tokens.toOwnedSlice(), allocator);
    const ast = try parser.parseProgram();

    const file = try std.fs.cwd().createFile("out.asm", .{});
    defer file.close();
    var codegen = CodeGen.init(file.writer(), allocator, arch);
    try codegen.genProgram(ast);
}
