const std = @import("std");
const token_pkg = @import("token.zig");
const ast = @import("ast.zig");
const type_system = @import("type_system.zig");
const base = @import("parser/base.zig");
const expr_parser = @import("parser/expr.zig");
const stmt_parser = @import("parser/stmt.zig");
const top_level = @import("parser/top_level.zig");

const Token = token_pkg.Token;
const Node = ast.Node;
const TypeSystem = type_system.TypeSystem;

/// Main Parser entry point that coordinates specialized sub-parsers.
pub const Parser = struct {
    base: *base.ParserBase,
    expr_p: *expr_parser.ExprParser,
    stmt_p: *stmt_parser.StmtParser,
    top_p: *top_level.TopLevelParser,
    type_system: TypeSystem,
    allocator: std.mem.Allocator,

    /// Creates and initializes a new Parser on the heap.
    pub fn create(tokens: []Token, allocator: std.mem.Allocator) !*Parser {
        const self = try allocator.create(Parser);
        self.type_system = TypeSystem.init(allocator);
        self.allocator = allocator;
        
        self.base = try allocator.create(base.ParserBase);
        self.expr_p = try allocator.create(expr_parser.ExprParser);
        self.stmt_p = try allocator.create(stmt_parser.StmtParser);
        self.top_p = try allocator.create(top_level.TopLevelParser);
        
        self.base.* = base.ParserBase.init(tokens, allocator, &self.type_system);
        self.expr_p.* = expr_parser.ExprParser.init(self.base);
        self.stmt_p.* = stmt_parser.StmtParser.init(self.base, self.expr_p);
        self.top_p.* = top_level.TopLevelParser.init(self.base, self.expr_p, self.stmt_p);
        
        return self;
    }

    pub fn deinit(self: *Parser) void {
        self.type_system.deinit();
        self.allocator.destroy(self.top_p);
        self.allocator.destroy(self.stmt_p);
        self.allocator.destroy(self.expr_p);
        self.allocator.destroy(self.base);
        self.allocator.destroy(self);
    }

    /// Parses the entire program by delegating to the top-level parser.
    pub fn parseProgram(self: *Parser) anyerror![]*Node {
        return self.top_p.parseProgram();
    }
};
