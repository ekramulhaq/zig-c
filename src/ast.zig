const std = @import("std");
const TokenType = @import("lexer.zig").TokenType;

/// Represents the type of an AST node.
pub const NodeType = enum {
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
    Function,
    FunctionCall,
    AddressOf,
    Deref,
    String,
};

/// Represents a node in the Abstract Syntax Tree.
pub const Node = struct {
    type: NodeType,
    value: ?i64 = null,
    name: ?[]const u8 = null,
    op: ?TokenType = null,
    left: ?*Node = null,
    right: ?*Node = null,
    condition: ?*Node = null,
    body: ?[]*Node = null,
    else_body: ?[]*Node = null,
    init: ?*Node = null,
    update: ?*Node = null,
    args: ?[]*Node = null,
    params: ?[][]const u8 = null,
    data: ?[]const u8 = null, // For string literals
    init_value: ?i64 = null, // For global initializers
};
