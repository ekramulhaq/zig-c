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
    DoWhile,
    Break,
    Continue,
    Ternary,
    Comparison,
    LogicalOp,
    UnaryOp,
    PostfixOp,
    Function,
    FunctionCall,
    AddressOf,
    Deref,
    String,
    Switch,
    Case,
    ArrayDecl,
    Index,
    StructDecl,
    EnumDecl,
    MemberAccess,
};

pub const DataType = enum {
    Int,
    Char,
    Void,
};

pub const Type = struct {
    id: DataType,
    is_pointer: bool = false,
    struct_name: ?[]const u8 = null,
};

/// Represents a node in the Abstract Syntax Tree.
pub const Node = struct {
    type: NodeType,
    data_type: DataType = .Int,
    is_pointer: bool = false,
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
    params_types: ?[]DataType = null,
    params_is_pointer: ?[]bool = null,
    params_struct_names: ?[]?[]const u8 = null,
    data: ?[]const u8 = null, // For string literals
    init_value: ?i64 = null, // For global initializers
    members: ?[]*Node = null, // For struct definitions
    struct_name: ?[]const u8 = null, // For member access and struct variables
};
