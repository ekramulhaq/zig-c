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
    Compound,
    Ternary,
    Comparison,
    LogicalOp,
    UnaryOp,
    TypeCast,
    PostfixOp,
    Function,
    FunctionCall,
    AddressOf,
    Deref,
    Sizeof,
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
    Float,
    Double,
    Void,

    pub fn isFloat(self: DataType) bool {
        return self == .Float or self == .Double;
    }
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
    pointer_level: usize = 0,
    value: ?i64 = null,
    fvalue: ?f64 = null,
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
    params_pointer_levels: ?[]usize = null,
    params_struct_names: ?[]?[]const u8 = null,
    is_variadic: bool = false,
    is_prototype: bool = false,
    data: ?[]const u8 = null, // For string literals
    init_value: ?i64 = null, // For global initializers
    finit_value: ?f64 = null, // For global float initializers
    members: ?[]*Node = null, // For struct definitions
    struct_name: ?[]const u8 = null, // For member access and struct variables

    pub fn dump(self: *const Node, indent: usize) void {
        var i: usize = 0;
        while (i < indent) : (i += 1) std.debug.print("  ", .{});

        std.debug.print("- {s}", .{@tagName(self.type)});
        if (self.name) |name| std.debug.print(" name='{s}'", .{name});
        if (self.value) |val| std.debug.print(" value={}", .{val});
        if (self.fvalue) |fval| std.debug.print(" fvalue={}", .{fval});
        if (self.op) |op| std.debug.print(" op={s}", .{@tagName(op)});
        if (self.struct_name) |sn| std.debug.print(" struct='{s}'", .{sn});
        std.debug.print("\n", .{});

        if (self.left) |l| l.dump(indent + 1);
        if (self.right) |r| r.dump(indent + 1);
        if (self.condition) |c| {
            i = 0; while (i < indent + 1) : (i += 1) std.debug.print("  ", .{});
            std.debug.print("(condition)\n", .{});
            c.dump(indent + 2);
        }
        if (self.init) |init_node| {
            i = 0; while (i < indent + 1) : (i += 1) std.debug.print("  ", .{});
            std.debug.print("(init)\n", .{});
            init_node.dump(indent + 2);
        }
        if (self.update) |u| {
            i = 0; while (i < indent + 1) : (i += 1) std.debug.print("  ", .{});
            std.debug.print("(update)\n", .{});
            u.dump(indent + 2);
        }
        if (self.body) |body| {
            i = 0; while (i < indent + 1) : (i += 1) std.debug.print("  ", .{});
            std.debug.print("(body)\n", .{});
            for (body) |stmt| stmt.dump(indent + 2);
        }
        if (self.else_body) |else_body| {
            i = 0; while (i < indent + 1) : (i += 1) std.debug.print("  ", .{});
            std.debug.print("(else)\n", .{});
            for (else_body) |stmt| stmt.dump(indent + 2);
        }
        if (self.args) |args| {
            i = 0; while (i < indent + 1) : (i += 1) std.debug.print("  ", .{});
            std.debug.print("(args)\n", .{});
            for (args) |arg| arg.dump(indent + 2);
        }
        if (self.members) |members| {
            i = 0; while (i < indent + 1) : (i += 1) std.debug.print("  ", .{});
            std.debug.print("(members)\n", .{});
            for (members) |m| m.dump(indent + 2);
        }
    }
};
