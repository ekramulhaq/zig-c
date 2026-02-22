const std = @import("std");
const ast = @import("../ast.zig");

pub const Global = struct {
    size: i32,
    init_value: ?i64,
    data_type: ast.DataType,
    is_pointer: bool,
    is_array: bool = false,
    pointer_level: usize = 0,
    struct_name: ?[]const u8,
};

pub const LocalVar = struct {
    offset: i32,
    data_type: ast.DataType,
    is_pointer: bool,
    is_array: bool = false,
    pointer_level: usize = 0,
    struct_name: ?[]const u8,
};
