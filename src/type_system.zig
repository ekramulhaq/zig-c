const std = @import("std");
const ast = @import("ast.zig");

/// Represents a member within a struct.
pub const StructMember = struct {
    name: []const u8,
    offset: i32,
    data_type: ast.DataType,
    is_pointer: bool,
    struct_name: ?[]const u8,
};

/// Represents the memory layout of a struct.
pub const StructLayout = struct {
    name: []const u8,
    members: std.StringHashMap(StructMember),
    total_size: i32,

    pub fn init(name: []const u8, allocator: std.mem.Allocator) StructLayout {
        return .{
            .name = name,
            .members = std.StringHashMap(StructMember).init(allocator),
            .total_size = 0,
        };
    }

    pub fn deinit(self: *StructLayout) void {
        self.members.deinit();
    }
};

/// TypeSystem manages all known types, including structs and typedefs.
pub const TypeSystem = struct {
    allocator: std.mem.Allocator,
    structs: std.StringHashMap(StructLayout),
    typedefs: std.StringHashMap(ast.Type),
    enums: std.StringHashMap(i64),

    pub fn init(allocator: std.mem.Allocator) TypeSystem {
        return .{
            .allocator = allocator,
            .structs = std.StringHashMap(StructLayout).init(allocator),
            .typedefs = std.StringHashMap(ast.Type).init(allocator),
            .enums = std.StringHashMap(i64).init(allocator),
        };
    }

    pub fn deinit(self: *TypeSystem) void {
        var it = self.structs.valueIterator();
        while (it.next()) |layout| {
            layout.deinit();
        }
        self.structs.deinit();
        self.typedefs.deinit();
        self.enums.deinit();
    }

    pub fn addStruct(self: *TypeSystem, name: []const u8, members: []*ast.Node) !void {
        var layout = StructLayout.init(name, self.allocator);
        var offset: i32 = 0;

        for (members) |member| {
            if (member.type == .VarDecl) {
                const m_size = self.getTypeSize(member.data_type, member.is_pointer, member.struct_name);
                try layout.members.put(member.name.?, .{
                    .name = member.name.?,
                    .offset = offset,
                    .data_type = member.data_type,
                    .is_pointer = member.is_pointer,
                    .struct_name = member.struct_name,
                });
                offset += m_size;
            }
        }
        layout.total_size = offset;
        try self.structs.put(name, layout);
    }

    pub fn getTypeSize(self: *TypeSystem, data_type: ast.DataType, is_pointer: bool, struct_name: ?[]const u8) i32 {
        if (is_pointer) return 8;
        if (struct_name) |sn| {
            if (self.structs.get(sn)) |layout| {
                return layout.total_size;
            }
        }
        switch (data_type) {
            .Int => return 8,
            .Char => return 8, // Padded to 8 for simple alignment in this compiler
            .Void => return 0,
            .Float => return 8,
            .Double => return 8,
        }
    }

    pub fn getMember(self: *TypeSystem, struct_name: []const u8, member_name: []const u8) ?StructMember {
        if (self.structs.get(struct_name)) |layout| {
            return layout.members.get(member_name);
        }
        return null;
    }

    /// Recursively search all structs for a member (fallback for nested pointer access)
    pub fn findMemberAnywhere(self: *TypeSystem, member_name: []const u8) ?StructMember {
        var it = self.structs.valueIterator();
        while (it.next()) |layout| {
            if (layout.members.get(member_name)) |m| return m;
        }
        return null;
    }
};
