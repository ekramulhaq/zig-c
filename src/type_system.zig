const std = @import("std");
const ast = @import("ast.zig");

/// Represents a member within a struct.
pub const StructMember = struct {
    name: []const u8,
    offset: i32,
    data_type: ast.DataType,
    is_pointer: bool,
    is_array: bool,    // true when declared as  type name[N]  inside a struct
    array_len: i32,    // element count (0 if not an array)
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
            offset = try self.addStructMember(&layout, member, offset);
        }
        // Align total size to at least 8 bytes if it contains any 8-byte members
        // For simplicity, let's just always align to 8 if size > 0
        if (offset > 0) {
            offset = (offset + 7) & ~@as(i32, 7);
        }
        layout.total_size = offset;
        try self.structs.put(name, layout);
    }

    fn addStructMember(self: *TypeSystem, layout: *StructLayout, member: *ast.Node, start_offset: i32) !i32 {
        var offset = start_offset;
        if (member.type == .Compound) {
            for (member.body.?) |sub| {
                offset = try self.addStructMember(layout, sub, offset);
            }
        } else if (member.type == .VarDecl) {
            const m_size = self.getTypeSize(member.data_type, member.is_pointer, member.struct_name);
            const m_align = self.getTypeAlign(member.data_type, member.is_pointer, member.struct_name);
            
            // Align offset
            if (m_align > 1) {
                offset = (offset + m_align - 1) & ~(m_align - 1);
            }

            try layout.members.put(member.name.?, .{
                .name = member.name.?,
                .offset = offset,
                .data_type = member.data_type,
                .is_pointer = member.is_pointer,
                .is_array = false,
                .array_len = 0,
                .struct_name = member.struct_name,
            });
            offset += m_size;
        } else if (member.type == .ArrayDecl) {
            const m_size = self.getTypeSize(member.data_type, member.is_pointer, member.struct_name);
            const m_align = self.getTypeAlign(member.data_type, member.is_pointer, member.struct_name);
            const array_elements = if (member.value) |v| @as(i32, @intCast(v)) else 1;
            
            // Align offset
            if (m_align > 1) {
                offset = (offset + m_align - 1) & ~(m_align - 1);
            }

            try layout.members.put(member.name.?, .{
                .name = member.name.?,
                .offset = offset,
                .data_type = member.data_type,
                .is_pointer = member.is_pointer,
                .is_array = true,
                .array_len = array_elements,
                .struct_name = member.struct_name,
            });
            offset += m_size * array_elements;
        }
        return offset;
    }

    pub fn getTypeSize(self: *TypeSystem, data_type: ast.DataType, is_pointer: bool, struct_name: ?[]const u8) i32 {
        if (is_pointer) return 8;
        if (struct_name) |sn| {
            if (self.structs.get(sn)) |layout| {
                return layout.total_size;
            }
        }
        switch (data_type) {
            .Int => return 4,
            .Char => return 1,
            .Void => return 0,
            .Float => return 4,
            .Double => return 8,
        }
    }

    pub fn getTypeAlign(self: *TypeSystem, data_type: ast.DataType, is_pointer: bool, struct_name: ?[]const u8) i32 {
        if (is_pointer) return 8;
        if (struct_name) |sn| {
            if (self.structs.get(sn)) |layout| {
                // For structs, we should return the max alignment of its members.
                // For simplicity, let's just return 8 if we don't track it, 
                // but we should probably track it.
                _ = layout;
                return 8;
            }
        }
        switch (data_type) {
            .Int => return 4,
            .Char => return 1,
            .Void => return 1,
            .Float => return 4,
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
