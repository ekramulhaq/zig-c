const std = @import("std");
const ast = @import("../ast.zig");
const core = @import("core.zig");

pub const Arm64Backend = struct {
    writer: std.fs.File.Writer,

    pub fn emitLoad(self: *Arm64Backend, reg: []const u8, offset: i32) !void {
        if (offset >= -256 and offset <= 255) {
            try self.writer.print("    ldr {s}, [x29, #{}]\n", .{reg, offset});
        } else {
            try self.writer.print("    mov x9, #{}\n", .{offset});
            try self.writer.print("    ldr {s}, [x29, x9]\n", .{reg});
        }
    }

    pub fn emitStore(self: *Arm64Backend, reg: []const u8, offset: i32) !void {
        if (offset >= -256 and offset <= 255) {
            try self.writer.print("    str {s}, [x29, #{}]\n", .{reg, offset});
        } else {
            try self.writer.print("    mov x9, #{}\n", .{offset});
            try self.writer.print("    str {s}, [x29, x9]\n", .{reg});
        }
    }

    pub fn emitFunctionProlog(self: *Arm64Backend, name: []const u8) !void {
        try self.writer.print(".p2align 2\n_{s}:\n    stp x29, x30, [sp, #-16]!\n    mov x29, sp\n    sub sp, sp, #2048\n", .{name});
    }

    pub fn emitFunctionEpilog(self: *Arm64Backend) !void {
        try self.writer.print("    mov sp, x29\n    ldp x29, x30, [sp], #16\n    ret\n", .{});
    }
};
