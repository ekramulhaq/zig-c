const std = @import("std");
const ast = @import("../ast.zig");
const core = @import("core.zig");

pub const X86_64Backend = struct {
    writer: std.fs.File.Writer,

    pub fn emitLoad(self: *X86_64Backend, reg: []const u8, offset: i32) !void {
        try self.writer.print("    movq {}(%rbp), {s}\n", .{offset, reg});
    }

    pub fn emitStore(self: *X86_64Backend, reg: []const u8, offset: i32) !void {
        try self.writer.print("    movq {s}, {}(%rbp)\n", .{reg, offset});
    }

    pub fn emitFunctionProlog(self: *X86_64Backend, name: []const u8) !void {
        try self.writer.print(".p2align 4, 0x90\n_{s}:\n    pushq %rbp\n    movq %rsp, %rbp\n    subq $2048, %rsp\n", .{name});
    }

    pub fn emitFunctionEpilog(self: *X86_64Backend) !void {
        try self.writer.print("    movq %rbp, %rsp\n    popq %rbp\n    ret\n", .{});
    }
};
