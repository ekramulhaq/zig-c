const std = @import("std");
const ast = @import("../ast.zig");
const core = @import("core.zig");

pub const Arm64Backend = struct {
    writer: std.fs.File.Writer,

    fn getSubReg(reg: []const u8, size: i32) []const u8 {
        if (size <= 4) {
            if (std.mem.eql(u8, reg, "x0")) return "w0";
            if (std.mem.eql(u8, reg, "x1")) return "w1";
            if (std.mem.eql(u8, reg, "x2")) return "w2";
            if (std.mem.eql(u8, reg, "x3")) return "w3";
            if (std.mem.eql(u8, reg, "x4")) return "w4";
            if (std.mem.eql(u8, reg, "x5")) return "w5";
            if (std.mem.eql(u8, reg, "x6")) return "w6";
            if (std.mem.eql(u8, reg, "x7")) return "w7";
            if (std.mem.eql(u8, reg, "x8")) return "w8";
            if (std.mem.eql(u8, reg, "x9")) return "w9";
        }
        return reg;
    }

    pub fn emitLoad(self: *Arm64Backend, reg: []const u8, offset: i32, size: i32, is_float: bool) !void {
        const sub_reg = getSubReg(reg, size);
        if (is_float) {
            if (size == 4) {
                // Load 32-bit float, then promote to double in d0
                try self.emitRaw("ldr", "s0", offset);
                try self.writer.print("    fcvt d0, s0\n", .{});
            } else {
                try self.emitRaw("ldr", "d0", offset);
            }
        } else {
            const op = switch (size) {
                1 => "ldrsb",
                2 => "ldrsh",
                // Use ldrsw to sign-extend 32-bit int to 64-bit register
                4 => "ldrsw",
                8 => "ldr",
                else => unreachable,
            };
            // ldrsw always needs the full 64-bit register (x0, not w0)
            const target_reg = if (size == 1 or size == 2) reg else reg;
            _ = sub_reg;
            try self.emitRaw(op, target_reg, offset);
        }
    }

    pub fn emitStore(self: *Arm64Backend, reg: []const u8, offset: i32, size: i32, is_float: bool) !void {
        const sub_reg = getSubReg(reg, size);
        if (is_float) {
            if (size == 4) {
                // Demote double d0 back to float s0 before storing
                try self.writer.print("    fcvt s0, d0\n", .{});
                try self.emitRaw("str", "s0", offset);
            } else {
                try self.emitRaw("str", "d0", offset);
            }
        } else {
            const op = switch (size) {
                1 => "strb",
                2 => "strh",
                4 => "str",
                8 => "str",
                else => unreachable,
            };
            try self.emitRaw(op, sub_reg, offset);
        }
    }

    fn emitRaw(self: *Arm64Backend, op: []const u8, reg: []const u8, offset: i32) !void {
        if (offset >= -256 and offset <= 255) {
            try self.writer.print("    {s} {s}, [x29, #{}]\n", .{op, reg, offset});
        } else {
            try self.writer.print("    mov x9, #{}\n", .{offset});
            try self.writer.print("    {s} {s}, [x29, x9]\n", .{op, reg});
        }
    }

    pub fn emitFunctionProlog(self: *Arm64Backend, name: []const u8) !void {
        try self.writer.print(".p2align 2\n_{s}:\n    stp x29, x30, [sp, #-16]!\n    mov x29, sp\n    sub sp, sp, #2048\n", .{name});
    }

    pub fn emitFunctionEpilog(self: *Arm64Backend) !void {
        try self.writer.print("    mov sp, x29\n    ldp x29, x30, [sp], #16\n    ret\n", .{});
    }
};
