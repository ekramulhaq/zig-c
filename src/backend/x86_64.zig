const std = @import("std");
const ast = @import("../ast.zig");
const core = @import("core.zig");

pub const X86_64Backend = struct {
    writer: std.fs.File.Writer,

    fn getSubReg(reg: []const u8, size: i32) []const u8 {
        if (std.mem.eql(u8, reg, "%rax")) {
            return switch (size) {
                1 => "%al",
                2 => "%ax",
                4 => "%eax",
                else => "%rax",
            };
        }
        if (std.mem.eql(u8, reg, "%rdi")) {
            return switch (size) {
                1 => "%dil",
                2 => "%di",
                4 => "%edi",
                else => "%rdi",
            };
        }
        if (std.mem.eql(u8, reg, "%rsi")) {
            return switch (size) {
                1 => "%sil",
                2 => "%si",
                4 => "%esi",
                else => "%rsi",
            };
        }
        if (std.mem.eql(u8, reg, "%rdx")) {
            return switch (size) {
                1 => "%dl",
                2 => "%dx",
                4 => "%edx",
                else => "%rdx",
            };
        }
        if (std.mem.eql(u8, reg, "%rcx")) {
            return switch (size) {
                1 => "%cl",
                2 => "%cx",
                4 => "%ecx",
                else => "%rcx",
            };
        }
        if (std.mem.eql(u8, reg, "%r8")) {
            return switch (size) {
                1 => "%r8b",
                2 => "%r8w",
                4 => "%r8d",
                else => "%r8",
            };
        }
        if (std.mem.eql(u8, reg, "%r9")) {
            return switch (size) {
                1 => "%r9b",
                2 => "%r9w",
                4 => "%r9d",
                else => "%r9",
            };
        }
        if (std.mem.eql(u8, reg, "%r10")) {
            return switch (size) {
                1 => "%r10b",
                2 => "%r10w",
                4 => "%r10d",
                else => "%r10",
            };
        }
        if (std.mem.eql(u8, reg, "%r11")) {
            return switch (size) {
                1 => "%r11b",
                2 => "%r11w",
                4 => "%r11d",
                else => "%r11",
            };
        }
        return reg;
    }

    pub fn emitLoad(self: *X86_64Backend, reg: []const u8, offset: i32, size: i32, is_float: bool) !void {
        if (is_float) {
            if (size == 4) {
                // Load 32-bit float and promote to double
                try self.writer.print("    movss {}(%rbp), {s}\n", .{offset, reg});
                try self.writer.print("    cvtss2sd {s}, {s}\n", .{reg, reg});
            } else {
                try self.writer.print("    movsd {}(%rbp), {s}\n", .{offset, reg});
            }
        } else {
            switch (size) {
                1 => try self.writer.print("    movzbq {}(%rbp), {s}\n", .{offset, reg}),
                2 => try self.writer.print("    movzwq {}(%rbp), {s}\n", .{offset, reg}),
                4 => try self.writer.print("    movslq {}(%rbp), {s}\n", .{offset, reg}),
                8 => try self.writer.print("    movq {}(%rbp), {s}\n", .{offset, reg}),
                else => unreachable,
            }
        }
    }

    pub fn emitStore(self: *X86_64Backend, reg: []const u8, offset: i32, size: i32, is_float: bool) !void {
        if (is_float) {
            if (size == 4) {
                // Demote from double to float before storing
                try self.writer.print("    cvtsd2ss {s}, {s}\n", .{reg, reg});
                try self.writer.print("    movss {s}, {}(%rbp)\n", .{reg, offset});
            } else {
                try self.writer.print("    movsd {s}, {}(%rbp)\n", .{reg, offset});
            }
        } else {
            const sub_reg = getSubReg(reg, size);
            switch (size) {
                1 => try self.writer.print("    movb {s}, {}(%rbp)\n", .{sub_reg, offset}),
                2 => try self.writer.print("    movw {s}, {}(%rbp)\n", .{sub_reg, offset}),
                4 => try self.writer.print("    movl {s}, {}(%rbp)\n", .{sub_reg, offset}),
                8 => try self.writer.print("    movq {s}, {}(%rbp)\n", .{sub_reg, offset}),
                else => unreachable,
            }
        }
    }

    pub fn emitFunctionProlog(self: *X86_64Backend, name: []const u8) !void {
        try self.writer.print(".p2align 4, 0x90\n_{s}:\n    pushq %rbp\n    movq %rsp, %rbp\n    subq $2048, %rsp\n", .{name});
    }

    pub fn emitFunctionEpilog(self: *X86_64Backend) !void {
        try self.writer.print("    movq %rbp, %rsp\n    popq %rbp\n    ret\n", .{});
    }
};
