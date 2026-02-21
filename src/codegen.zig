const std = @import("std");
const ast = @import("ast.zig");
const common = @import("common.zig");
const type_system = @import("type_system.zig");
const backend_core = @import("backend/core.zig");
const arm64 = @import("backend/arm64.zig");
const x86_64 = @import("backend/x86_64.zig");

const Node = ast.Node;
const Arch = common.Arch;
const TypeSystem = type_system.TypeSystem;
const Global = backend_core.Global;
const LocalVar = backend_core.LocalVar;

/// CodeGen converts an AST into assembly code.
pub const CodeGen = struct {
    writer: std.fs.File.Writer,
    label_count: usize,
    vars: std.StringHashMap(LocalVar),
    globals: std.StringHashMap(Global),
    type_system: TypeSystem,
    strings: std.ArrayList([]const u8),
    stack_pos: i32,
    temp_stack_pos: i32,
    break_label: ?[]const u8,
    continue_label: ?[]const u8,
    asm_comments: bool,
    arch: Arch,
    allocator: std.mem.Allocator,
    
    // Concrete backends
    arm64_backend: arm64.Arm64Backend,
    x86_64_backend: x86_64.X86_64Backend,

    /// Initializes a new CodeGen with the given writer, allocator, and target architecture.
    pub fn init(writer: std.fs.File.Writer, allocator: std.mem.Allocator, arch: Arch, ts: TypeSystem) CodeGen {
        return CodeGen{
            .writer = writer,
            .label_count = 0,
            .vars = std.StringHashMap(LocalVar).init(allocator),
            .globals = std.StringHashMap(Global).init(allocator),
            .type_system = ts,
            .strings = std.ArrayList([]const u8).init(allocator),
            .stack_pos = 0,
            .temp_stack_pos = -1024,
            .break_label = null,
            .continue_label = null,
            .asm_comments = false,
            .arch = arch,
            .allocator = allocator,
            .arm64_backend = .{ .writer = writer },
            .x86_64_backend = .{ .writer = writer },
        };
    }

    fn emitLoad(self: *CodeGen, reg: []const u8, offset: i32) !void {
        if (self.arch == .arm64) {
            try self.arm64_backend.emitLoad(reg, offset);
        } else {
            try self.x86_64_backend.emitLoad(reg, offset);
        }
    }

    fn emitStore(self: *CodeGen, reg: []const u8, offset: i32) !void {
        if (self.arch == .arm64) {
            try self.arm64_backend.emitStore(reg, offset);
        } else {
            try self.x86_64_backend.emitStore(reg, offset);
        }
    }

    fn pushTemp(self: *CodeGen) !void {
        try self.emitStore(if (self.arch == .arm64) "x0" else "%rax", self.temp_stack_pos);
        self.temp_stack_pos -= 8;
    }

    fn popTemp(self: *CodeGen, reg: []const u8) !void {
        self.temp_stack_pos += 8;
        try self.emitLoad(reg, self.temp_stack_pos);
    }

    fn newLabel(self: *CodeGen) []const u8 {
        const label = std.fmt.allocPrint(self.allocator, "L{}", .{self.label_count}) catch @panic("Allocation failed");
        self.label_count += 1;
        return label;
    }

    fn registerVar(self: *CodeGen, name: []const u8, size: i32, data_type: ast.DataType, is_pointer: bool, struct_name: ?[]const u8) !LocalVar {
        if (self.vars.get(name)) |local| return local;
        self.stack_pos -= size;
        const local = LocalVar{ .offset = self.stack_pos, .data_type = data_type, .is_pointer = is_pointer, .struct_name = struct_name };
        try self.vars.put(name, local);
        return local;
    }

    fn genAddr(self: *CodeGen, node: *Node) anyerror!void {
        switch (node.type) {
            .Identifier => {
                if (self.vars.get(node.name.?)) |local| {
                    const offset = local.offset;
                    if (self.arch == .arm64) {
                        if (offset >= 0) {
                            try self.writer.print("    add x0, x29, #{}\n", .{offset});
                        } else {
                            try self.writer.print("    sub x0, x29, #{}\n", .{-offset});
                        }
                    } else {
                        try self.writer.print("    leaq {}(%rbp), %rax\n", .{offset});
                    }
                } else if (self.globals.get(node.name.?)) |global| {
                    if (global.is_pointer) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    adrp x8, _{s}@PAGE\n", .{node.name.?});
                            try self.writer.print("    ldr x0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                        } else {
                            try self.writer.print("    movq _{s}(%rip), %rax\n", .{node.name.?});
                        }
                    } else {
                        if (self.arch == .arm64) {
                            try self.writer.print("    adrp x0, _{s}@PAGE\n", .{node.name.?});
                            try self.writer.print("    add x0, x0, _{s}@PAGEOFF\n", .{node.name.?});
                        } else {
                            try self.writer.print("    leaq _{s}(%rip), %rax\n", .{node.name.?});
                        }
                    }
                } else @panic("Undefined variable");
            },
            .Deref => {
                try self.genExpr(node.right.?);
            },
            .Index => {
                var is_ptr = false;
                if (node.left.?.type == .Identifier) {
                    if (self.vars.get(node.left.?.name.?)) |local| {
                        is_ptr = local.is_pointer;
                    } else if (self.globals.get(node.left.?.name.?)) |global| {
                        is_ptr = global.is_pointer;
                    }
                }

                if (is_ptr) {
                    try self.genExpr(node.left.?);
                } else {
                    try self.genAddr(node.left.?);
                }
                try self.pushTemp();
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    lsl x1, x0, #3\n", .{});
                    try self.popTemp("x0");
                    try self.writer.print("    add x0, x0, x1\n", .{});
                } else {
                    try self.writer.print("    shlq $3, %rax\n", .{});
                    try self.writer.print("    movq %rax, %r10\n", .{});
                    try self.popTemp("%rax");
                    try self.writer.print("    addq %r10, %rax\n", .{});
                }
            },
            .MemberAccess => {
                var s_name: ?[]const u8 = null;
                if (node.left.?.type == .Identifier) {
                    if (self.vars.get(node.left.?.name.?)) |local| {
                        s_name = local.struct_name;
                    } else if (self.globals.get(node.left.?.name.?)) |global| {
                        s_name = global.struct_name;
                    }
                } else if (node.left.?.type == .Deref) {
                    const inner = node.left.?.right.?;
                    if (inner.type == .Identifier) {
                        if (self.vars.get(inner.name.?)) |local| {
                            s_name = local.struct_name;
                        } else if (self.globals.get(inner.name.?)) |global| {
                            s_name = global.struct_name;
                        }
                    }
                }

                if (s_name == null) {
                    var it = self.type_system.structs.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.*.members.contains(node.name.?)) {
                            s_name = entry.key_ptr.*;
                            break;
                        }
                    }
                }

                if (s_name) |sn| {
                    if (self.type_system.structs.get(sn)) |layout| {
                        if (layout.members.get(node.name.?)) |m| {
                            const off = m.offset;
                            try self.genAddr(node.left.?);
                            if (self.arch == .arm64) {
                                try self.writer.print("    add x0, x0, #{}\n", .{off});
                            } else {
                                try self.writer.print("    addq ${}, %rax\n", .{off});
                            }
                            return;
                        }
                    }
                }
                @panic("Unknown struct member or type");
            },
            else => @panic("Cannot take address of this node type"),
        }
    }

    fn genExpr(self: *CodeGen, node: *Node) anyerror!void {
        switch (node.type) {
            .Number => {
                if (self.arch == .arm64) {
                    try self.writer.print("    mov x0, #{}\n", .{node.value.?});
                } else {
                    try self.writer.print("    movq ${}, %rax\n", .{node.value.?});
                }
            },
            .Identifier => {
                if (self.vars.get(node.name.?)) |local| {
                    try self.emitLoad(if (self.arch == .arm64) "x0" else "%rax", local.offset);
                } else if (self.globals.get(node.name.?)) |_| {
                    if (self.arch == .arm64) {
                        try self.writer.print("    adrp x8, _{s}@PAGE\n", .{node.name.?});
                        try self.writer.print("    ldr x0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                    } else {
                        try self.writer.print("    movq _{s}(%rip), %rax\n", .{node.name.?});
                    }
                } else @panic("Undefined variable");
            },
            .String => {
                const idx = self.strings.items.len;
                try self.strings.append(node.data.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    adrp x0, L_.str.{}@PAGE\n", .{idx});
                    try self.writer.print("    add x0, x0, L_.str.{}@PAGEOFF\n", .{idx});
                } else {
                    try self.writer.print("    leaq L_.str.{}(%rip), %rax\n", .{idx});
                }
            },
            .Sizeof => {
                const size = self.type_system.getTypeSize(node.data_type, false, node.struct_name);
                if (self.arch == .arm64) {
                    try self.writer.print("    mov x0, #{}\n", .{size});
                } else {
                    try self.writer.print("    movq ${}, %rax\n", .{size});
                }
            },
            .AddressOf => {
                try self.genAddr(node.right.?);
            },
            .Deref, .Index, .MemberAccess => {
                try self.genAddr(node);
                if (self.arch == .arm64) {
                    try self.writer.print("    ldr x0, [x0]\n", .{});
                } else {
                    try self.writer.print("    movq (%rax), %rax\n", .{});
                }
            },
            .Assignment => {
                if (node.name) |name| {
                    const is_global = self.globals.contains(name);
                    var offset: i32 = 0;
                    if (!is_global) {
                        const size = self.type_system.getTypeSize(node.data_type, node.is_pointer, node.struct_name);
                        const local = try self.registerVar(name, size, node.data_type, node.is_pointer, node.struct_name);
                        offset = local.offset;
                    }
                    if (node.op == null or node.op.? == .Equal) {
                        try self.genExpr(node.right.?);
                    } else {
                        if (is_global) {
                            if (self.arch == .arm64) {
                                try self.writer.print("    adrp x8, _{s}@PAGE\n    ldr x0, [x8, _{s}@PAGEOFF]\n", .{name, name});
                            } else {
                                try self.writer.print("    movq _{s}(%rip), %rax\n", .{name});
                            }
                        } else {
                            try self.emitLoad(if (self.arch == .arm64) "x0" else "%rax", offset);
                        }
                        try self.pushTemp();
                        try self.genExpr(node.right.?);
                        if (self.arch == .arm64) {
                            try self.writer.print("    mov x1, x0\n", .{});
                            try self.popTemp("x0");
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    add x0, x0, x1\n", .{}),
                                .MinusEqual => try self.writer.print("    sub x0, x0, x1\n", .{}),
                                .StarEqual => try self.writer.print("    mul x0, x0, x1\n", .{}),
                                .SlashEqual => try self.writer.print("    sdiv x0, x0, x1\n", .{}),
                                .PercentEqual => { try self.writer.print("    sdiv x2, x0, x1\n    msub x0, x2, x1, x0\n", .{}); },
                                else => unreachable,
                            }
                        } else {
                            try self.writer.print("    movq %rax, %r10\n", .{});
                            try self.popTemp("%rax");
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    addq %r10, %rax\n", .{}),
                                .MinusEqual => try self.writer.print("    subq %r10, %rax\n", .{}),
                                .StarEqual => try self.writer.print("    imulq %r10, %rax\n", .{}),
                                .SlashEqual => { try self.writer.print("    cqo\n    idivq %r10\n", .{}); },
                                .PercentEqual => { try self.writer.print("    cqo\n    idivq %r10\n    movq %rdx, %rax\n", .{}); },
                                else => unreachable,
                            }
                        }
                    }
                    if (is_global) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    adrp x8, _{s}@PAGE\n    str x0, [x8, _{s}@PAGEOFF]\n", .{name, name});
                        } else {
                            try self.writer.print("    movq %rax, _{s}(%rip)\n", .{name});
                        }
                    } else {
                        try self.emitStore(if (self.arch == .arm64) "x0" else "%rax", offset);
                    }
                } else if (node.left) |addr_node| {
                    try self.genAddr(addr_node);
                    try self.pushTemp();
                    
                    if (node.op != null and node.op.? != .Equal) {
                        if (self.arch == .arm64) {
                            try self.popTemp("x0");
                            try self.pushTemp();
                            try self.writer.print("    ldr x0, [x0]\n", .{});
                        } else {
                            try self.popTemp("%rax");
                            try self.pushTemp();
                            try self.writer.print("    movq (%rax), %rax\n", .{});
                        }
                        try self.pushTemp();
                        try self.genExpr(node.right.?);
                        
                        if (self.arch == .arm64) {
                            try self.writer.print("    mov x1, x0\n", .{});
                            try self.popTemp("x0");
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    add x0, x0, x1\n", .{}),
                                .MinusEqual => try self.writer.print("    sub x0, x0, x1\n", .{}),
                                .StarEqual => try self.writer.print("    mul x0, x0, x1\n", .{}),
                                .SlashEqual => try self.writer.print("    sdiv x0, x0, x1\n", .{}),
                                .PercentEqual => { try self.writer.print("    sdiv x2, x0, x1\n    msub x0, x2, x1, x0\n", .{}); },
                                else => unreachable,
                            }
                            try self.pushTemp();
                            try self.popTemp("x1");
                            try self.popTemp("x0");
                            try self.writer.print("    str x1, [x0]\n    mov x0, x1\n", .{});
                        } else {
                            try self.writer.print("    movq %rax, %r10\n", .{});
                            try self.popTemp("%rax");
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    addq %r10, %rax\n", .{}),
                                .MinusEqual => try self.writer.print("    subq %r10, %rax\n", .{}),
                                .StarEqual => try self.writer.print("    imulq %r10, %rax\n", .{}),
                                .SlashEqual => { try self.writer.print("    cqo\n    idivq %r10\n", .{}); },
                                .PercentEqual => { try self.writer.print("    cqo\n    idivq %r10\n    movq %rdx, %rax\n", .{}); },
                                else => unreachable,
                            }
                            try self.writer.print("    movq %rax, %r10\n", .{});
                            try self.popTemp("%rax");
                            try self.writer.print("    movq %r10, (%rax)\n    movq %r10, %rax\n", .{});
                        }
                    } else {
                        try self.genExpr(node.right.?);
                        if (self.arch == .arm64) {
                            try self.writer.print("    mov x1, x0\n", .{});
                            try self.popTemp("x0");
                            try self.writer.print("    str x1, [x0]\n    mov x0, x1\n", .{});
                        } else {
                            try self.writer.print("    movq %rax, %r10\n", .{});
                            try self.popTemp("%rax");
                            try self.writer.print("    movq %r10, (%rax)\n    movq %r10, %rax\n", .{});
                        }
                    }
                }
            },
            .FunctionCall => {
                const is_printf = std.mem.eql(u8, node.name.?, "printf");
                if (node.args) |args| {
                    for (args) |arg| {
                        try self.genExpr(arg);
                        try self.pushTemp();
                    }
                    var j: usize = args.len;
                    while (j > 0) {
                        j -= 1;
                        if (self.arch == .arm64) {
                            if (is_printf and j > 0) {
                                try self.emitLoad("x0", self.temp_stack_pos + 8);
                                try self.writer.print("    str x0, [sp, #-16]!\n", .{});
                                self.temp_stack_pos += 8;
                            } else {
                                var buf: [16]u8 = undefined;
                                const reg = try std.fmt.bufPrint(&buf, "x{}", .{j});
                                try self.popTemp(reg);
                            }
                        } else {
                            const arg_regs = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
                            try self.popTemp(arg_regs[j]);
                        }
                    }
                }
                if (self.arch == .arm64) {
                    try self.writer.print("    bl _{s}\n", .{node.name.?});
                    if (is_printf) {
                        const stack_args = if (node.args) |a| if (a.len > 1) a.len - 1 else 0 else 0;
                        if (stack_args > 0) {
                            try self.writer.print("    add sp, sp, #{}\n", .{stack_args * 16});
                        }
                    }
                } else {
                    try self.writer.print("    movb $0, %al\n", .{});
                    try self.writer.print("    callq _{s}\n", .{node.name.?});
                }
            },
            .BinaryOp => {
                try self.genExpr(node.left.?);
                try self.pushTemp();
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    mov x1, x0\n", .{});
                    try self.popTemp("x0");
                    switch (node.op.?) {
                        .Plus => try self.writer.print("    add x0, x0, x1\n", .{}),
                        .Minus => try self.writer.print("    sub x0, x0, x1\n", .{}),
                        .Star => try self.writer.print("    mul x0, x0, x1\n", .{}),
                        .Slash => try self.writer.print("    sdiv x0, x0, x1\n", .{}),
                        .Percent => { try self.writer.print("    sdiv x2, x0, x1\n    msub x0, x2, x1, x0\n", .{}); },
                        .Ampersand => try self.writer.print("    and x0, x0, x1\n", .{}),
                        .Pipe => try self.writer.print("    orr x0, x0, x1\n", .{}),
                        .Caret => try self.writer.print("    eor x0, x0, x1\n", .{}),
                        .LessLess => try self.writer.print("    lsl x0, x0, x1\n", .{}),
                        .GreaterGreater => try self.writer.print("    asr x0, x0, x1\n", .{}),
                        else => unreachable,
                    }
                } else {
                    try self.writer.print("    movq %rax, %r10\n", .{});
                    try self.popTemp("%rax");
                    switch (node.op.?) {
                        .Plus => try self.writer.print("    addq %r10, %rax\n", .{}),
                        .Minus => try self.writer.print("    subq %r10, %rax\n", .{}),
                        .Star => try self.writer.print("    imulq %r10, %rax\n", .{}),
                        .Slash => { try self.writer.print("    cqo\n    idivq %r10\n", .{}); },
                        .Percent => { try self.writer.print("    cqo\n    idivq %r10\n    movq %rdx, %rax\n", .{}); },
                        .Ampersand => try self.writer.print("    andq %r10, %rax\n", .{}),
                        .Pipe => try self.writer.print("    orq %r10, %rax\n", .{}),
                        .Caret => try self.writer.print("    xorq %r10, %rax\n", .{}),
                        .LessLess => { try self.writer.print("    movq %r10, %rcx\n    shlq %cl, %rax\n", .{}); },
                        .GreaterGreater => { try self.writer.print("    movq %r10, %rcx\n    sarq %cl, %rax\n", .{}); },
                        else => unreachable,
                    }
                }
            },
            .Comparison => {
                try self.genExpr(node.left.?);
                try self.pushTemp();
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    mov x1, x0\n", .{});
                    try self.popTemp("x0");
                    try self.writer.print("    cmp x0, x1\n", .{});
                    const label_true = self.newLabel();
                    const label_end = self.newLabel();
                    switch (node.op.?) {
                        .Greater => try self.writer.print("    b.gt {s}\n", .{label_true}),
                        .Less => try self.writer.print("    b.lt {s}\n", .{label_true}),
                        .GreaterEqual => try self.writer.print("    b.ge {s}\n", .{label_true}),
                        .LessEqual => try self.writer.print("    b.le {s}\n", .{label_true}),
                        .EqualEqual => try self.writer.print("    b.eq {s}\n", .{label_true}),
                        .NotEqual => try self.writer.print("    b.ne {s}\n", .{label_true}),
                        else => unreachable,
                    }
                    try self.writer.print("    mov x0, #0\n    b {s}\n{s}:\n    mov x0, #1\n{s}:\n", .{label_end, label_true, label_end});
                } else {
                    try self.writer.print("    movq %rax, %r10\n", .{});
                    try self.popTemp("%rax");
                    try self.writer.print("    cmpq %r10, %rax\n", .{});
                    const label_true = self.newLabel();
                    const label_end = self.newLabel();
                    switch (node.op.?) {
                        .Greater => try self.writer.print("    jg {s}\n", .{label_true}),
                        .Less => try self.writer.print("    jl {s}\n", .{label_true}),
                        .GreaterEqual => try self.writer.print("    jge {s}\n", .{label_true}),
                        .LessEqual => try self.writer.print("    jle {s}\n", .{label_true}),
                        .EqualEqual => try self.writer.print("    je {s}\n", .{label_true}),
                        .NotEqual => try self.writer.print("    jne {s}\n", .{label_true}),
                        else => unreachable,
                    }
                    try self.writer.print("    movq $0, %rax\n    jmp {s}\n{s}:\n    movq $1, %rax\n{s}:\n", .{label_end, label_true, label_end});
                }
            },
            .LogicalOp => {
                const label_end = self.newLabel();
                if (node.op.? == .AmpersandAmpersand) {
                    try self.genExpr(node.left.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{label_end});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{label_end});
                    }
                    try self.genExpr(node.right.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n    cset x0, ne\n", .{});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n    setne %al\n    movzbl %al, %eax\n", .{});
                    }
                } else {
                    try self.genExpr(node.left.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n    b.ne {s}\n", .{label_end});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n    jne {s}\n", .{label_end});
                    }
                    try self.genExpr(node.right.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n    cset x0, ne\n", .{});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n    setne %al\n    movzbl %al, %eax\n", .{});
                    }
                }
                try self.writer.print("{s}:\n", .{label_end});
            },
            .Ternary => {
                const label_else = self.newLabel();
                const label_end = self.newLabel();
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{label_else});
                } else {
                    try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{label_else});
                }
                try self.genExpr(node.left.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    b {s}\n", .{label_end});
                } else {
                    try self.writer.print("    jmp {s}\n", .{label_end});
                }
                try self.writer.print("{s}:\n", .{label_else});
                try self.genExpr(node.right.?);
                try self.writer.print("{s}:\n", .{label_end});
            },
            .TypeCast => {
                try self.genExpr(node.right.?);
                if (!node.is_pointer and node.data_type == .Char) {
                    if (self.arch == .arm64) {
                        try self.writer.print("    and x0, x0, #0xFF\n", .{});
                    } else {
                        try self.writer.print("    andq $0xFF, %rax\n", .{});
                    }
                }
            },
            .UnaryOp => {
                if (node.op.? == .PlusPlus or node.op.? == .MinusMinus) {
                    try self.genAddr(node.right.?);
                    try self.pushTemp();
                    if (self.arch == .arm64) {
                        try self.writer.print("    ldr x0, [x0]\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    add x0, x0, #1\n", .{}); } else { try self.writer.print("    sub x0, x0, #1\n", .{}); }
                        try self.pushTemp();
                        try self.popTemp("x1");
                        try self.popTemp("x0");
                        try self.writer.print("    str x1, [x0]\n    mov x0, x1\n", .{});
                    } else {
                        try self.writer.print("    movq (%rax), %rax\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    incq %rax\n", .{}); } else { try self.writer.print("    decq %rax\n", .{}); }
                        try self.pushTemp();
                        try self.popTemp("%r10");
                        try self.popTemp("%rax");
                        try self.writer.print("    movq %r10, (%rax)\n    movq %r10, %rax\n", .{});
                    }
                } else {
                    try self.genExpr(node.right.?);
                    if (node.op.? == .Minus) {
                        if (self.arch == .arm64) { try self.writer.print("    neg x0, x0\n", .{}); } else { try self.writer.print("    negq %rax\n", .{}); }
                    } else if (node.op.? == .Bang) {
                        if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    cset x0, eq\n", .{}); } else { try self.writer.print("    cmpq $0, %rax\n    sete %al\n    movzbl %al, %eax\n", .{}); }
                    } else if (node.op.? == .Tilde) {
                        if (self.arch == .arm64) { try self.writer.print("    mvn x0, x0\n", .{}); } else { try self.writer.print("    notq %rax\n", .{}); }
                    }
                }
            },
            .PostfixOp => {
                try self.genAddr(node.right.?);
                try self.pushTemp();
                if (self.arch == .arm64) {
                    try self.writer.print("    ldr x0, [x0]\n", .{});
                    try self.pushTemp();
                    if (node.op.? == .PlusPlus) { try self.writer.print("    add x1, x0, #1\n", .{}); } else { try self.writer.print("    sub x1, x0, #1\n", .{}); }
                    try self.popTemp("x0");
                    try self.popTemp("x2");
                    try self.writer.print("    str x1, [x2]\n", .{});
                } else {
                    try self.writer.print("    movq (%rax), %rax\n", .{});
                    try self.pushTemp();
                    try self.writer.print("    movq %rax, %r11\n", .{});
                    if (node.op.? == .PlusPlus) { try self.writer.print("    incq %r11\n", .{}); } else { try self.writer.print("    decq %r11\n", .{}); }
                    try self.popTemp("%rax");
                    try self.popTemp("%r10");
                    try self.writer.print("    movq %r11, (%r10)\n", .{});
                }
            },
            else => @panic("Invalid expression node"),
        }
    }

    fn genStmt(self: *CodeGen, node: *Node) anyerror!void {
        if (self.asm_comments) {
            try self.writer.print("    ; {s}\n", .{@tagName(node.type)});
        }
        switch (node.type) {
            .VarDecl => {
                const size = self.type_system.getTypeSize(node.data_type, node.is_pointer, node.struct_name);
                if (node.right) |right| {
                    try self.genExpr(right);
                    const local = try self.registerVar(node.name.?, size, node.data_type, node.is_pointer, node.struct_name);
                    try self.emitStore(if (self.arch == .arm64) "x0" else "%rax", local.offset);
                } else {
                    _ = try self.registerVar(node.name.?, size, node.data_type, node.is_pointer, node.struct_name);
                }
            },
            .ArrayDecl => {
                const element_size = self.type_system.getTypeSize(node.data_type, node.is_pointer, node.struct_name);
                _ = try self.registerVar(node.name.?, @intCast(node.value.? * element_size), node.data_type, node.is_pointer, node.struct_name);
            },
            .Assignment, .FunctionCall, .BinaryOp, .Comparison, .LogicalOp, .UnaryOp, .PostfixOp, .Number, .Identifier, .String, .Index, .MemberAccess, .Deref, .AddressOf, .Ternary => {
                try self.genExpr(node);
            },
            .Return => {
                if (node.right) |right| {
                    try self.genExpr(right);
                }
                if (self.arch == .arm64) {
                    try self.arm64_backend.emitFunctionEpilog();
                } else {
                    try self.x86_64_backend.emitFunctionEpilog();
                }
            },
            .If => {
                const else_label = self.newLabel();
                const end_label = self.newLabel();
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{else_label}); } else { try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{else_label}); }
                for (node.body.?) |stmt| { try self.genStmt(stmt); }
                try self.writer.print("{s} {s}\n{s}:\n", .{ if (self.arch == .arm64) "    b" else "    jmp", end_label, else_label });
                if (node.else_body) |else_stmts| { for (else_stmts) |stmt| { try self.genStmt(stmt); } }
                try self.writer.print("{s}:\n", .{end_label});
            },
            .While => {
                const start_label = self.newLabel();
                const end_label = self.newLabel();
                const old_break = self.break_label;
                const old_continue = self.continue_label;
                self.break_label = end_label;
                self.continue_label = start_label;
                
                try self.writer.print("{s}:\n", .{start_label});
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{end_label}); } else { try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{end_label}); }
                for (node.body.?) |stmt| { try self.genStmt(stmt); }
                try self.writer.print("{s} {s}\n{s}:\n", .{ if (self.arch == .arm64) "    b" else "    jmp", start_label, end_label });
                
                self.break_label = old_break;
                self.continue_label = old_continue;
            },
            .For => {
                const start_label = self.newLabel();
                const update_label = self.newLabel();
                const end_label = self.newLabel();
                const old_break = self.break_label;
                const old_continue = self.continue_label;
                self.break_label = end_label;
                self.continue_label = update_label;
                
                try self.genStmt(node.init.?);
                try self.writer.print("{s}:\n", .{start_label});
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{end_label}); } else { try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{end_label}); }
                for (node.body.?) |stmt| { try self.genStmt(stmt); }
                try self.writer.print("{s}:\n", .{update_label});
                try self.genExpr(node.update.?);
                try self.writer.print("{s} {s}\n{s}:\n", .{ if (self.arch == .arm64) "    b" else "    jmp", start_label, end_label });
                
                self.break_label = old_break;
                self.continue_label = old_continue;
            },
            .DoWhile => {
                const start_label = self.newLabel();
                const cond_label = self.newLabel();
                const end_label = self.newLabel();
                const old_break = self.break_label;
                const old_continue = self.continue_label;
                self.break_label = end_label;
                self.continue_label = cond_label;
                
                try self.writer.print("{s}:\n", .{start_label});
                for (node.body.?) |stmt| { try self.genStmt(stmt); }
                try self.writer.print("{s}:\n", .{cond_label});
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    b.ne {s}\n", .{start_label}); } else { try self.writer.print("    cmpq $0, %rax\n    jne {s}\n", .{start_label}); }
                try self.writer.print("{s}:\n", .{end_label});
                
                self.break_label = old_break;
                self.continue_label = old_continue;
            },
            .Break => {
                if (self.break_label) |label| {
                    if (self.arch == .arm64) { try self.writer.print("    b {s}\n", .{label}); } else { try self.writer.print("    jmp {s}\n", .{label}); }
                } else @panic("break outside of loop");
            },
            .Continue => {
                if (self.continue_label) |label| {
                    if (self.arch == .arm64) { try self.writer.print("    b {s}\n", .{label}); } else { try self.writer.print("    jmp {s}\n", .{label}); }
                } else @panic("continue outside of loop");
            },
            .Compound => {
                for (node.body.?) |stmt| {
                    try self.genStmt(stmt);
                }
            },
            .Switch => {
                const end_label = self.newLabel();
                const old_break = self.break_label;
                self.break_label = end_label;

                try self.genExpr(node.condition.?);
                try self.pushTemp();

                var default_label: ?[]const u8 = null;
                
                for (node.body.?) |stmt| {
                    if (stmt.type == .Case) {
                        const case_label = self.newLabel();
                        stmt.data = case_label;
                        if (stmt.init_value) |val| {
                            try self.popTemp(if (self.arch == .arm64) "x1" else "%r10");
                            try self.pushTemp();
                            if (self.arch == .arm64) {
                                try self.writer.print("    mov x2, #{}\n", .{val});
                                try self.writer.print("    cmp x1, x2\n", .{});
                                try self.writer.print("    b.eq {s}\n", .{case_label});
                            } else {
                                try self.writer.print("    cmpq ${}, %r10\n", .{val});
                                try self.writer.print("    je {s}\n", .{case_label});
                            }
                        } else {
                            default_label = case_label;
                        }
                    }
                }
                
                if (default_label) |dl| {
                    if (self.arch == .arm64) { try self.writer.print("    b {s}\n", .{dl}); } else { try self.writer.print("    jmp {s}\n", .{dl}); }
                } else {
                    if (self.arch == .arm64) { try self.writer.print("    b {s}\n", .{end_label}); } else { try self.writer.print("    jmp {s}\n", .{end_label}); }
                }

                self.temp_stack_pos += 8;

                for (node.body.?) |stmt| {
                    try self.genStmt(stmt);
                }

                try self.writer.print("{s}:\n", .{end_label});
                self.break_label = old_break;
            },
            .Case => {
                if (node.data) |label| {
                    try self.writer.print("{s}:\n", .{label});
                }
            },
            .StructDecl, .EnumDecl => {},
            else => @panic("Invalid statement node"),
        }
    }

    fn genFunction(self: *CodeGen, node: *Node) !void {
        self.vars.clearRetainingCapacity();
        self.stack_pos = 0;
        self.temp_stack_pos = -1024;
        if (self.asm_comments) {
            try self.writer.print("    ; function {s}\n", .{node.name.?});
        }
        try self.writer.print(".globl _{s}\n", .{node.name.?});
        if (self.arch == .arm64) {
            try self.arm64_backend.emitFunctionProlog(node.name.?);
        } else {
            try self.x86_64_backend.emitFunctionProlog(node.name.?);
        }
        if (node.params) |params| {
            for (params, 0..) |param, idx| {
                const param_type = if (node.params_types) |pts| pts[idx] else .Int;
                const is_ptr = if (node.params_is_pointer) |pips| pips[idx] else false;
                const s_name = if (node.params_struct_names) |psns| psns[idx] else null;
                const size = self.type_system.getTypeSize(param_type, is_ptr, s_name);
                const local = try self.registerVar(param, size, param_type, is_ptr, s_name);
                if (self.arch == .arm64) {
                    try self.writer.print("    str x{}, [x29, #{}]\n", .{idx, local.offset});
                } else {
                    const arg_regs = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
                    if (idx < 6) {
                        try self.writer.print("    movq {s}, {}(%rbp)\n", .{arg_regs[idx], local.offset});
                    }
                }
            }
        }
        for (node.body.?) |stmt| { try self.genStmt(stmt); }
        if (self.arch == .arm64) {
            try self.arm64_backend.emitFunctionEpilog();
        } else {
            try self.x86_64_backend.emitFunctionEpilog();
        }
    }

    fn registerGlobal(self: *CodeGen, node: *Node) !void {
        if (node.type == .VarDecl) {
            const size = self.type_system.getTypeSize(node.data_type, node.is_pointer, node.struct_name);
            try self.globals.put(node.name.?, .{ .size = size, .init_value = node.init_value, .data_type = node.data_type, .is_pointer = node.is_pointer, .struct_name = node.struct_name });
        } else if (node.type == .ArrayDecl) {
            const element_size = self.type_system.getTypeSize(node.data_type, node.is_pointer, node.struct_name);
            try self.globals.put(node.name.?, .{ .size = @intCast(node.value.? * element_size), .init_value = null, .data_type = node.data_type, .is_pointer = node.is_pointer, .struct_name = node.struct_name });
        } else if (node.type == .Compound) {
            for (node.body.?) |stmt| {
                try self.registerGlobal(stmt);
            }
        }
    }

    /// Generates code for the entire program.
    pub fn genProgram(self: *CodeGen, nodes: []*Node) !void {
        for (nodes) |node| {
            try self.registerGlobal(node);
        }
        try self.writer.print(".text\n", .{});
        for (nodes) |node| {
            if (node.type == .Function and !node.is_prototype) {
                try self.genFunction(node);
            }
        }
        if (self.strings.items.len > 0) {
            try self.writer.print(".section __TEXT,__cstring,cstring_literals\n", .{});
            for (self.strings.items, 0..) |str, i| {
                try self.writer.print("L_.str.{}: .asciz \"{s}\"\n", .{i, str});
            }
        }
        if (self.globals.count() > 0) {
            var it = self.globals.iterator();
            while (it.next()) |entry| {
                const name = entry.key_ptr.*;
                const g = entry.value_ptr.*;
                if (g.init_value) |val| {
                    try self.writer.print(".section __DATA,__data\n.globl _{s}\n.p2align 3\n_{s}:\n    .quad {}\n", .{name, name, val});
                } else {
                    try self.writer.print(".comm _{s}, {}, 3\n", .{name, g.size});
                }
            }
        }
    }
};
