const std = @import("std");
const ast = @import("ast.zig");
const common = @import("common.zig");

const Node = ast.Node;
const Arch = common.Arch;

/// CodeGen converts an AST into assembly code.
pub const CodeGen = struct {
    writer: std.fs.File.Writer,
    label_count: usize,
    vars: std.StringHashMap(i32),
    globals: std.StringHashMap(?i64),
    strings: std.ArrayList([]const u8),
    stack_pos: i32,
    temp_stack_pos: i32,
    arch: Arch,
    allocator: std.mem.Allocator,

    /// Initializes a new CodeGen with the given writer, allocator, and target architecture.
    pub fn init(writer: std.fs.File.Writer, allocator: std.mem.Allocator, arch: Arch) CodeGen {
        return CodeGen{
            .writer = writer,
            .label_count = 0,
            .vars = std.StringHashMap(i32).init(allocator),
            .globals = std.StringHashMap(?i64).init(allocator),
            .strings = std.ArrayList([]const u8).init(allocator),
            .stack_pos = 0,
            .temp_stack_pos = -128,
            .arch = arch,
            .allocator = allocator,
        };
    }

    fn pushTemp(self: *CodeGen) !void {
        if (self.arch == .arm64) {
            try self.writer.print("    str x0, [x29, #{}]\n", .{self.temp_stack_pos});
        } else {
            try self.writer.print("    movq %rax, {}(%rbp)\n", .{self.temp_stack_pos});
        }
        self.temp_stack_pos -= 8;
    }

    fn popTemp(self: *CodeGen, reg: []const u8) !void {
        self.temp_stack_pos += 8;
        if (self.arch == .arm64) {
            try self.writer.print("    ldr {s}, [x29, #{}]\n", .{reg, self.temp_stack_pos});
        } else {
            try self.writer.print("    movq {}(%rbp), {s}\n", .{self.temp_stack_pos, reg});
        }
    }

    fn newLabel(self: *CodeGen) []const u8 {
        const label = std.fmt.allocPrint(self.allocator, "L{}", .{self.label_count}) catch @panic("Allocation failed");
        self.label_count += 1;
        return label;
    }

    fn registerVar(self: *CodeGen, name: []const u8) !i32 {
        if (self.vars.get(name)) |offset| return offset;
        self.stack_pos -= 8;
        const offset = self.stack_pos;
        try self.vars.put(name, offset);
        return offset;
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
                if (self.vars.get(node.name.?)) |offset| {
                    if (self.arch == .arm64) {
                        try self.writer.print("    ldr x0, [x29, #{}]\n", .{offset});
                    } else {
                        try self.writer.print("    movq {}(%rbp), %rax\n", .{offset});
                    }
                } else if (self.globals.contains(node.name.?)) {
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
            .AddressOf => {
                if (node.right.?.type == .Identifier) {
                    const name = node.right.?.name.?;
                    if (self.vars.get(name)) |offset| {
                        if (self.arch == .arm64) {
                            try self.writer.print("    add x0, x29, #{}\n", .{offset});
                        } else {
                            try self.writer.print("    leaq {}(%rbp), %rax\n", .{offset});
                        }
                    } else if (self.globals.contains(name)) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    adrp x0, _{s}@PAGE\n", .{name});
                            try self.writer.print("    add x0, x0, _{s}@PAGEOFF\n", .{name});
                        } else {
                            try self.writer.print("    leaq _{s}(%rip), %rax\n", .{name});
                        }
                    } else @panic("Undefined variable");
                } else @panic("AddressOf only supported for identifiers");
            },
            .Deref => {
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    ldr x0, [x0]\n", .{});
                } else {
                    try self.writer.print("    movq (%rax), %rax\n", .{});
                }
            },
            .Assignment => {
                if (node.name) |name| {
                    const is_global = self.globals.contains(name);
                    const offset = if (!is_global) try self.registerVar(name) else 0;
                    if (node.op == null or node.op.? == .Equal) {
                        try self.genExpr(node.right.?);
                    } else {
                        if (is_global) {
                            if (self.arch == .arm64) {
                                try self.writer.print("    adrp x8, _{s}@PAGE\n", .{name});
                                try self.writer.print("    ldr x0, [x8, _{s}@PAGEOFF]\n", .{name});
                            } else {
                                try self.writer.print("    movq _{s}(%rip), %rax\n", .{name});
                            }
                        } else {
                            if (self.arch == .arm64) {
                                try self.writer.print("    ldr x0, [x29, #{}]\n", .{offset});
                            } else {
                                try self.writer.print("    movq {}(%rbp), %rax\n", .{offset});
                            }
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
                        if (self.arch == .arm64) {
                            try self.writer.print("    str x0, [x29, #{}]\n", .{offset});
                        } else {
                            try self.writer.print("    movq %rax, {}(%rbp)\n", .{offset});
                        }
                    }
                } else if (node.left) |addr_node| {
                    try self.genExpr(addr_node);
                    try self.pushTemp();
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
            },
            .FunctionCall => {
                if (node.args) |args| {
                    for (args) |arg| {
                        try self.genExpr(arg);
                        try self.pushTemp();
                    }
                    var j: usize = args.len;
                    while (j > 0) {
                        j -= 1;
                        if (self.arch == .arm64) {
                            var buf: [16]u8 = undefined;
                            const reg = try std.fmt.bufPrint(&buf, "x{}", .{j});
                            try self.popTemp(reg);
                        } else {
                            const arg_regs = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
                            try self.popTemp(arg_regs[j]);
                        }
                    }
                }
                if (self.arch == .arm64) {
                    try self.writer.print("    bl _{s}\n", .{node.name.?});
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
            .UnaryOp => {
                try self.genExpr(node.right.?);
                if (node.op.? == .Minus) {
                    if (self.arch == .arm64) { try self.writer.print("    neg x0, x0\n", .{}); } else { try self.writer.print("    negq %rax\n", .{}); }
                } else if (node.op.? == .Bang) {
                    if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    cset x0, eq\n", .{}); } else { try self.writer.print("    cmpq $0, %rax\n    sete %al\n    movzbl %al, %eax\n", .{}); }
                } else if (node.op.? == .Tilde) {
                    if (self.arch == .arm64) { try self.writer.print("    mvn x0, x0\n", .{}); } else { try self.writer.print("    notq %rax\n", .{}); }
                }
            },
            else => @panic("Invalid expression node"),
        }
    }

    fn genStmt(self: *CodeGen, node: *Node) anyerror!void {
        switch (node.type) {
            .VarDecl => {
                if (node.right) |right| {
                    try self.genExpr(right);
                    const offset = try self.registerVar(node.name.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    str x0, [x29, #{}]\n", .{offset});
                    } else {
                        try self.writer.print("    movq %rax, {}(%rbp)\n", .{offset});
                    }
                } else {
                    _ = try self.registerVar(node.name.?);
                }
            },
            .Assignment, .FunctionCall => {
                try self.genExpr(node);
            },
            .Return => {
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    mov sp, x29\n    ldp x29, x30, [sp], #16\n    ret\n", .{});
                } else {
                    try self.writer.print("    movq %rbp, %rsp\n    popq %rbp\n    ret\n", .{});
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
                try self.writer.print("{s}:\n", .{start_label});
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{end_label}); } else { try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{end_label}); }
                for (node.body.?) |stmt| { try self.genStmt(stmt); }
                try self.writer.print("{s} {s}\n{s}:\n", .{ if (self.arch == .arm64) "    b" else "    jmp", start_label, end_label });
            },
            .For => {
                const start_label = self.newLabel();
                const end_label = self.newLabel();
                try self.genStmt(node.init.?);
                try self.writer.print("{s}:\n", .{start_label});
                try self.genExpr(node.condition.?);
                if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{end_label}); } else { try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{end_label}); }
                for (node.body.?) |stmt| { try self.genStmt(stmt); }
                try self.genExpr(node.update.?);
                try self.writer.print("{s} {s}\n{s}:\n", .{ if (self.arch == .arm64) "    b" else "    jmp", start_label, end_label });
            },
            else => @panic("Invalid statement node"),
        }
    }

    fn genFunction(self: *CodeGen, node: *Node) !void {
        self.vars.clearRetainingCapacity();
        self.stack_pos = 0;
        self.temp_stack_pos = -128;
        try self.writer.print(".globl _{s}\n", .{node.name.?});
        if (self.arch == .arm64) {
            try self.writer.print(".p2align 2\n_{s}:\n    stp x29, x30, [sp, #-16]!\n    mov x29, sp\n    sub sp, sp, #512\n", .{node.name.?});
        } else {
            try self.writer.print(".p2align 4, 0x90\n_{s}:\n    pushq %rbp\n    movq %rsp, %rbp\n    subq $512, %rsp\n", .{node.name.?});
        }
        if (node.params) |params| {
            for (params, 0..) |param, idx| {
                const offset = try self.registerVar(param);
                if (self.arch == .arm64) {
                    try self.writer.print("    str x{}, [x29, #{}]\n", .{idx, offset});
                } else {
                    const arg_regs = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
                    try self.writer.print("    movq {s}, {}(%rbp)\n", .{arg_regs[idx], offset});
                }
            }
        }
        for (node.body.?) |stmt| { try self.genStmt(stmt); }
        if (self.arch == .arm64) {
            try self.writer.print("    mov x0, #0\n    mov sp, x29\n    ldp x29, x30, [sp], #16\n    ret\n", .{});
        } else {
            try self.writer.print("    xorq %rax, %rax\n    movq %rbp, %rsp\n    popq %rbp\n    ret\n", .{});
        }
    }

    /// Generates code for the entire program.
    pub fn genProgram(self: *CodeGen, nodes: []*Node) !void {
        for (nodes) |node| {
            if (node.type == .VarDecl) {
                try self.globals.put(node.name.?, node.init_value);
            }
        }
        try self.writer.print(".text\n", .{});
        for (nodes) |node| {
            if (node.type == .Function) {
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
                if (entry.value_ptr.*) |val| {
                    try self.writer.print(".section __DATA,__data\n", .{});
                    try self.writer.print(".globl _{s}\n", .{name});
                    try self.writer.print(".p2align 3\n", .{});
                    try self.writer.print("_{s}:\n", .{name});
                    try self.writer.print("    .quad {}\n", .{val});
                } else {
                    try self.writer.print(".comm _{s}, 8, 3\n", .{name});
                }
            }
        }
    }
};
