const std = @import("std");
const ast = @import("ast.zig");
const common = @import("common.zig");

const Node = ast.Node;
const Arch = common.Arch;

const Global = struct {
    size: i32,
    init_value: ?i64,
};

/// CodeGen converts an AST into assembly code.
pub const CodeGen = struct {
    writer: std.fs.File.Writer,
    label_count: usize,
    vars: std.StringHashMap(i32),
    globals: std.StringHashMap(Global),
    struct_layouts: std.StringHashMap(std.StringHashMap(i32)), // struct name -> member name -> offset
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
            .globals = std.StringHashMap(Global).init(allocator),
            .struct_layouts = std.StringHashMap(std.StringHashMap(i32)).init(allocator),
            .strings = std.ArrayList([]const u8).init(allocator),
            .stack_pos = 0,
            .temp_stack_pos = -1024, // Temps start at -1024 to avoid overlap with locals
            .arch = arch,
            .allocator = allocator,
        };
    }

    fn pushTemp(self: *CodeGen) !void {
        if (self.arch == .arm64) {
            try self.emitStore("x0", self.temp_stack_pos);
        } else {
            try self.writer.print("    movq %rax, {}(%rbp)\n", .{self.temp_stack_pos});
        }
        self.temp_stack_pos -= 8;
    }

    fn popTemp(self: *CodeGen, reg: []const u8) !void {
        self.temp_stack_pos += 8;
        if (self.arch == .arm64) {
            try self.emitLoad(reg, self.temp_stack_pos);
        } else {
            try self.writer.print("    movq {}(%rbp), {s}\n", .{self.temp_stack_pos, reg});
        }
    }

    fn emitLoad(self: *CodeGen, reg: []const u8, offset: i32) !void {
        if (self.arch == .arm64) {
            if (offset >= -256 and offset <= 255) {
                try self.writer.print("    ldr {s}, [x29, #{}]\n", .{reg, offset});
            } else {
                try self.writer.print("    mov x9, #{}\n", .{offset});
                try self.writer.print("    ldr {s}, [x29, x9]\n", .{reg});
            }
        } else {
            try self.writer.print("    movq {}(%rbp), {s}\n", .{offset, reg});
        }
    }

    fn emitStore(self: *CodeGen, reg: []const u8, offset: i32) !void {
        if (self.arch == .arm64) {
            if (offset >= -256 and offset <= 255) {
                try self.writer.print("    str {s}, [x29, #{}]\n", .{reg, offset});
            } else {
                try self.writer.print("    mov x9, #{}\n", .{offset});
                try self.writer.print("    str {s}, [x29, x9]\n", .{reg});
            }
        } else {
            try self.writer.print("    movq {s}, {}(%rbp)\n", .{reg, offset});
        }
    }

    fn newLabel(self: *CodeGen) []const u8 {
        const label = std.fmt.allocPrint(self.allocator, "L{}", .{self.label_count}) catch @panic("Allocation failed");
        self.label_count += 1;
        return label;
    }

    fn registerVar(self: *CodeGen, name: []const u8, size: i32) !i32 {
        if (self.vars.get(name)) |offset| return offset;
        self.stack_pos -= size;
        const offset = self.stack_pos;
        try self.vars.put(name, offset);
        return offset;
    }

    fn genAddr(self: *CodeGen, node: *Node) anyerror!void {
        switch (node.type) {
            .Identifier => {
                if (self.vars.get(node.name.?)) |offset| {
                    if (self.arch == .arm64) {
                        if (offset >= 0) {
                            try self.writer.print("    add x0, x29, #{}\n", .{offset});
                        } else {
                            try self.writer.print("    sub x0, x29, #{}\n", .{-offset});
                        }
                    } else {
                        try self.writer.print("    leaq {}(%rbp), %rax\n", .{offset});
                    }
                } else if (self.globals.get(node.name.?)) |_| {
                    if (self.arch == .arm64) {
                        try self.writer.print("    adrp x0, _{s}@PAGE\n", .{node.name.?});
                        try self.writer.print("    add x0, x0, _{s}@PAGEOFF\n", .{node.name.?});
                    } else {
                        try self.writer.print("    leaq _{s}(%rip), %rax\n", .{node.name.?});
                    }
                } else @panic("Undefined variable");
            },
            .Deref => {
                try self.genExpr(node.right.?);
            },
            .Index => {
                try self.genAddr(node.left.?);
                try self.pushTemp(); // Base address
                try self.genExpr(node.right.?); // Index
                if (self.arch == .arm64) {
                    try self.writer.print("    lsl x1, x0, #3\n", .{}); // index * 8
                    try self.popTemp("x0"); // base address
                    try self.writer.print("    add x0, x0, x1\n", .{});
                } else {
                    try self.writer.print("    shlq $3, %rax\n", .{}); // index * 8
                    try self.writer.print("    movq %rax, %r10\n", .{});
                    try self.popTemp("%rax"); // base address
                    try self.writer.print("    addq %r10, %rax\n", .{});
                }
            },
            .MemberAccess => {
                var it = self.struct_layouts.valueIterator();
                var offset: ?i32 = null;
                while (it.next()) |members| {
                    if (members.get(node.name.?)) |off| {
                        offset = off;
                        break;
                    }
                }
                
                if (offset) |off| {
                    try self.genAddr(node.left.?);
                    if (self.arch == .arm64) {
                        try self.writer.print("    add x0, x0, #{}\n", .{off});
                    } else {
                        try self.writer.print("    addq ${}, %rax\n", .{off});
                    }
                } else @panic("Unknown struct member");
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
                if (self.vars.get(node.name.?)) |offset| {
                    try self.emitLoad(if (self.arch == .arm64) "x0" else "%rax", offset);
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
                    const offset = if (!is_global) try self.registerVar(name, 8) else 0;
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
                    // Deref, Index, MemberAccess
                    try self.genAddr(addr_node);
                    try self.pushTemp(); // Target address
                    
                    if (node.op != null and node.op.? != .Equal) {
                        // Compound assignment to address: evaluate current value
                        if (self.arch == .arm64) {
                            try self.popTemp("x0");
                            try self.pushTemp(); // save it back
                            try self.writer.print("    ldr x0, [x0]\n", .{});
                        } else {
                            try self.popTemp("%rax");
                            try self.pushTemp();
                            try self.writer.print("    movq (%rax), %rax\n", .{});
                        }
                        try self.pushTemp(); // Current value
                        try self.genExpr(node.right.?); // RHS
                        
                        if (self.arch == .arm64) {
                            try self.writer.print("    mov x1, x0\n", .{});
                            try self.popTemp("x0"); // Current value
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    add x0, x0, x1\n", .{}),
                                .MinusEqual => try self.writer.print("    sub x0, x0, x1\n", .{}),
                                .StarEqual => try self.writer.print("    mul x0, x0, x1\n", .{}),
                                .SlashEqual => try self.writer.print("    sdiv x0, x0, x1\n", .{}),
                                .PercentEqual => { try self.writer.print("    sdiv x2, x0, x1\n    msub x0, x2, x1, x0\n", .{}); },
                                else => unreachable,
                            }
                            try self.pushTemp(); // result
                            try self.popTemp("x1"); // result
                            try self.popTemp("x0"); // address
                            try self.writer.print("    str x1, [x0]\n    mov x0, x1\n", .{});
                        } else {
                            try self.writer.print("    movq %rax, %r10\n", .{}); // RHS in r10
                            try self.popTemp("%rax"); // current value in rax
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    addq %r10, %rax\n", .{}),
                                .MinusEqual => try self.writer.print("    subq %r10, %rax\n", .{}),
                                .StarEqual => try self.writer.print("    imulq %r10, %rax\n", .{}),
                                .SlashEqual => { try self.writer.print("    cqo\n    idivq %r10\n", .{}); },
                                .PercentEqual => { try self.writer.print("    cqo\n    idivq %r10\n    movq %rdx, %rax\n", .{}); },
                                else => unreachable,
                            }
                            try self.writer.print("    movq %rax, %r10\n", .{}); // result in r10
                            try self.popTemp("%rax"); // address in rax
                            try self.writer.print("    movq %r10, (%rax)\n    movq %r10, %rax\n", .{});
                        }
                    } else {
                        // Simple assignment: *addr = RHS
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
                                // For printf, variadic args after the first go to the stack
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
                        // Clean up variadic args from stack
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
            .UnaryOp => {
                if (node.op.? == .PlusPlus or node.op.? == .MinusMinus) {
                    try self.genAddr(node.right.?);
                    try self.pushTemp();
                    if (self.arch == .arm64) {
                        try self.writer.print("    ldr x0, [x0]\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    add x0, x0, #1\n", .{}); } else { try self.writer.print("    sub x0, x0, #1\n", .{}); }
                        try self.pushTemp();
                        try self.popTemp("x1"); // new value
                        try self.popTemp("x0"); // address
                        try self.writer.print("    str x1, [x0]\n    mov x0, x1\n", .{});
                    } else {
                        try self.writer.print("    movq (%rax), %rax\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    incq %rax\n", .{}); } else { try self.writer.print("    decq %rax\n", .{}); }
                        try self.pushTemp();
                        try self.popTemp("%r10"); // new value
                        try self.popTemp("%rax"); // address
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
            else => @panic("Invalid expression node"),
        }
    }

    fn genStmt(self: *CodeGen, node: *Node) anyerror!void {
        switch (node.type) {
            .VarDecl => {
                if (node.right) |right| {
                    try self.genExpr(right);
                    const offset = try self.registerVar(node.name.?, 8);
                    try self.emitStore(if (self.arch == .arm64) "x0" else "%rax", offset);
                } else {
                    _ = try self.registerVar(node.name.?, 8);
                }
            },
            .ArrayDecl => {
                _ = try self.registerVar(node.name.?, @intCast(node.value.? * 8));
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
            .StructDecl => {},
            else => @panic("Invalid statement node"),
        }
    }

    fn genFunction(self: *CodeGen, node: *Node) !void {
        self.vars.clearRetainingCapacity();
        self.stack_pos = 0;
        self.temp_stack_pos = -1024;
        try self.writer.print(".globl _{s}\n", .{node.name.?});
        if (self.arch == .arm64) {
            try self.writer.print(".p2align 2\n_{s}:\n    stp x29, x30, [sp, #-16]!\n    mov x29, sp\n    sub sp, sp, #2048\n", .{node.name.?});
        } else {
            try self.writer.print(".p2align 4, 0x90\n_{s}:\n    pushq %rbp\n    movq %rsp, %rbp\n    subq $2048, %rsp\n", .{node.name.?});
        }
        if (node.params) |params| {
            for (params, 0..) |param, idx| {
                const offset = try self.registerVar(param, 8);
                if (self.arch == .arm64) {
                    try self.writer.print("    str x{}, [x29, #{}]\n", .{idx, offset});
                } else {
                    const arg_regs = [_][]const u8{ "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
                    if (idx < 6) {
                        try self.writer.print("    movq {s}, {}(%rbp)\n", .{arg_regs[idx], offset});
                    }
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
                try self.globals.put(node.name.?, .{ .size = 8, .init_value = node.init_value });
            } else if (node.type == .ArrayDecl) {
                try self.globals.put(node.name.?, .{ .size = @intCast(node.value.? * 8), .init_value = null });
            } else if (node.type == .StructDecl) {
                var layout = std.StringHashMap(i32).init(self.allocator);
                var offset: i32 = 0;
                for (node.members.?) |member| {
                    if (member.type == .VarDecl) {
                        try layout.put(member.name.?, offset);
                        offset += 8;
                    }
                }
                try self.struct_layouts.put(node.name.?, layout);
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
