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
    floats: std.ArrayList(f64),
    has_floats: bool = false,
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
            .floats = std.ArrayList(f64).init(allocator),
            .has_floats = false,
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

    fn pushTemp(self: *CodeGen, data_type: ast.DataType) !void {
        try self.emitStore(self.getReg(data_type, 0), self.temp_stack_pos);
        self.temp_stack_pos -= 8;
    }

    fn popTemp(self: *CodeGen, reg: []const u8) !void {
        self.temp_stack_pos += 8;
        try self.emitLoad(reg, self.temp_stack_pos);
    }

    fn getReg(self: *CodeGen, data_type: ast.DataType, idx: usize) []const u8 {
        if (data_type.isFloat()) {
            if (self.arch == .arm64) {
                const regs = [_][]const u8{ "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7" };
                return regs[idx];
            } else {
                const regs = [_][]const u8{ "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7" };
                return regs[idx];
            }
        } else {
            if (self.arch == .arm64) {
                const regs = [_][]const u8{ "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7" };
                return regs[idx];
            } else {
                const regs = [_][]const u8{ "%rax", "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
                if (idx == 0) return "%rax";
                return regs[idx];
            }
        }
    }

    fn newLabel(self: *CodeGen) []const u8 {
        const label = std.fmt.allocPrint(self.allocator, "L{}", .{self.label_count}) catch @panic("Allocation failed");
        self.label_count += 1;
        return label;
    }

    /// Resolve the actual DataType for an identifier node, looking up in vars then globals.
    fn resolveIdType(self: *CodeGen, node: *ast.Node) ast.DataType {
        if (node.type == .Identifier) {
            if (node.name) |name| {
                if (self.vars.get(name)) |local| return local.data_type;
                if (self.globals.get(name)) |global| return global.data_type;
            }
        }
        return node.data_type;
    }

    fn resolveTypeInfo(self: *CodeGen, node: *ast.Node) struct { dt: ast.DataType, is_ptr: bool, s_name: ?[]const u8 } {
        if (node.type == .Identifier) {
            if (self.vars.get(node.name.?)) |local| {
                return .{ .dt = local.data_type, .is_ptr = local.is_pointer, .s_name = local.struct_name };
            } else if (self.globals.get(node.name.?)) |global| {
                return .{ .dt = global.data_type, .is_ptr = global.is_pointer, .s_name = global.struct_name };
            }
        } else if (node.type == .MemberAccess) {
            const info = self.resolveTypeInfo(node.left.?);
            var s_name = info.s_name;
            if (info.is_ptr) { // e.g., root->next where root is Node*
                // Already handled by parsePostfix converting -> to Deref
            }
            if (node.left.?.type == .Deref) {
                const inner = node.left.?.right.?;
                const inner_info = self.resolveTypeInfo(inner);
                s_name = inner_info.s_name;
            }
            if (s_name) |sn| {
                if (self.type_system.getMember(sn, node.name.?)) |m| {
                    return .{ .dt = m.data_type, .is_ptr = m.is_pointer, .s_name = m.struct_name };
                }
            }
        } else if (node.type == .Deref) {
             const info = self.resolveTypeInfo(node.right.?);
             return .{ .dt = info.dt, .is_ptr = false, .s_name = info.s_name };
        } else if (node.type == .Index) {
            const info = self.resolveTypeInfo(node.left.?);
            return .{ .dt = info.dt, .is_ptr = false, .s_name = info.s_name };
        }
        return .{ .dt = .Int, .is_ptr = false, .s_name = null };
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
                const info = self.resolveTypeInfo(node.left.?);
                const elem_size = self.type_system.getTypeSize(info.dt, info.is_ptr, info.s_name);

                if (info.is_ptr) {
                    try self.genExpr(node.left.?);
                } else {
                    try self.genAddr(node.left.?);
                }
                try self.pushTemp(.Int);
                try self.genExpr(node.right.?);
                if (self.arch == .arm64) {
                    try self.writer.print("    mov x1, #{}\n", .{elem_size});
                    try self.writer.print("    mul x1, x0, x1\n", .{});
                    try self.popTemp("x0");
                    try self.writer.print("    add x0, x0, x1\n", .{});
                } else {
                    try self.writer.print("    imulq ${}, %rax, %rax\n", .{elem_size});
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
                if (node.fvalue) |fval| {
                    self.has_floats = true;
                    const idx = self.floats.items.len;
                    try self.floats.append(fval);
                    if (self.arch == .arm64) {
                        try self.writer.print("    adrp x8, L_.float.{}@PAGE\n", .{idx});
                        try self.writer.print("    ldr d0, [x8, L_.float.{}@PAGEOFF]\n", .{idx});
                    } else {
                        try self.writer.print("    movsd L_.float.{}(%rip), %xmm0\n", .{idx});
                    }
                    node.data_type = .Double;
                } else {
                    if (self.arch == .arm64) {
                        try self.writer.print("    mov x0, #{}\n", .{node.value.?});
                    } else {
                        try self.writer.print("    movq ${}, %rax\n", .{node.value.?});
                    }
                    node.data_type = .Int;
                }
            },
            .Identifier => {
                if (self.vars.get(node.name.?)) |local| {
                    node.data_type = local.data_type;
                    if (node.data_type.isFloat()) self.has_floats = true;
                    try self.emitLoad(self.getReg(local.data_type, 0), local.offset);
                } else if (self.globals.get(node.name.?)) |global| {
                    node.data_type = global.data_type;
                    if (node.data_type.isFloat()) self.has_floats = true;
                    if (self.arch == .arm64) {
                        try self.writer.print("    adrp x8, _{s}@PAGE\n", .{node.name.?});
                        if (node.data_type.isFloat()) {
                            try self.writer.print("    ldr d0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                        } else {
                            try self.writer.print("    ldr x0, [x8, _{s}@PAGEOFF]\n", .{node.name.?});
                        }
                    } else {
                        if (node.data_type.isFloat()) {
                            try self.writer.print("    movsd _{s}(%rip), %xmm0\n", .{node.name.?});
                        } else {
                            try self.writer.print("    movq _{s}(%rip), %rax\n", .{node.name.?});
                        }
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
                    var data_type: ast.DataType = .Int;
                    if (is_global) {
                        data_type = self.globals.get(name).?.data_type;
                    } else {
                        if (self.vars.get(name)) |local| {
                            offset = local.offset;
                            data_type = local.data_type;
                        } else {
                            const size = self.type_system.getTypeSize(node.data_type, node.is_pointer, node.struct_name);
                            const local = try self.registerVar(name, size, node.data_type, node.is_pointer, node.struct_name);
                            offset = local.offset;
                            data_type = local.data_type;
                        }
                    }
                    node.data_type = data_type;
                    if (data_type.isFloat()) self.has_floats = true;

                    if (node.op != .Equal) {
                        // Compound assignment: load current value first
                        if (is_global) {
                            if (self.arch == .arm64) {
                                try self.writer.print("    adrp x8, _{s}@PAGE\n", .{name});
                                if (data_type.isFloat()) {
                                    try self.writer.print("    ldr d0, [x8, _{s}@PAGEOFF]\n", .{name});
                                } else {
                                    try self.writer.print("    ldr x0, [x8, _{s}@PAGEOFF]\n", .{name});
                                }
                            } else {
                                if (data_type.isFloat()) {
                                    try self.writer.print("    movsd _{s}(%rip), %xmm0\n", .{name});
                                } else {
                                    try self.writer.print("    movq _{s}(%rip), %rax\n", .{name});
                                }
                            }
                        } else {
                            try self.emitLoad(self.getReg(data_type, 0), offset);
                        }
                        try self.pushTemp(data_type);
                        try self.genExpr(node.right.?);
                        if (data_type.isFloat()) {
                            if (self.arch == .arm64) {
                                try self.writer.print("    fmov d1, d0\n", .{});
                                try self.popTemp("d0");
                                switch (node.op.?) {
                                    .PlusEqual => try self.writer.print("    fadd d0, d0, d1\n", .{}),
                                    .MinusEqual => try self.writer.print("    fsub d0, d0, d1\n", .{}),
                                    .StarEqual => try self.writer.print("    fmul d0, d0, d1\n", .{}),
                                    .SlashEqual => try self.writer.print("    fdiv d0, d0, d1\n", .{}),
                                    else => unreachable,
                                }
                            } else {
                                try self.writer.print("    movsd %xmm0, %xmm1\n", .{});
                                try self.popTemp("%xmm0");
                                switch (node.op.?) {
                                    .PlusEqual => try self.writer.print("    addsd %xmm1, %xmm0\n", .{}),
                                    .MinusEqual => try self.writer.print("    subsd %xmm1, %xmm0\n", .{}),
                                    .StarEqual => try self.writer.print("    mulsd %xmm1, %xmm0\n", .{}),
                                    .SlashEqual => try self.writer.print("    divsd %xmm1, %xmm0\n", .{}),
                                    else => unreachable,
                                }
                            }
                        } else {
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
                    } else {
                        try self.genExpr(node.right.?);
                    }

                    if (is_global) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    adrp x8, _{s}@PAGE\n", .{name});
                            if (data_type.isFloat()) {
                                try self.writer.print("    str d0, [x8, _{s}@PAGEOFF]\n", .{name});
                            } else {
                                try self.writer.print("    str x0, [x8, _{s}@PAGEOFF]\n", .{name});
                            }
                        } else {
                            if (data_type.isFloat()) {
                                try self.writer.print("    movsd %xmm0, _{s}(%rip)\n", .{name});
                            } else {
                                try self.writer.print("    movq %rax, _{s}(%rip)\n", .{name});
                            }
                        }
                    } else {
                        try self.emitStore(self.getReg(data_type, 0), offset);
                    }
                } else if (node.left) |addr_node| {
                    try self.genAddr(addr_node);
                    try self.pushTemp(.Int); // Save address

                    if (node.op != .Equal) {
                        // Compound assignment: load current value from address
                        if (self.arch == .arm64) {
                            try self.emitLoad("x0", self.temp_stack_pos + 8);
                            try self.writer.print("    ldr x0, [x0]\n", .{});
                        } else {
                            try self.emitLoad("%rax", self.temp_stack_pos + 8);
                            try self.writer.print("    movq (%rax), %rax\n", .{});
                        }
                        try self.pushTemp(.Int); // Save old value

                        try self.genExpr(node.right.?);
                        if (self.arch == .arm64) {
                            try self.writer.print("    mov x1, x0\n", .{});
                            try self.popTemp("x0"); // Old value
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    add x0, x0, x1\n", .{}),
                                .MinusEqual => try self.writer.print("    sub x0, x0, x1\n", .{}),
                                .StarEqual => try self.writer.print("    mul x0, x0, x1\n", .{}),
                                .SlashEqual => try self.writer.print("    sdiv x0, x0, x1\n", .{}),
                                .PercentEqual => { try self.writer.print("    sdiv x2, x0, x1\n    msub x0, x2, x1, x0\n", .{}); },
                                else => unreachable,
                            }
                            try self.popTemp("x1"); // Address
                            try self.writer.print("    str x0, [x1]\n", .{});
                        } else {
                            try self.writer.print("    movq %rax, %r10\n", .{});
                            try self.popTemp("%rax"); // Old value
                            switch (node.op.?) {
                                .PlusEqual => try self.writer.print("    addq %r10, %rax\n", .{}),
                                .MinusEqual => try self.writer.print("    subq %r10, %rax\n", .{}),
                                .StarEqual => try self.writer.print("    imulq %r10, %rax\n", .{}),
                                .SlashEqual => { try self.writer.print("    cqo\n    idivq %r10\n", .{}); },
                                .PercentEqual => { try self.writer.print("    cqo\n    idivq %r10\n    movq %rdx, %rax\n", .{}); },
                                else => unreachable,
                            }
                            try self.popTemp("%r10"); // Address
                            try self.writer.print("    movq %rax, (%r10)\n", .{});
                        }
                    } else {
                        try self.genExpr(node.right.?);
                        const dt = node.right.?.data_type;
                        if (self.arch == .arm64) {
                            try self.popTemp("x1");
                            if (dt.isFloat()) {
                                try self.writer.print("    str d0, [x1]\n", .{});
                            } else {
                                try self.writer.print("    str x0, [x1]\n", .{});
                            }
                        } else {
                            try self.popTemp("%r10");
                            if (dt.isFloat()) {
                                try self.writer.print("    movsd %xmm0, (%r10)\n", .{});
                            } else {
                                try self.writer.print("    movq %rax, (%r10)\n", .{});
                            }
                        }
                    }
                }
            },
            .FunctionCall => {
                const is_printf = std.mem.eql(u8, node.name.?, "printf");
                if (node.args) |args| {
                    for (args) |arg| {
                        try self.genExpr(arg);
                        try self.pushTemp(arg.data_type);
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
                const dt = node.left.?.data_type;
                try self.pushTemp(dt);
                try self.genExpr(node.right.?);
                node.data_type = dt; // Simplified type propagation
                if (dt.isFloat()) self.has_floats = true;

                if (dt.isFloat()) {
                    if (self.arch == .arm64) {
                        try self.writer.print("    fmov d1, d0\n", .{});
                        try self.popTemp("d0");
                        switch (node.op.?) {
                            .Plus => try self.writer.print("    fadd d0, d0, d1\n", .{}),
                            .Minus => try self.writer.print("    fsub d0, d0, d1\n", .{}),
                            .Star => try self.writer.print("    fmul d0, d0, d1\n", .{}),
                            .Slash => try self.writer.print("    fdiv d0, d0, d1\n", .{}),
                            else => unreachable,
                        }
                    } else {
                        try self.writer.print("    movsd %xmm0, %xmm1\n", .{});
                        try self.popTemp("%xmm0");
                        switch (node.op.?) {
                            .Plus => try self.writer.print("    addsd %xmm1, %xmm0\n", .{}),
                            .Minus => try self.writer.print("    subsd %xmm1, %xmm0\n", .{}),
                            .Star => try self.writer.print("    mulsd %xmm1, %xmm0\n", .{}),
                            .Slash => try self.writer.print("    divsd %xmm1, %xmm0\n", .{}),
                            else => unreachable,
                        }
                    }
                } else {
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
                }
            },
            .Comparison => {
                try self.genExpr(node.left.?);
                const dt = node.left.?.data_type;
                try self.pushTemp(dt);
                try self.genExpr(node.right.?);
                node.data_type = .Int;

                if (dt.isFloat()) {
                    if (self.arch == .arm64) {
                        try self.writer.print("    fmov d1, d0\n", .{});
                        try self.popTemp("d0");
                        try self.writer.print("    fcmp d0, d1\n", .{});
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
                        try self.writer.print("    movsd %xmm0, %xmm1\n", .{});
                        try self.popTemp("%xmm0");
                        try self.writer.print("    ucomisd %xmm1, %xmm0\n", .{});
                        const label_true = self.newLabel();
                        const label_end = self.newLabel();
                        switch (node.op.?) {
                            .Greater => try self.writer.print("    ja {s}\n", .{label_true}),
                            .Less => try self.writer.print("    jb {s}\n", .{label_true}),
                            .GreaterEqual => try self.writer.print("    jae {s}\n", .{label_true}),
                            .LessEqual => try self.writer.print("    jbe {s}\n", .{label_true}),
                            .EqualEqual => try self.writer.print("    je {s}\n", .{label_true}),
                            .NotEqual => try self.writer.print("    jne {s}\n", .{label_true}),
                            else => unreachable,
                        }
                        try self.writer.print("    movq $0, %rax\n    jmp {s}\n{s}:\n    movq $1, %rax\n{s}:\n", .{label_end, label_true, label_end});
                    }
                } else {
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
                }
            },
            .LogicalOp => {
                const label_end = self.newLabel();
                if (node.op.? == .AmpersandAmpersand) {
                    try self.genExpr(node.left.?);
                    if (node.left.?.data_type.isFloat()) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    fcmp d0, #0.0\n    b.eq {s}\n", .{label_end});
                        } else {
                            try self.writer.print("    xorpd %xmm1, %xmm1\n    ucomisd %xmm1, %xmm0\n    je {s}\n", .{label_end});
                        }
                    } else {
                        if (self.arch == .arm64) {
                            try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{label_end});
                        } else {
                            try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{label_end});
                        }
                    }
                    try self.genExpr(node.right.?);
                    if (node.right.?.data_type.isFloat()) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    fcmp d0, #0.0\n    cset x0, ne\n", .{});
                        } else {
                            try self.writer.print("    xorpd %xmm1, %xmm1\n    ucomisd %xmm1, %xmm0\n    setne %al\n    movzbl %al, %eax\n", .{});
                        }
                    } else {
                        if (self.arch == .arm64) {
                            try self.writer.print("    cmp x0, #0\n    cset x0, ne\n", .{});
                        } else {
                            try self.writer.print("    cmpq $0, %rax\n    setne %al\n    movzbl %al, %eax\n", .{});
                        }
                    }
                } else {
                    try self.genExpr(node.left.?);
                    if (node.left.?.data_type.isFloat()) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    fcmp d0, #0.0\n    b.ne {s}\n", .{label_end});
                        } else {
                            try self.writer.print("    xorpd %xmm1, %xmm1\n    ucomisd %xmm1, %xmm0\n    jne {s}\n", .{label_end});
                        }
                    } else {
                        if (self.arch == .arm64) {
                            try self.writer.print("    cmp x0, #0\n    b.ne {s}\n", .{label_end});
                        } else {
                            try self.writer.print("    cmpq $0, %rax\n    jne {s}\n", .{label_end});
                        }
                    }
                    try self.genExpr(node.right.?);
                    if (node.right.?.data_type.isFloat()) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    fcmp d0, #0.0\n    cset x0, ne\n", .{});
                        } else {
                            try self.writer.print("    xorpd %xmm1, %xmm1\n    ucomisd %xmm1, %xmm0\n    setne %al\n    movzbl %al, %eax\n", .{});
                        }
                    } else {
                        if (self.arch == .arm64) {
                            try self.writer.print("    cmp x0, #0\n    cset x0, ne\n", .{});
                        } else {
                            try self.writer.print("    cmpq $0, %rax\n    setne %al\n    movzbl %al, %eax\n", .{});
                        }
                    }
                }
                try self.writer.print("{s}:\n", .{label_end});
                node.data_type = .Int;
            },
            .Ternary => {
                const label_else = self.newLabel();
                const label_end = self.newLabel();
                try self.genExpr(node.condition.?);
                if (node.condition.?.data_type.isFloat()) {
                    if (self.arch == .arm64) {
                        try self.writer.print("    fcmp d0, #0.0\n    b.eq {s}\n", .{label_else});
                    } else {
                        try self.writer.print("    xorpd %xmm1, %xmm1\n    ucomisd %xmm1, %xmm0\n    je {s}\n", .{label_else});
                    }
                } else {
                    if (self.arch == .arm64) {
                        try self.writer.print("    cmp x0, #0\n    b.eq {s}\n", .{label_else});
                    } else {
                        try self.writer.print("    cmpq $0, %rax\n    je {s}\n", .{label_else});
                    }
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
                node.data_type = node.left.?.data_type;
            },
            .TypeCast => {
                try self.genExpr(node.right.?);
                const src_dt = node.right.?.data_type;
                const dst_dt = node.data_type;
                if (dst_dt.isFloat()) self.has_floats = true;
                if (!node.is_pointer) {
                    if (src_dt == .Int and dst_dt.isFloat()) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    scvtf d0, x0\n", .{});
                        } else {
                            try self.writer.print("    cvtsi2sdq %rax, %xmm0\n", .{});
                        }
                    } else if (src_dt.isFloat() and dst_dt == .Int) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    fcvtzs x0, d0\n", .{});
                        } else {
                            try self.writer.print("    cvttsd2siq %xmm0, %rax\n", .{});
                        }
                    } else if (!node.is_pointer and dst_dt == .Char) {
                        if (self.arch == .arm64) {
                            try self.writer.print("    and x0, x0, #0xFF\n", .{});
                        } else {
                            try self.writer.print("    andq $0xFF, %rax\n", .{});
                        }
                    }
                }
            },
            .UnaryOp => {
                if (node.op.? == .PlusPlus or node.op.? == .MinusMinus) {
                    const unary_dt = self.resolveIdType(node.right.?);
                    try self.genAddr(node.right.?);
                    try self.pushTemp(.Int);
                    if (self.arch == .arm64) {
                        try self.popTemp("x1");
                        if (unary_dt.isFloat()) {
                            self.has_floats = true;
                            try self.writer.print("    ldr d0, [x1]\n", .{});
                            try self.writer.print("    adrp x8, L_.float.constant_1@PAGE\n    ldr d1, [x8, L_.float.constant_1@PAGEOFF]\n", .{});
                            if (node.op.? == .PlusPlus) { try self.writer.print("    fadd d0, d0, d1\n", .{}); } else { try self.writer.print("    fsub d0, d0, d1\n", .{}); }
                            try self.writer.print("    str d0, [x1]\n", .{});
                        } else {
                            try self.writer.print("    ldr x0, [x1]\n", .{});
                            if (node.op.? == .PlusPlus) { try self.writer.print("    add x0, x0, #1\n", .{}); } else { try self.writer.print("    sub x0, x0, #1\n", .{}); }
                            try self.writer.print("    str x0, [x1]\n", .{});
                        }
                    } else {
                        try self.popTemp("%r10");
                        if (unary_dt.isFloat()) {
                            self.has_floats = true;
                            try self.writer.print("    movsd (%r10), %xmm0\n", .{});
                            try self.writer.print("    movsd L_.float.constant_1(%rip), %xmm1\n", .{});
                            if (node.op.? == .PlusPlus) { try self.writer.print("    addsd %xmm1, %xmm0\n", .{}); } else { try self.writer.print("    subsd %xmm1, %xmm0\n", .{}); }
                            try self.writer.print("    movsd %xmm0, (%r10)\n", .{});
                        } else {
                            try self.writer.print("    movq (%r10), %rax\n", .{});
                            if (node.op.? == .PlusPlus) { try self.writer.print("    incq %rax\n", .{}); } else { try self.writer.print("    decq %rax\n", .{}); }
                            try self.writer.print("    movq %rax, (%r10)\n", .{});
                        }
                    }
                } else {
                    try self.genExpr(node.right.?);
                    if (node.right.?.data_type.isFloat()) {
                        self.has_floats = true;
                        if (node.op.? == .Minus) {
                            if (self.arch == .arm64) { try self.writer.print("    fneg d0, d0\n", .{}); } else { 
                                try self.writer.print("    movsd L_.float.neg_zero(%rip), %xmm1\n    xorpd %xmm1, %xmm0\n", .{}); 
                            }
                        }
                    } else {
                        if (node.op.? == .Minus) {
                            if (self.arch == .arm64) { try self.writer.print("    neg x0, x0\n", .{}); } else { try self.writer.print("    negq %rax\n", .{}); }
                        } else if (node.op.? == .Bang) {
                            if (self.arch == .arm64) { try self.writer.print("    cmp x0, #0\n    cset x0, eq\n", .{}); } else { try self.writer.print("    cmpq $0, %rax\n    sete %al\n    movzbl %al, %eax\n", .{}); }
                        } else if (node.op.? == .Tilde) {
                            if (self.arch == .arm64) { try self.writer.print("    mvn x0, x0\n", .{}); } else { try self.writer.print("    notq %rax\n", .{}); }
                        }
                    }
                }
            },
            .PostfixOp => {
                const postfix_dt = self.resolveIdType(node.right.?);
                try self.genAddr(node.right.?);
                try self.pushTemp(.Int);
                if (self.arch == .arm64) {
                    try self.popTemp("x1");
                    if (postfix_dt.isFloat()) {
                        self.has_floats = true;
                        try self.writer.print("    ldr d0, [x1]\n", .{});
                        try self.writer.print("    fmov d2, d0\n", .{});
                        try self.writer.print("    adrp x8, L_.float.constant_1@PAGE\n    ldr d1, [x8, L_.float.constant_1@PAGEOFF]\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    fadd d2, d2, d1\n", .{}); } else { try self.writer.print("    fsub d2, d2, d1\n", .{}); }
                        try self.writer.print("    str d2, [x1]\n", .{});
                    } else {
                        try self.writer.print("    ldr x0, [x1]\n", .{});
                        try self.writer.print("    mov x2, x0\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    add x2, x2, #1\n", .{}); } else { try self.writer.print("    sub x2, x2, #1\n", .{}); }
                        try self.writer.print("    str x2, [x1]\n", .{});
                    }
                } else {
                    try self.popTemp("%r10");
                    if (postfix_dt.isFloat()) {
                        self.has_floats = true;
                        try self.writer.print("    movsd (%r10), %xmm0\n", .{});
                        try self.writer.print("    movsd %xmm0, %xmm2\n", .{});
                        try self.writer.print("    movsd L_.float.constant_1(%rip), %xmm1\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    addsd %xmm1, %xmm2\n", .{}); } else { try self.writer.print("    subsd %xmm1, %xmm2\n", .{}); }
                        try self.writer.print("    movsd %xmm2, (%r10)\n", .{});
                    } else {
                        try self.writer.print("    movq (%r10), %rax\n", .{});
                        try self.writer.print("    movq %rax, %r11\n", .{});
                        if (node.op.? == .PlusPlus) { try self.writer.print("    incq %r11\n", .{}); } else { try self.writer.print("    decq %r11\n", .{}); }
                        try self.writer.print("    movq %r11, (%r10)\n", .{});
                    }
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
                    try self.emitStore(self.getReg(node.data_type, 0), local.offset);
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
                try self.pushTemp(node.condition.?.data_type);

                var default_label: ?[]const u8 = null;
                
                for (node.body.?) |stmt| {
                    if (stmt.type == .Case) {
                        const case_label = self.newLabel();
                        stmt.data = case_label;
                        if (stmt.init_value) |val| {
                            try self.popTemp(if (self.arch == .arm64) "x1" else "%r10");
                            try self.pushTemp(node.condition.?.data_type);
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
        if (self.floats.items.len > 0 or self.has_floats) {
            try self.writer.print(".section __TEXT,__literal8,8byte_literals\n.p2align 3\n", .{});
            for (self.floats.items, 0..) |fval, i| {
                const bits = @as(u64, @bitCast(fval));
                try self.writer.print("L_.float.{}: .quad {}\n", .{i, bits});
            }
            if (self.has_floats) {
                const one: f64 = 1.0;
                const one_bits = @as(u64, @bitCast(one));
                try self.writer.print("L_.float.constant_1: .quad {}\n", .{one_bits});
                
                const neg_zero: f64 = -0.0;
                const neg_zero_bits = @as(u64, @bitCast(neg_zero));
                try self.writer.print("L_.float.neg_zero: .quad {}\n", .{neg_zero_bits});
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
