const std = @import("std");
const ast = @import("ast.zig");
const token_pkg = @import("token.zig");

const Node = ast.Node;
const NodeType = ast.NodeType;
const TokenType = token_pkg.TokenType;

pub const Optimizer = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Optimizer {
        return Optimizer{ .allocator = allocator };
    }

    pub fn optimize(self: *Optimizer, nodes: []*Node) void {
        for (nodes) |node| {
            self.optimizeNode(node);
        }
    }

    fn optimizeNode(self: *Optimizer, node: *Node) void {
        // Recursive optimization
        if (node.left) |left| self.optimizeNode(left);
        if (node.right) |right| self.optimizeNode(right);
        if (node.condition) |cond| self.optimizeNode(cond);
        if (node.init) |init_node| self.optimizeNode(init_node);
        if (node.update) |update| self.optimizeNode(update);
        
        if (node.body) |body| {
            for (body) |stmt| self.optimizeNode(stmt);
        }
        if (node.else_body) |else_body| {
            for (else_body) |stmt| self.optimizeNode(stmt);
        }
        if (node.args) |args| {
            for (args) |arg| self.optimizeNode(arg);
        }

        // Apply optimizations based on node type
        switch (node.type) {
            .BinaryOp, .Comparison, .LogicalOp => self.foldBinary(node),
            .UnaryOp => self.foldUnary(node),
            else => {},
        }
    }

    fn foldBinary(self: *Optimizer, node: *Node) void {
        _ = self;
        const left = node.left orelse return;
        const right = node.right orelse return;

        if (left.type == .Number and right.type == .Number) {
            const l_val = left.value.?;
            const r_val = right.value.?;
            var result: ?i64 = null;

            if (node.op) |op| {
                switch (op) {
                    .Plus => result = l_val +% r_val,
                    .Minus => result = l_val -% r_val,
                    .Star => result = l_val *% r_val,
                    .Slash => if (r_val != 0) { result = @divTrunc(l_val, r_val); },
                    .Percent => if (r_val != 0) { result = @rem(l_val, r_val); },
                    
                    // Bitwise
                    .Ampersand => result = l_val & r_val,
                    .Pipe => result = l_val | r_val,
                    .Caret => result = l_val ^ r_val,
                    .LessLess => result = l_val << @intCast(r_val),
                    .GreaterGreater => result = l_val >> @intCast(r_val),

                    // Comparison
                    .EqualEqual => result = if (l_val == r_val) 1 else 0,
                    .NotEqual => result = if (l_val != r_val) 1 else 0,
                    .Greater => result = if (l_val > r_val) 1 else 0,
                    .GreaterEqual => result = if (l_val >= r_val) 1 else 0,
                    .Less => result = if (l_val < r_val) 1 else 0,
                    .LessEqual => result = if (l_val <= r_val) 1 else 0,

                    // Logical (integers as booleans)
                    .AmpersandAmpersand => result = if (l_val != 0 and r_val != 0) 1 else 0,
                    .PipePipe => result = if (l_val != 0 or r_val != 0) 1 else 0,

                    else => {},
                }
            }

            if (result) |res| {
                // Transform this node into a Number node
                node.type = .Number;
                node.value = res;
                node.left = null;
                node.right = null;
                node.op = null;
            }
        }
    }

    fn foldUnary(self: *Optimizer, node: *Node) void {
        _ = self;
        const right = node.right orelse return;

        if (right.type == .Number) {
            const val = right.value.?;
            var result: ?i64 = null;

            if (node.op) |op| {
                switch (op) {
                    .Minus => result = -%val,
                    .Tilde => result = ~val,
                    .Bang => result = if (val == 0) 1 else 0,
                    else => {},
                }
            }

            if (result) |res| {
                node.type = .Number;
                node.value = res;
                node.right = null;
                node.op = null;
            }
        }
    }
};
