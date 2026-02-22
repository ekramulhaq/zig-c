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
        // Recurse into children first (bottom-up)
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
        if (node.members) |members| {
            for (members) |m| self.optimizeNode(m);
        }

        // Apply optimizations based on node type
        switch (node.type) {
            .BinaryOp, .Comparison, .LogicalOp => {
                self.foldBinaryInt(node);
                self.foldBinaryFloat(node);
                self.applyAlgebraicIdentity(node);
                self.applyStrengthReduction(node);
            },
            .UnaryOp => self.foldUnary(node),
            .If => self.eliminateDeadBranch(node),
            else => {},
        }
    }

    // ─── Integer constant folding ────────────────────────────────────────────────

    fn foldBinaryInt(self: *Optimizer, node: *Node) void {
        _ = self;
        const left = node.left orelse return;
        const right = node.right orelse return;

        // Both operands must be integer literal Number nodes
        if (left.type != .Number or right.type != .Number) return;
        if (left.fvalue != null or right.fvalue != null) return;

        const l_val = left.value orelse return;
        const r_val = right.value orelse return;
        var result: ?i64 = null;

        if (node.op) |op| {
            switch (op) {
                .Plus  => result = l_val +% r_val,
                .Minus => result = l_val -% r_val,
                .Star  => result = l_val *% r_val,
                .Slash => if (r_val != 0) { result = @divTrunc(l_val, r_val); },
                .Percent => if (r_val != 0) { result = @rem(l_val, r_val); },

                .Ampersand    => result = l_val & r_val,
                .Pipe         => result = l_val | r_val,
                .Caret        => result = l_val ^ r_val,
                .LessLess     => result = l_val << @intCast(r_val),
                .GreaterGreater => result = l_val >> @intCast(r_val),

                .EqualEqual   => result = if (l_val == r_val) 1 else 0,
                .NotEqual     => result = if (l_val != r_val) 1 else 0,
                .Greater      => result = if (l_val >  r_val) 1 else 0,
                .GreaterEqual => result = if (l_val >= r_val) 1 else 0,
                .Less         => result = if (l_val <  r_val) 1 else 0,
                .LessEqual    => result = if (l_val <= r_val) 1 else 0,

                .AmpersandAmpersand => result = if (l_val != 0 and r_val != 0) 1 else 0,
                .PipePipe           => result = if (l_val != 0 or  r_val != 0) 1 else 0,

                else => {},
            }
        }

        if (result) |res| {
            node.type   = .Number;
            node.value  = res;
            node.fvalue = null;
            node.left   = null;
            node.right  = null;
            node.op     = null;
        }
    }

    // ─── Float constant folding ──────────────────────────────────────────────────

    fn foldBinaryFloat(self: *Optimizer, node: *Node) void {
        _ = self;
        const left  = node.left  orelse return;
        const right = node.right orelse return;
        if (left.type != .Number or right.type != .Number) return;

        // At least one must be a float literal; the other may be int (implicit cast)
        const l_f: ?f64 = if (left.fvalue)  |f| f else if (left.value)  |v| @floatFromInt(v) else null;
        const r_f: ?f64 = if (right.fvalue) |f| f else if (right.value) |v| @floatFromInt(v) else null;
        if (l_f == null or r_f == null) return;
        // Only fold if at least one side is actually a float literal
        if (left.fvalue == null and right.fvalue == null) return;

        const lv = l_f.?;
        const rv = r_f.?;
        var result: ?f64 = null;

        if (node.op) |op| {
            switch (op) {
                .Plus  => result = lv + rv,
                .Minus => result = lv - rv,
                .Star  => result = lv * rv,
                .Slash => if (rv != 0.0) { result = lv / rv; },
                else => {},
            }
        }

        if (result) |res| {
            node.type      = .Number;
            node.fvalue    = res;
            node.value     = null;
            node.data_type = .Double;
            node.left      = null;
            node.right     = null;
            node.op        = null;
        }
    }

    // ─── Algebraic identity simplification ──────────────────────────────────────
    // x + 0 → x,   x - 0 → x,   x * 1 → x,   x / 1 → x,
    // x * 0 → 0,   0 * x → 0,   x & 0 → 0,   x | 0 → x,
    // x ^ 0 → x,   x - x → 0  (only when both sides are identical identifiers)

    fn applyAlgebraicIdentity(self: *Optimizer, node: *Node) void {
        if (node.type != .BinaryOp) return;
        const left  = node.left  orelse return;
        const right = node.right orelse return;
        const op    = node.op    orelse return;

        // Helper: is this an integer Number node with given value?
        const rightIsInt = (right.type == .Number and right.fvalue == null);
        const leftIsInt  = (left.type  == .Number and left.fvalue  == null);
        const rval: i64  = if (rightIsInt) (right.value orelse -999) else -999;
        const lval: i64  = if (leftIsInt)  (left.value  orelse -999) else -999;

        switch (op) {
            .Plus => {
                // x + 0 → x
                if (rightIsInt and rval == 0) { self.replaceWith(node, left); return; }
                // 0 + x → x
                if (leftIsInt  and lval == 0) { self.replaceWith(node, right); return; }
            },
            .Minus => {
                // x - 0 → x
                if (rightIsInt and rval == 0) { self.replaceWith(node, left); return; }
                // x - x → 0  (only for simple identifiers with same name)
                if (left.type == .Identifier and right.type == .Identifier) {
                    if (left.name != null and right.name != null) {
                        if (std.mem.eql(u8, left.name.?, right.name.?)) {
                            node.type  = .Number;
                            node.value = 0;
                            node.left  = null;
                            node.right = null;
                            node.op    = null;
                            return;
                        }
                    }
                }
            },
            .Star => {
                // x * 0 → 0 or 0 * x → 0
                if ((rightIsInt and rval == 0) or (leftIsInt and lval == 0)) {
                    node.type  = .Number;
                    node.value = 0;
                    node.left  = null;
                    node.right = null;
                    node.op    = null;
                    return;
                }
                // x * 1 → x
                if (rightIsInt and rval == 1) { self.replaceWith(node, left); return; }
                // 1 * x → x
                if (leftIsInt  and lval == 1) { self.replaceWith(node, right); return; }
            },
            .Slash => {
                // x / 1 → x
                if (rightIsInt and rval == 1) { self.replaceWith(node, left); return; }
            },
            .Pipe => {
                // x | 0 → x
                if (rightIsInt and rval == 0) { self.replaceWith(node, left); return; }
                if (leftIsInt  and lval == 0) { self.replaceWith(node, right); return; }
            },
            .Ampersand => {
                // x & 0 → 0
                if ((rightIsInt and rval == 0) or (leftIsInt and lval == 0)) {
                    node.type  = .Number;
                    node.value = 0;
                    node.left  = null;
                    node.right = null;
                    node.op    = null;
                    return;
                }
            },
            .Caret => {
                // x ^ 0 → x
                if (rightIsInt and rval == 0) { self.replaceWith(node, left); return; }
                if (leftIsInt  and lval == 0) { self.replaceWith(node, right); return; }
            },
            else => {},
        }
    }

    // Copy the content of `src` into `dst` in-place so pointers held by the
    // caller remain valid.
    fn replaceWith(_: *Optimizer, dst: *Node, src: *Node) void {
        dst.* = src.*;
    }

    // ─── Strength reduction ──────────────────────────────────────────────────────
    // Replace multiply-by-power-of-2 with a left shift:
    //   x * 2  → x << 1,   x * 4  → x << 2,  etc.
    // Replace divide-by-power-of-2 with a right shift (arithmetic, non-negative only).

    fn applyStrengthReduction(_: *Optimizer, node: *Node) void {
        if (node.type != .BinaryOp) return;
        const right = node.right orelse return;
        const op    = node.op    orelse return;

        if (right.type != .Number or right.fvalue != null) return;
        const rval = right.value orelse return;

        switch (op) {
            .Star => {
                const shift = isPowerOfTwo(rval) orelse return;
                if (shift == 0) return; // * 1 already handled by identity pass
                right.value = shift;
                node.op     = .LessLess;
            },
            .Slash => {
                const shift = isPowerOfTwo(rval) orelse return;
                if (shift == 0) return; // / 1 already handled
                right.value = shift;
                node.op     = .GreaterGreater;
            },
            else => {},
        }
    }

    fn isPowerOfTwo(n: i64) ?i64 {
        if (n <= 1) return null;
        if (n & (n - 1) != 0) return null;
        // count trailing zeros = log2
        var shift: i64 = 0;
        var v = n;
        while (v > 1) : (v >>= 1) shift += 1;
        return shift;
    }

    // ─── Unary constant folding ──────────────────────────────────────────────────

    fn foldUnary(self: *Optimizer, node: *Node) void {
        const right = node.right orelse return;
        const op    = node.op    orelse return;

        // Float unary minus
        if (right.type == .Number and right.fvalue != null) {
            if (op == .Minus) {
                right.fvalue = -(right.fvalue.?);
                self.replaceWith(node, right);
            }
            return;
        }

        if (right.type != .Number) return;
        const val = right.value orelse return;
        var result: ?i64 = null;

        switch (op) {
            .Minus => result = -%val,
            .Tilde => result = ~val,
            .Bang  => result = if (val == 0) 1 else 0,
            else   => {},
        }

        if (result) |res| {
            node.type   = .Number;
            node.value  = res;
            node.fvalue = null;
            node.right  = null;
            node.op     = null;
        }
    }

    // ─── Dead branch elimination ─────────────────────────────────────────────────
    // If the condition of an `if` has been folded to a Number literal, we can
    // statically select the live branch and turn the If node into a Compound.

    fn eliminateDeadBranch(self: *Optimizer, node: *Node) void {
        _ = self;
        if (node.type != .If) return;
        const cond = node.condition orelse return;
        if (cond.type != .Number) return;

        const taken = (cond.value orelse 0) != 0 or (cond.fvalue orelse 0.0) != 0.0;

        if (taken) {
            // Replace If with a Compound of the then-body
            node.type      = .Compound;
            node.condition = null;
            // body stays; else_body becomes unreachable
            node.else_body = null;
        } else {
            // Replace If with a Compound of the else-body (or empty compound)
            node.type      = .Compound;
            node.condition = null;
            node.body      = node.else_body;
            node.else_body = null;
        }
    }
};
