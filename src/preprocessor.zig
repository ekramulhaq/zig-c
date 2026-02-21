const std = @import("std");

pub const Preprocessor = struct {
    allocator: std.mem.Allocator,
    include_dirs: std.ArrayList([]const u8),
    included_files: std.StringHashMap(void),
    macros: std.StringHashMap([]const u8),
    conditional_stack: std.ArrayList(bool),
    arena: std.heap.ArenaAllocator,

    pub fn create(allocator: std.mem.Allocator) !*Preprocessor {
        const self = try allocator.create(Preprocessor);
        self.arena = std.heap.ArenaAllocator.init(allocator);
        const arena_allocator = self.arena.allocator();
        self.allocator = allocator;
        self.include_dirs = std.ArrayList([]const u8).init(arena_allocator);
        self.included_files = std.StringHashMap(void).init(arena_allocator);
        self.macros = std.StringHashMap([]const u8).init(arena_allocator);
        self.conditional_stack = std.ArrayList(bool).init(arena_allocator);
        
        // Default include directory relative to the project root
        try self.include_dirs.append("include");
        
        return self;
    }

    pub fn deinit(self: *Preprocessor) void {
        const alloc = self.allocator;
        self.arena.deinit();
        alloc.destroy(self);
    }

    pub fn addIncludeDir(self: *Preprocessor, dir: []const u8) !void {
        try self.include_dirs.append(try self.arena.allocator().dupe(u8, dir));
    }

    pub fn preprocessFile(self: *Preprocessor, path: []const u8) anyerror![]const u8 {
        const absolute_path = std.fs.cwd().realpathAlloc(self.arena.allocator(), path) catch |err| {
            // If it doesn't exist, it might be in an include dir
            return err;
        };

        if (self.included_files.contains(absolute_path)) {
            return ""; // Already included
        }
        try self.included_files.put(absolute_path, {});

        const file_content = try std.fs.cwd().readFileAlloc(self.arena.allocator(), path, 1024 * 1024);
        const current_dir = std.fs.path.dirname(path) orelse ".";
        return self.preprocessSource(file_content, current_dir);
    }

    pub fn preprocessSource(self: *Preprocessor, source: []const u8, current_dir: []const u8) anyerror![]const u8 {
        var result = std.ArrayList(u8).init(self.arena.allocator());
        var line_it = std.mem.splitScalar(u8, source, '\n');

        while (line_it.next()) |line| {
            const trimmed = std.mem.trimLeft(u8, line, " \t\r");

            // Handle directives
            if (std.mem.startsWith(u8, trimmed, "#")) {
                if (try self.handleDirective(trimmed, current_dir, &result)) continue;
            }

            // Skip lines if we are in a false conditional block
            if (!self.shouldInclude()) continue;

            // Expand macros for normal lines
            const expanded = try self.expandMacros(line);
            try result.appendSlice(expanded);
            try result.append('\n');
        }

        return result.toOwnedSlice();
    }

    fn shouldInclude(self: *Preprocessor) bool {
        for (self.conditional_stack.items) |condition| {
            if (!condition) return false;
        }
        return true;
    }

    fn handleDirective(self: *Preprocessor, line: []const u8, current_dir: []const u8, result: *std.ArrayList(u8)) !bool {
        // Directives that are processed even when skipping (nested conditionals)
        if (std.mem.startsWith(u8, line, "#ifdef") or std.mem.startsWith(u8, line, "#ifndef")) {
             // We need to process ifdef/ifndef to track nesting level even if we are skipping
            if (!self.shouldInclude()) {
                try self.conditional_stack.append(false); // Valid skipping, so we push false to keep skipping
                return true;
            }
            
            const is_ifndef = std.mem.startsWith(u8, line, "#ifndef");
            const prefix_len = if (is_ifndef) "#ifndef".len else "#ifdef".len;
            const macro_name = std.mem.trim(u8, line[prefix_len..], " \t\r");
            
            if (macro_name.len == 0) return true; // Malformed

            const exists = self.macros.contains(macro_name);
            const condition = if (is_ifndef) !exists else exists;
            try self.conditional_stack.append(condition);
            return true;
        }

        if (std.mem.startsWith(u8, line, "#endif")) {
            if (self.conditional_stack.items.len > 0) {
                _ = self.conditional_stack.pop();
            }
            return true;
        }

        if (std.mem.startsWith(u8, line, "#else")) {
            if (self.conditional_stack.pop()) |last| {
                // Simplified else logic: invert the last condition.
                try self.conditional_stack.append(!last);
            }
            return true;
        }

        // Skip other directives if we are in a false block
        if (!self.shouldInclude()) return true;

        if (std.mem.startsWith(u8, line, "#include")) {
            const rest = std.mem.trim(u8, line["#include".len..], " \t\r");
            if (rest.len < 3) return true;

            var file_name: []const u8 = undefined;
            var is_system = false;

            if (rest[0] == '"' and rest[rest.len - 1] == '"') {
                file_name = rest[1 .. rest.len - 1];
                is_system = false;
            } else if (rest[0] == '<' and rest[rest.len - 1] == '>') {
                file_name = rest[1 .. rest.len - 1];
                is_system = true;
            } else {
                return true;
            }

            const include_content = try self.resolveInclude(file_name, is_system, current_dir);
            try result.appendSlice(include_content);
            try result.append('\n');
            return true;
        }

        if (std.mem.startsWith(u8, line, "#define")) {
            const rest = std.mem.trim(u8, line["#define".len..], " \t\r");
            var it = std.mem.tokenizeScalar(u8, rest, ' ');
            const name = it.next();
            if (name) |n| {
                // Determine value (rest of the line)
                const value_start_index = std.mem.indexOf(u8, rest, n).? + n.len;
                const value = std.mem.trim(u8, rest[value_start_index..], " \t\r");
                try self.defineMacro(n, value);
            }
            return true;
        }

        if (std.mem.startsWith(u8, line, "#undef")) {
            const name = std.mem.trim(u8, line["#undef".len..], " \t\r");
            _ = self.macros.remove(name);
            return true;
        }

        return false; // Not a directive we handled
    }

    fn defineMacro(self: *Preprocessor, name: []const u8, value: []const u8) !void {
        try self.macros.put(try self.arena.allocator().dupe(u8, name), try self.arena.allocator().dupe(u8, value));
    }

    fn expandMacros(self: *Preprocessor, line: []const u8) ![]const u8 {
        var result = std.ArrayList(u8).init(self.arena.allocator());
        var i: usize = 0;
        const len = line.len;

        while (i < len) {
            const c = line[i];
            if (isIdentifierStart(c)) {
                const start = i;
                while (i < len and isIdentifierChar(line[i])) : (i += 1) {}
                const word = line[start..i];
                if (self.macros.get(word)) |replacement| {
                     try result.appendSlice(replacement);
                } else {
                    try result.appendSlice(word);
                }
            } else {
                try result.append(c);
                i += 1;
            }
        }
        return result.toOwnedSlice();
    }

    fn resolveInclude(self: *Preprocessor, file_name: []const u8, is_system: bool, current_dir: []const u8) ![]const u8 {
        // ... (rest of function)
        // 1. Try local directory if not system include
        if (!is_system) {
            const local_path = try std.fs.path.join(self.arena.allocator(), &[_][]const u8{ current_dir, file_name });
            if (std.fs.cwd().access(local_path, .{})) |_| {
                return self.preprocessFile(local_path);
            } else |_| {}
        }

        // 2. Try include directories
        for (self.include_dirs.items) |dir| {
            const system_path = try std.fs.path.join(self.arena.allocator(), &[_][]const u8{ dir, file_name });
            if (std.fs.cwd().access(system_path, .{})) |_| {
                return self.preprocessFile(system_path);
            } else |_| {}
        }

        std.debug.print("Preprocessor Error: Could not find include file '{s}'\n", .{file_name});
        return error.FileNotFound;
    }
};

fn isIdentifierStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentifierChar(c: u8) bool {
    return isIdentifierStart(c) or (c >= '0' and c <= '9');
}
