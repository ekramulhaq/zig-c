const std = @import("std");

pub const Preprocessor = struct {
    allocator: std.mem.Allocator,
    include_dirs: std.ArrayList([]const u8),
    included_files: std.StringHashMap(void),
    arena: std.heap.ArenaAllocator,

    pub fn create(allocator: std.mem.Allocator) !*Preprocessor {
        const self = try allocator.create(Preprocessor);
        self.arena = std.heap.ArenaAllocator.init(allocator);
        const arena_allocator = self.arena.allocator();
        self.allocator = allocator;
        self.include_dirs = std.ArrayList([]const u8).init(arena_allocator);
        self.included_files = std.StringHashMap(void).init(arena_allocator);
        
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
            if (std.mem.startsWith(u8, trimmed, "#include")) {
                const rest = std.mem.trim(u8, trimmed["#include".len..], " \t\r");
                if (rest.len < 3) continue;

                var file_name: []const u8 = undefined;
                var is_system = false;

                if (rest[0] == '"' and rest[rest.len - 1] == '"') {
                    file_name = rest[1 .. rest.len - 1];
                    is_system = false;
                } else if (rest[0] == '<' and rest[rest.len - 1] == '>') {
                    file_name = rest[1 .. rest.len - 1];
                    is_system = true;
                } else {
                    continue;
                }

                const include_content = try self.resolveInclude(file_name, is_system, current_dir);
                try result.appendSlice(include_content);
                try result.append('\n');
            } else {
                try result.appendSlice(line);
                try result.append('\n');
            }
        }

        return result.toOwnedSlice();
    }

    fn resolveInclude(self: *Preprocessor, file_name: []const u8, is_system: bool, current_dir: []const u8) ![]const u8 {
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
