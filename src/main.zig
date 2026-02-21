const std = @import("std");
const builtin = @import("builtin");
const common = @import("common.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const codegen = @import("codegen.zig");
const optimizer = @import("optimizer.zig");
const preprocessor = @import("preprocessor.zig");

const Arch = common.Arch;
const Lexer = lexer.Lexer;
const Token = lexer.Token;
const Parser = parser.Parser;
const CodeGen = codegen.CodeGen;
const Optimizer = optimizer.Optimizer;
const Preprocessor = preprocessor.Preprocessor;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var arch: Arch = if (builtin.cpu.arch == .aarch64) .arm64 else .x86_64;
    var input_file: ?[]const u8 = null;
    var dump_tokens = false;
    var dump_ast = false;
    var asm_comments = false;
    var include_dirs = std.ArrayList([]const u8).init(allocator);
    defer include_dirs.deinit();

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--arch")) {
            i += 1;
            if (i < args.len) {
                if (std.mem.eql(u8, args[i], "x86_64")) {
                    arch = .x86_64;
                } else if (std.mem.eql(u8, args[i], "arm64")) {
                    arch = .arm64;
                } else {
                    std.debug.print("Unknown architecture: {s}\n", .{args[i]});
                    std.process.exit(1);
                }
            }
        } else if (std.mem.eql(u8, args[i], "-I")) {
            i += 1;
            if (i < args.len) {
                try include_dirs.append(args[i]);
            }
        } else if (std.mem.eql(u8, args[i], "--dump-tokens")) {
            dump_tokens = true;
        } else if (std.mem.eql(u8, args[i], "--dump-ast")) {
            dump_ast = true;
        } else if (std.mem.eql(u8, args[i], "--asm-comments")) {
            asm_comments = true;
        } else {
            input_file = args[i];
        }
    }

    var prep = try Preprocessor.create(allocator);
    defer prep.deinit();
    for (include_dirs.items) |dir| {
        try prep.addIncludeDir(dir);
    }

    const source = if (input_file) |path| prep.preprocessFile(path) catch |err| {
        std.debug.print("Preprocessing Error: {}\n", .{err});
        std.process.exit(1);
    } else try prep.preprocessSource("int main() { return 42; }", ".");

    var lex = Lexer.init(source);
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    while (true) {
        const token = lex.nextToken() catch |err| {
            if (err == error.InvalidCharacter) {
                std.debug.print("Lexer Error: Invalid character at line {}, col {}\n", .{ lex.line, lex.col });
                std.process.exit(1);
            }
            return err;
        };
        try tokens.append(token);
        if (token.type == .EOF) break;
    }

    if (dump_tokens) {
        std.debug.print("--- Tokens ---\n", .{});
        for (tokens.items) |token| {
            std.debug.print("{s: <20} | {s: <10} | line {}, col {}\n", .{ @tagName(token.type), token.value, token.line, token.col });
        }
        std.debug.print("\n", .{});
    }

    var par = try Parser.create(try tokens.toOwnedSlice(), allocator);
    defer par.deinit();
    const ast_nodes = par.parseProgram() catch |err| {
        if (err == error.ParseError or err == error.UnexpectedToken or err == error.UnexpectedEOF) {
            // Error message already printed by parser
            std.process.exit(1);
        }
        return err;
    };

    var opt = Optimizer.init(allocator);
    opt.optimize(ast_nodes);

    if (dump_ast) {
        std.debug.print("--- AST ---\n", .{});
        for (ast_nodes) |node| {
            node.dump(0);
        }
        std.debug.print("\n", .{});
    }

    const file = try std.fs.cwd().createFile("out.asm", .{});
    defer file.close();
    var cg = CodeGen.init(file.writer(), allocator, arch, par.type_system);
    cg.asm_comments = asm_comments;
    cg.genProgram(ast_nodes) catch |err| {
        std.debug.print("CodeGen Error: {}\n", .{err});
        std.process.exit(1);
    };
}
