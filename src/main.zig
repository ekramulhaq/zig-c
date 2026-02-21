const std = @import("std");
const builtin = @import("builtin");
const common = @import("common.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const codegen = @import("codegen.zig");
const optimizer = @import("optimizer.zig");

const Arch = common.Arch;
const Lexer = lexer.Lexer;
const Token = lexer.Token;
const Parser = parser.Parser;
const CodeGen = codegen.CodeGen;
const Optimizer = optimizer.Optimizer;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var arch: Arch = if (builtin.cpu.arch == .aarch64) .arm64 else .x86_64;
    var input_file: ?[]const u8 = null;

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
        } else {
            input_file = args[i];
        }
    }

    const source = if (input_file) |path| try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024) else "int main() { return 42; }";
    var lex = Lexer.init(source);
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    while (true) {
        const token = lex.nextToken();
        try tokens.append(token);
        if (token.type == .EOF) break;
    }

    var par = Parser.init(try tokens.toOwnedSlice(), allocator);
    const ast = try par.parseProgram();

    var opt = Optimizer.init(allocator);
    opt.optimize(ast);

    const file = try std.fs.cwd().createFile("out.asm", .{});
    defer file.close();
    var cg = CodeGen.init(file.writer(), allocator, arch);
    try cg.genProgram(ast);
}
