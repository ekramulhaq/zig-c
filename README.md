# Simple Compiler in Zig

This project implements a simple compiler for a C-like language, written in Zig.
It targets x86-64 assembly (AT&T syntax) compatible with macOS.

## Building the Compiler

Run the following command to build and run the compiler:

```bash
zig build run
```

This will generate an assembly file named `out.asm`.

## Assembling and Running the Output

To run the generated assembly on macOS (including Apple Silicon via Rosetta 2), use `clang` with the `-arch x86_64` flag:

```bash
# Rename the file to .s so clang recognizes it as assembly source
mv out.asm out.s

# Assemble and link (targeting x86_64)
clang -arch x86_64 -o out out.s

# Run the executable
./out

# Check the exit code (should be 42)
echo "Exit Code: $?"
```

## Language Features

Supported features:
- Variables (`int x = 0;`)
- Arithmetic (`+, -, *, /`)
- Comparisons (`>, <`)
- Control Flow (`if/else`, `while`, `for`)
- Return statement (`return x;`)
