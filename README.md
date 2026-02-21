# Simple Compiler in Zig

A multi-pass, multi-architecture compiler for a subset of the C language, implemented in Zig. This project targets macOS on both Intel (x86_64) and Apple Silicon (ARM64).

## Features

- **Language Constructs**: Integers (`int`), Characters (`char`), `void` functions, Structs, Enums, and `typedef`.
- **Control Flow**: `if/else`, `while`, `for`, `do-while`, `break`, and `continue`.
- **Operators**: Full suite of arithmetic, bitwise, comparison, and logical operators.
- **Memory**: Support for pointers (`&`, `*`), arrays, and global variables.
- **Architectures**: Code generation for ARM64 and x86_64 (macOS).
- **Optimization**: Constant folding at the AST level.

## Getting Started

### Prerequisites

- **Zig**: Version 0.13.0 or 0.14.0 (for best compatibility).
- **macOS**: Target system for assembly and linking.

### Building the Compiler

Run the following command to build the project:

```bash
zig build
```

The compiled binary will be located in `zig-out/bin/compiler`.

### Compiling and Running a Program

To compile a `.simple` source file (a subset of C) into an executable:

```bash
# Compile to ARM64 assembly (default)
./zig-out/bin/compiler --arch arm64 examples/arithmetic.simple

# Assemble and link the generated out.asm
mv out.asm out.s
clang -arch arm64 -o my_app out.s

# Run the app
./my_app
echo "Exit code: $?"
```

## Running Tests

To run the automated test suite, which verifies 26 test cases across both supported architectures:

```bash
./test.sh
```

## Documentation

For a detailed language specification and usage guide, see [DOCUMENTATION.md](DOCUMENTATION.md).

For a deep dive into the system architecture and internal design, see [ARCHITECTURE.md](ARCHITECTURE.md).

## License

MIT
