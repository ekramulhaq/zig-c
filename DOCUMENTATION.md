# Simple Compiler Documentation

This document provides a comprehensive guide to the Simple Compiler, its features, and how to use it.

## Table of Contents
1.  [Overview](#overview)
2.  [Language Specification](#language-specification)
    - [Data Types](#data-types)
    - [Control Flow](#control-flow)
    - [Operators](#operators)
    - [Functions](#functions)
    - [Memory and Structs](#memory-and-structs)
3.  [Architectural Design](#architectural-design)
4.  [Supported Architectures](#supported-architectures)
5.  [Testing and Examples](#testing-and-examples)
6.  [Usage Guide](#usage-guide)

---

## Overview
The Simple Compiler is a hand-written compiler for a subset of the C language, implemented in Zig. It targets macOS (both ARM64 and x86_64 architectures) and produces assembly code compatible with the system assembler (`clang`). It supports multiple file inclusion via the `#include` directive and provides a basic standard library.

---

## Language Specification

### Data Types
- **`int`**: 64-bit signed integer.
- **`char`**: 8-bit character (currently stored as 64-bit for stack alignment).
- **`void`**: Return type for functions that do not return a value.
- **`struct`**: Custom data structures with named members.
- **`enum`**: Enumerated types with constant members.
- **`typedef`**: Create aliases for existing types.

#### Literals
- **Numeric**: Decimal integers (e.g., `42`), hexadecimal (e.g., `0xFF`, `0x1A`), and octal (e.g., `077`, `010`).
- **Character**: Single-quoted characters (e.g., `'A'`). Supports escape sequences: `\n`, `\t`, `\r`, `\\`, `\'`, `\0`.
- **String**: Double-quoted strings (e.g., `"Hello, world!"`). Supports escape sequences: `\n`, `\t`, `\r`, `\\`, `\"`, `\0`, `\a`, `\b`, `\f`.

### Control Flow
- **`if` / `else`**: Conditional execution.
- **`while`**: Condition-controlled loop.
- **`for`**: Iteration-controlled loop.
- **`do-while`**: Post-condition loop.
- **`switch` / `case` / `default`**: Multi-way branching with fallthrough support.
- **`break`**: Immediate exit from the innermost loop or switch.
- **`continue`**: Skip the rest of the current loop iteration.
- **`return`**: Exit function with an optional value.

### Operators
- **Arithmetic**: `+, -, *, /, %` (prefix and postfix `++` and `--`).
- **Bitwise**: `&, |, ^, ~, <<, >>`.
- **Comparison**: `==, !=, >, <, >=, <=`.
- **Logical**: `&&, ||, !`.
- **Ternary**: `cond ? then : else`.
- **Unary**: `sizeof(type)`, `&` (address-of), `*` (dereference).
- **Type Cast**: `(type)expression` (e.g., `(char)x`, `(void *)ptr`).
- **Assignment**: Standard (`=`) and Compound (`+=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=`).
- **Memory**: `->` (arrow access), `.` (dot access).

### Functions and Declarations
- Multiple variable declarations in a single line (e.g., `int a, b = 10, *c;`).
- Support for multiple parameters and local variable declarations.
- Support for function prototypes (declarations without bodies) ending in `;`.
- Variadic functions (e.g., `int printf(char *format, ...);`) are supported in declarations.
- Recursive function calls are fully supported.
- External function calling (e.g., `printf`, `exit`, `malloc`).
- **ARM64**: Uses registers `x0-x7` for the first 8 arguments.
- **x86_64**: Uses `rdi, rsi, rdx, rcx, r8, r9` for the first 6 arguments.

### Preprocessor and Includes
The compiler includes a built-in preprocessor that handles the following directives:
- **`#include "file.h"`**: Includes a local header file. Searches in the current directory, then in system include directories.
- **`#include <file.h>`**: Includes a system header file. Searches in the `include/` directory and any directories specified with `-I`.
- **`#define NAME value`**: Defines an object-like macro.
- **`#undef NAME`**: Undefines a macro.
- **Conditional Compilation**: Supports `#ifdef`, `#ifndef`, `#else`, and `#endif` for conditional code inclusion and header guards.

The compiler prevents multiple inclusions of the same file by tracking absolute file paths.

### Standard Library
A basic standard library is provided in the `include/` directory:
- **`stdio.h`**: Declarations for `printf`, `puts`, `getchar`, `putchar`, `sprintf`, `snprintf`.
- **`stdlib.h`**: Declarations for `exit`, `malloc`, `free`, `calloc`, `realloc`, `atoi`, `atol`, `memset`, `memcpy`.
- **`stddef.h`**: Defines `NULL`.

### Memory and Structs
- Local variables are stack-allocated within the function's scope.
- Global variables (both initialized and uninitialized) are placed in the data/comm sections.
- Structs allow for grouped data with fixed-size members.
- Members can be arrays or other structs (nested structs).

---

## Architectural Design
For a deep dive into the compiler's internal design, including the lexer, parser, optimizer, and code generator, please refer to the [ARCHITECTURE.md](ARCHITECTURE.md) document.

---

## Supported Architectures
The compiler targets macOS and generates assembly for:
- **ARM64 (Apple Silicon)**: Emits A64 assembly, follows AAPCS64.
- **x86_64 (Intel)**: Emits AT&T syntax assembly, follows System V AMD64 ABI.

Both backends implement strict 16-byte stack alignment as required by macOS.

---

## Testing and Examples
The project includes an automated test suite (`test.sh`) and several example programs in the `examples/` directory.

### Key Examples:
- `gauntlet.simple`: Comprehensive stress-test for recursion, floats, bit ops, and sorting.
- `bst.simple`: Binary Search Tree implementation testing recursive structs and functions.
- `array_struct.simple`: Verifies correct indexing for arrays of multi-byte structs.
- `bubble_sort.simple`: Implements standard bubble sort on an array.
- `binary_search.simple`: Demonstrates recursion and array indexing.

### Known Issues and Failing Tests
All test cases are currently passing.

---

## Usage Guide

### 1. Build the Compiler
```bash
zig build
```

### 2. Compile a Source File
```bash
# Target ARM64 (default)
./zig-out/bin/compiler --arch arm64 input.simple

# Target x86_64
./zig-out/bin/compiler --arch x86_64 input.simple

# Specify additional include directories
./zig-out/bin/compiler -I my_headers input.simple
```

### 3. Assemble and Run
```bash
# Rename output to .s
mv out.asm out.s

# Link with clang
clang -o my_program out.s

# Run
./my_program
```

### 4. Run the Test Suite
```bash
./test.sh
```

---

## Debugging Options
The compiler provides several flags to aid in debugging the compilation process:

- **`--dump-tokens`**: Prints the full token stream generated by the Lexer.
- **`--dump-ast`**: Prints a formatted visualization of the Abstract Syntax Tree (AST) after optimization.
- **`--asm-comments`**: Annotates the generated assembly code with comments indicating the high-level source constructs (e.g., `; If`, `; While`).

Example:
```bash
./zig-out/bin/compiler --dump-ast --asm-comments examples/arithmetic.simple
```

---

## Context for Future Development
**Current State**:
- Multi-architecture support (ARM64/x86_64) is stable.
- The codebase is modular (Backend/Parser split).
- 98/98 tests are passing.
- Key features implemented: Structs (nested), Enums, Typedef, Pointers, Arrays, Compound Assignments (arithmetic & bitwise), Ternary, Switch/Case, Postfix ops, Sizeof, Multiple Declarations, Type Casting, Block Comments, Hex/Octal Literals, String Escape Sequences, NULL macro.

**Immediate Next Steps**:
1. **Initializers**: Support `{1, 2, 3}` syntax for arrays and structs.
2. **Semantic Analysis**: Add a dedicated pass between Parser and CodeGen to handle type checking and symbol resolution more robustly.
3. **Void Pointers/Generic Pointers**: Improve handling of `void*`.
4. **`unsigned`/`long`/`short` Types**: Add integer type width variants.
5. **`static`/`const`/`extern` Qualifiers**: Storage class and type qualifiers.
6. **Function-like Macros**: `#define MAX(a,b) ((a)>(b)?(a):(b))`.
7. **`union` Type**: Overlapping member storage.

**Resume Prompt**:
> "I am working on a Zig-based C compiler. All 98 tests are passing. The project supports nested structs, multi-architecture backends (ARM64/x86_64), basic optimizations, block comments, hex/octal literals, bitwise compound assignments, and string escape sequences. Please help me implement the next feature: [Array Initializers / unsigned types / union]."
