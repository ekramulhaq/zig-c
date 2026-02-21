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
The Simple Compiler is a hand-written compiler for a subset of the C language, implemented in Zig. It targets macOS (both ARM64 and x86_64 architectures) and produces assembly code compatible with the system assembler (`clang`).

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
- **Numeric**: Decimal integers (e.g., `42`).
- **Character**: Single-quoted characters (e.g., `'A'`). Supports escape sequences: `\n`, `\t`, `\r`, `\\`, `\'`, `\0`.
- **String**: Double-quoted strings (e.g., `"Hello, world!"`).

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
- **Assignment**: Standard (`=`) and Compound (`+=, -=, *=, /=, %=`).
- **Memory**: `&` (address-of), `*` (dereference), `->` (arrow access), `.` (dot access).

### Functions
- Support for multiple parameters and local variable declarations.
- Recursive function calls are fully supported.
- External function calling (e.g., `printf`, `exit`, `malloc`).
- **ARM64**: Uses registers `x0-x7` for the first 8 arguments.
- **x86_64**: Uses `rdi, rsi, rdx, rcx, r8, r9` for the first 6 arguments.

### Memory and Structs
- Local variables are stack-allocated within the function's scope.
- Global variables (both initialized and uninitialized) are placed in the data/comm sections.
- Structs allow for grouped data with fixed-size members (currently all members are 8 bytes).

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
- `loop_control.simple`: Demonstrates `do-while`, `break`, and `continue`.
- `void_char.simple`: Showcases `void` functions and character literals.
- `typedef_enum.simple`: Illustrates type aliasing and enumeration usage.
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
