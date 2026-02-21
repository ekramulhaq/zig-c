# Simple Compiler Project Documentation

This document summarizes the development and current state of the Zig-based compiler project.

## Project Overview
The project is a hand-written compiler for a subset of the C language, implemented in Zig. It translates high-level source code into assembly language for macOS, supporting both Intel (x86_64) and Apple Silicon (ARM64) architectures.

## Features Implemented

### 1. Core Language Constructs
- **Data Types**: Integers (`int`) and Pointers (`int *p`).
- **Operators**:
    - Arithmetic: `+, -, *, /, %`
    - Bitwise: `&, |, ^, ~, <<, >>`
    - Comparison: `==, !=, >, <, >=, <=`
    - Logical: `&&, ||, !`
    - Unary: `- (negation), ~ (bitwise NOT), ! (logical NOT)`
- **Assignments**: Standard (`=`) and Compound (`+=, -=, *=, /=, %=`).
- **Control Flow**: `if/else`, `while` loops, `for` loops.
- **Function Support**: 
    - Multiple parameters (up to 6 on x86_64, 8 on ARM64 via registers).
    - Recursive calls.
    - External function calls (e.g., `printf`, `exit`).
- **Memory**:
    - Local variables (stack-allocated).
    - Global variables (initialized and uninitialized).
    - Pointers (`&` address-of, `*` dereference).
- **Strings**: String literals support (stored in `__cstring` section).
- **Comments**: Single-line comments (`//`).

### 2. Multi-Architecture Support
The compiler targets macOS on two architectures:

#### **ARM64 (Apple Silicon)**
- Emits **A64 assembly**.
- Implements 16-byte stack alignment.
- Uses PC-relative addressing (`adrp`, `@PAGE`) for globals and string literals.
- Follows standard calling convention (registers `x0-x7`).

#### **x86_64 (Intel)**
- Emits **AT&T syntax assembly**.
- Maintains strict 16-byte stack alignment.
- Uses caller-saved registers (`%r10`) for temporaries to avoid ABI violations.
- Implements standard x86_64 calling convention (registers `rdi, rsi, ...`).

### 3. Compilation Infrastructure
- **Lexer**: Hand-written scanner with support for multi-character tokens and strings.
- **Parser**: Recursive descent parser implementing standard C operator precedence (13 levels).
- **Code Generator**: Stack-offset based backend that manages a 512-byte stack frame per function.

## Testing and Verification
- **Automated Test Suite**: `test.sh` verifies 22 test cases across both architectures.
- **Example Programs**:
    - `loop.simple`: While loops and accumulation.
    - `functions.simple`: Recursive Fibonacci and multi-argument calls.
    - `pointers.simple`: Pointer arithmetic and dereferencing.
    - `globals.simple`: Initialized global variable state.
    - `printf.simple`: External variadic call demonstration.

## Usage
### Compile a file
```bash
# To ARM64
zig build run -- examples/pointers.simple --arch arm64

# To x86_64
zig build run -- examples/pointers.simple --arch x86_64
```

### Run the full test suite
```bash
./test.sh
```
