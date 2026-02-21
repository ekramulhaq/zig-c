# Simple Compiler Project Documentation

This document summarizes the development and current state of the Zig-based compiler project.

## Project Overview
The project is a hand-written compiler for a subset of the C language, implemented in Zig. It translates high-level source code into assembly language for macOS, supporting both Intel (x86_64) and Apple Silicon (ARM64) architectures.

## What Has Been Done

### 1. Core Compiler Infrastructure
- **Lexer**: Tokenizes source text into a stream of tokens (Keywords, Identifiers, Numbers, Operators, Braces).
- **Parser**: A recursive descent parser that constructs an Abstract Syntax Tree (AST). It handles:
    - Variable declarations and assignments.
    - Arithmetic expressions (with operator precedence).
    - Control flow (`if/else`, `while`, `for` loops).
    - Return statements.
- **Code Generator**: A multi-target backend that emits assembly code.

### 2. Multi-Architecture Support
The compiler now supports two primary architectures through a command-line flag (`--arch`):

#### **ARM64 (Apple Silicon)**
- Emits **A64 assembly**.
- Implements 16-byte stack alignment required by the macOS ARM64 ABI using `stp` and `ldp` instructions.
- Uses PC-relative addressing via `adrp` and `@PAGE/@PAGEOFF` relocations for global variable access.
- Correctly handles the ARM64 calling convention (return values in `x0`).

#### **x86_64 (Intel)**
- Emits **AT&T syntax assembly** (compatible with native macOS `clang` and `as`).
- Uses RIP-relative addressing for global variables.
- Correctly handles the x86_64 system call interface for macOS (using `0x2000001` for exit).

### 3. Critical Fixes and Improvements
- **Parser Robustness**: Fixed recursive error set inference issues in Zig that prevented the compiler from building.
- **Arithmetic Logic**: Corrected the operand order for subtraction and division in the generated assembly.
- **String Formatting**: Fixed runtime panics by using correct Zig format specifiers (`{s}`) for string slices.
- **CLI Enhancements**: Added support for reading source code from external files and selecting the target architecture at runtime.

### 4. Testing and Verification
- **Automated Test Suite**: Created `test.sh`, which automates the entire lifecycle:
    1. Builds the compiler.
    2. Compiles multiple test cases (`examples/*.simple`).
    3. Assembles and links for both `arm64` and `x86_64`.
    4. Verifies the execution result via exit codes.
- **Example Programs**: Created a library of examples covering loops, nested logic, and complex arithmetic.

## Current Project State
The project is currently in a **fully verified, functional state**. It successfully cross-compiles and executes a range of logic-heavy programs on macOS across both modern hardware architectures.

## Usage
### Compile a file
```bash
# To ARM64
zig build run -- examples/loop.simple --arch arm64

# To x86_64
zig build run -- examples/loop.simple --arch x86_64
```

### Run the full test suite
```bash
./test.sh
```
