# Compiler Architecture

This document provides a technical overview of the compiler's internal design and implementation.

## Overview

The compiler is a multi-pass, multi-architecture compiler for a subset of the C language, implemented in Zig. It follows a classic frontend-middle-backend architecture.

```mermaid
graph LR
    Source[Source Code] --> Lexer
    Lexer --> Parser
    Parser --> AST[AST]
    AST --> Optimizer
    Optimizer --> CodeGen
    CodeGen --> Assembly[Target Assembly]
```

## 1. Frontend

### Lexer (`src/lexer.zig`)
- **Type**: Hand-written scanner.
- **Responsibility**: Converts raw source text into a stream of `Token` objects.
- **Features**: 
    - Supports multi-character tokens (`==`, `>>=`, etc.).
    - Handles string literals and character literals with escape sequences (`
`, `	`).
    - Skips comments (`//`).

### Parser (`src/parser.zig`)
- **Type**: Recursive Descent Parser.
- **Responsibility**: Consumes tokens and builds an Abstract Syntax Tree (AST).
- **Features**:
    - Implements C-standard operator precedence (13 levels).
    - Handles complex control flow (nested loops, `if/else`, `do-while`, `switch/case`).
    - Supports ternary operator (`? :`), postfix operations (`++`, `--`), and `sizeof(type)`.
    - Supports multiple variable declarations in a single statement (`int a, b, *c;`).
    - Manages `typedef` and `enum` symbol tables during the parse phase for immediate substitution.
    - Context-aware lookahead for differentiating between function definitions and global variable declarations.

### AST (`src/ast.zig`)
- **Structure**: A tree of `Node` objects.
- **Design**: Nodes are polymorphic via the `NodeType` enum and carry optional data fields (name, value, operator, etc.) to minimize allocation overhead while remaining flexible.

## 2. Middle-end

### Optimizer (`src/optimizer.zig`)
- **Type**: AST-to-AST transformation pass.
- **Responsibility**: Performs high-level optimizations before code generation.
- **Optimizations**:
    - **Constant Folding**: Evaluates arithmetic, bitwise, comparison, and logical operations at compile-time if operands are constant.
    - **Dead Code Elimination**: Basic logic for removing unreachable code branches (future extension).

## 3. Backend

### Code Generator (`src/codegen.zig`)
- **Responsibility**: Translates the optimized AST into target-specific assembly code.
- **Design**: Modular backend that abstracts architecture-specific details behind an `Arch` interface.
- **Type Resolution**: Implements `resolveTypeInfo` for recursive type discovery (e.g., resolving `rect.top_left.x` to the correct struct member type and offset across multiple nesting levels).

#### **Stack Management**
- **Frame Layout**: Each function manages its own stack frame (currently fixed at 2048 bytes for simplicity).
- **Locals**: Local variables are allocated negative offsets from the Base Pointer (`RBP`/`X29`).
- **Temporaries**: Intermediate expression results are stored in a "temporary stack area" (offset -1024 and below) to avoid collisions with local variables.

#### **Calling Conventions**
- **ARM64 (Apple Silicon)**: 
    - Follows the AAPCS64 calling convention.
    - Uses `x0-x7` for parameters and `x0` for return values.
    - Maintains 16-byte stack alignment.
- **x86_64 (Intel)**: 
    - Follows the System V AMD64 ABI.
    - Uses `rdi, rsi, rdx, rcx, r8, r9` for parameters and `rax` for return values.
    - Implements strict 16-byte alignment before `call` instructions.

## 4. Extensibility and Maintenance

The compiler is designed to be easily extensible:
- **New Types**: The `DataType` enum in `ast.zig` can be expanded to support `float`, `long`, etc.
- **New Architectures**: Adding a new target involves extending the `Arch` enum in `common.zig` and implementing corresponding logic in `codegen.zig`.
- **New Optimizations**: Additional passes can be added to the `Optimizer` or as new passes in `main.zig`.
