# mini-c-lang
A MIPS compiler for a C-like language using Scala and GraalVM Polyglot runtime with Flex and Yacc.

## Features
- Lexer and Parser written in C using Flex and Yacc.
- Scala extractors over the generated AST using GraalVM polyglot interface and opaque types.
- AST Interpreter
- Constant Folding
- Y normal form
- Three Address Code generation
- MIPS 2000 code generation (currently only supports registers).

## Todo
- Add while loops and functions to TAC and MIPS
- Add stack memory and registers
- Add heap memory
- Refactor so that Scala compiler doesn't take 10 minutes for pattern matching logic.
