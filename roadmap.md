# Roadmap

## Operators 
 - [ ] Basic arithmetic, `+`, `-`, `/`, `*`, `%`, comparisons, etc.
 - [ ] Pipeline operators: `.` and `<.`.
 - [ ] Assignment: `=`.
 - [ ] `+=`, `-=`, `/=`, and others.
 - [ ] Bit-wise operators: `bnot`, `band`, `bor`, `bxor`.

## Functions and Control Flows
 - [ ] Functions + lambdas.
   - [ ] Function overloading.
   - [ ] Closure conversion.
 - [ ] Tail recursion.
 - [ ] If statements and expressions.
 - [ ] While loop.
 - [ ] Switch expression.

## Types
 - [ ] Minimal type inference.
 - [ ] Simple `struct` and `enum`.
 - [ ] Type aliases.
 - [ ] Generics.
 - [ ] Traits.
   - [ ] Static dispatch.

## Modules
 - [ ] `import` statements.
 - [ ] `export` statements.
 - [ ] Relative importing of modules.
 - [ ] Import select symbols from a module.
 - [ ] Import all visible symbols from a module.

## Compiler Specifics
 - [x] Lexer
 - [x] Parser
 - [ ] Type checker
 - [ ] Single ownership
 - [ ] Backends.
   - [ ] C
   - [ ] Cranelift
   - [ ] [Nolan VM](https://github.com/bichanna/nolan-vm)
 - [ ] LSP server.
 - [ ] Warnings when variables are unused.

## Concurrency
 - [ ] Actor model concurrency.
