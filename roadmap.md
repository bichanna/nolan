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
 - [ ] Pattern matching.
   - [ ] Variable / match-all patterns.
   - [ ] Constructor patterns.
   - [ ] Number literal patterns.
   - [ ] `or` clause in pattern matching.
   - [ ] Pattern guards.
   - [ ] Completeness and redundancy checking.

## Types
 - [ ] Minimal type inference.
 - [ ] Product and sum type definitions: `record` and `enum`.
 - [ ] Type aliases.
 - [ ] Generics.
 - [ ] Traits.
   - [ ] Static dispatch.

## Modules
 - [ ] Import expressions.
 - [ ] Export statements.
 - [ ] Relative importing of modules.
 - [ ] Import select symbols from a module.
 - [ ] Import all visible symbols from a module.

## Compiler Specifics
 - [ ] Backends.
   - [ ] C
   - [ ] Cranelift
   - [ ] LLVM
 - [ ] LSP server.
 - [ ] Warnings when variables are unused.
 - [ ] Line continuations.

## Concurrency
 - [ ] Actor model concurrency.
