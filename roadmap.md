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
 - [ ] Product and sum type definitions: `data` and `record`.
 - [ ] Type aliases.
 - [ ] Generics.
 - [ ] Traits.
   - [ ] Static dispatch.

## Modules
 - [ ] `use` statements.
 - [ ] `export` statements.
 - [ ] Relative importing of modules.
 - [ ] Import select symbols from a module.
 - [ ] Import all visible symbols from a module.

## Compiler Specifics
 - [x] Lexer
 - [x] Parser
 - [ ] Type checker
 - [ ] Backends.
   - [ ] C
   - [ ] Cranelift
   - [ ] LLVM
 - [ ] LSP server.
 - [ ] Warnings when variables are unused.

## Concurrency
 - [ ] Actor model concurrency.
