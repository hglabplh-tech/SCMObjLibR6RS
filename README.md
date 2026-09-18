# SCMObjLibR6RS

SCMObjLibR6RS is an experimental object-oriented language and runtime project
implemented with Scheme. It began as an exploration of how classes, objects,
interfaces, typed members, access control, and method dispatch can be built from
the language facilities provided by Scheme itself.

The project is now being renewed using the R6RS language supported by Racket.
Racket will provide the development and execution platform, while R6RS will be
used deliberately as the foundation of the portable core.

## Project status

The repository contains historical code originally written for MzScheme. That
code represents an early prototype and is being retained as the behavioral and
conceptual basis for the renewed implementation. It should not be mistaken for
the final architecture.

The existing object system already explores:

- Classes and object construction
- Single inheritance
- Public, protected, and private members
- Mutable and immutable member variables
- Predicate-based types for fields, parameters, and return values
- Additional value contracts
- Method dispatch and runtime method signatures
- Method overloading
- Constructors
- `self` and super-method calls
- Interfaces and checked interface views
- Positional and named method arguments

The repository also contains early work on a tokenizer, declarative language
definitions, abstract syntax trees, environments, address tables, and a
register-based processor. These parts are not yet one complete runtime.

## Direction

The long-term goal is to connect and extend the existing components into a
coherent language implementation:

```text
source program
      |
      v
 tokenizer -> parser -> abstract syntax tree
                             |
                             v
                    semantic analysis
                             |
                             v
                 intermediate instructions
                             |
                             v
                     processor / runtime
```

The renewed project is expected to include:

1. An R6RS object runtime with clearly defined semantics.
2. A modernized class and interface definition language.
3. A tokenizer and parser connected to a defined AST.
4. Name, scope, type, contract, and access-control analysis.
5. An intermediate representation for executable programs.
6. A processor or virtual machine that executes that representation.
7. Racket-based tools for testing, debugging, and running programs.

This is a long-term direction rather than a claim that all these components are
already complete.

## Is this project useful for studying?

Yes. Renewing and extending the project can be particularly useful for studying
how programming languages are constructed. Instead of using an existing object
system as a black box, the project exposes the mechanisms underneath it.

Topics that can be studied through the project include:

- Hygienic macros and language extension
- Runtime representation of classes and objects
- Dynamic dispatch and method lookup
- Inheritance and access-control rules
- Structural and nominal interface designs
- Predicate-based type checking and contracts
- Lexical environments and symbol resolution
- Tokenization, parsing, and AST construction
- Intermediate representations
- Interpreters, bytecode processors, and virtual machines
- Conditions, error reporting, and runtime diagnostics
- Portability boundaries between R6RS and a Racket host
- Testing a language implementation by observable behavior

The age of the original prototype is useful in this context: it makes it
possible to compare older Scheme implementation techniques with present-day
R6RS and Racket facilities. Porting the project is therefore not only
maintenance work; it is also an opportunity to examine design decisions,
preserve intended behavior, replace accidental complexity, and document a
language as it evolves.

This project is best approached as a learning, research, and experimentation
environment. It is not currently intended to compete with Racket's production
class system or to serve as a production-ready runtime.

## Renewal principles

The renewal should preserve the ideas and observable features of the original
prototype without requiring every historical implementation detail to survive.
In particular:

- Existing behavior should be captured by tests before it is changed.
- The portable core should use explicit R6RS libraries.
- Racket-specific integration should be kept at a clear boundary.
- Public language semantics should be documented independently of their
  implementation.
- The tokenizer, parser, processor, and object runtime should communicate
  through defined data structures and interfaces.
- Experimental components should be identified honestly as experimental.
- Improvements to error handling, performance, and maintainability should not
  silently alter language behavior.

## Current compatibility

Much of the historical source still uses the `mzscheme` module language, old
SRFI module paths, mutable-pair compatibility libraries, and a retired PLaneT
test dependency. A central part of the renewal is to replace these dependencies
with explicit R6RS and current Racket facilities.

Until that work is complete, running the historical code may require a full
Racket installation with compatibility libraries. The checked-in historical
tests and examples describe useful behavior, but they are not yet organized as
a modern, portable test suite.

## License

See [COPYING](./COPYING) and [LICENSE](./LICENSE) for the license and copyright information.
