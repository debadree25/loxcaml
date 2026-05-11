# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

LoxCaml — a treewalking interpreter for the Lox language (from *Crafting Interpreters*), written in OCaml. Built with dune; depends on `unix` and `lwt`.

## Common commands

```sh
dune build                        # build the executable (also: ./run.sh)
dune exec loxcaml -- run file.lox # run a Lox program end-to-end
./run.sh <cmd> <file.lox>         # convenience wrapper around build + exec
dune test                         # run the alcotest suite in test/
dune test --force                 # re-run tests even if cached as passing
dune fmt                          # format with ocamlformat (profile = default)
dune clean                        # nuke _build/
```

To run a single test case: `dune exec test/test_loxcaml.exe -- test "<group>" "<case>"` (e.g. `dune exec test/test_loxcaml.exe -- test scanner keywords`).

The CLI takes one of four subcommands followed by a path to a `.lox` file:

| Command    | Pipeline stage exercised                       |
| ---------- | ----------------------------------------------- |
| `tokenize` | scanner only — prints token stream              |
| `parse`    | scanner + parser — prints AST                   |
| `evaluate` | scans, parses, then evaluates the *first* statement (wraps it in `Evaluation`) and prints the value |
| `run`      | full interpreter over all statements            |

`test.lox` at the repo root is a scratch input file; `test/` holds the alcotest suite.

## Benchmarks

`bench/` contains self-timing `.lox` programs (they call `clock()` themselves and print elapsed seconds). Run `./bench/run.sh` to execute them all and see both the Lox-level timing and wall-clock time. Current programs:

- `fib.lox` — `fib(35)`.
- `loop.lox` — 10000000 iteration `while`.

Note: `clock()` is implemented via `Unix.gettimeofday`, so it has microsecond resolution.

## Architecture

The interpreter is a classic three-stage pipeline. Modules live in `src/` and are wired together in `src/main.ml`:

```
source ──▶ Scanner ──▶ Parser ──▶ Interpreter
       tokens        AST        runtime values
```

Each stage returns `Result` and short-circuits on error. `main.ml` maps stage failures to the conventional Lox exit codes: **65** for scan/parse errors, **70** for runtime errors.

### Layout

- `src/` is a single dune scope that defines two things: a library `loxcaml_lib` (everything except `main.ml`) with `(wrapped false)` so module names stay flat, and the `main` executable (public name `loxcaml`) which depends on the library. `(wrapped false)` is what lets `main.ml` and the test files write `open Scanner` etc. without a `Loxcaml_lib.` prefix.
- `test/` runs `test_loxcaml.ml` against `loxcaml_lib` via alcotest. Tests are grouped into `scanner`, `parser`, and `interpreter` suites; the interpreter tests use a `with_captured_stdout` helper that swaps the stdout fd around `Interpreter.interpreter` and reads back the result.

### Module map

- **`token.ml`** — Token ADT (`token`), the parallel "pattern" enum `token_pattern` used to match against tokens whose payload you don't care about, plus `literal`, `token_info` (token + lexeme + line), and pretty-printers.
- **`scanner.ml`** — Mutable scanner record over the source string. Produces `token_info list`; collects errors into `had_error` and returns `Error (tokens, _)` if any were reported.
- **`ast.ml`** — `expr` and `statement` ADTs and an S-expression-style printer used by the `parse` command. Note `Evaluation` is a wrapper used only by the `evaluate` subcommand to print a single statement's value.
- **`parser.ml`** — Recursive-descent parser following the Crafting Interpreters grammar (`assignment → logical_or → logical_and → equality → comparison → term → factor → unary → call → primary`). Errors propagate via `Result` (`let*` from `Utils`); on parse failure the parser calls `synchronize` to recover at statement boundaries.
- **`runtime.ml`** — Runtime `value` type (`Primitive | NativeFunc | UserFunc`), the `environment` chain (each env has an optional `enclosing` parent and a `Hashtbl` of bindings), and globals registration (currently just `clock`).
- **`interpreter.ml`** — Tree-walking evaluator. Mutable `interpreter_state` holds the current `environment`. Defines its own three-arm `execution_result` (`Ok | Error | Return`) with a custom `let*` so that a `Return v` inside a function body unwinds back to `evaluate_call` without being treated as an error.
- **`utils.ml`** — `let*` for `Result.bind`, `let**` for `Option.bind`, plus a result-aware list map.

### Conventions and patterns to know

- **Two `let*` bindings exist.** `Utils.let*` is `Result.bind` and is used by the parser. `Interpreter.let*` is `execution_bind` over the three-arm `execution_result` and is used by the evaluator. They are not interchangeable — the file's `open` lines determine which is in scope.
- **Errors are printed at the point of detection**, not at the top level. `Scanner.report_error`, `Parser.report_error`, and `Interpreter.report_runtime_error` all write to `stderr` directly; the `Error` value returned afterwards is mostly a signal to stop, not data to be formatted later.
- **Scopes are pushed/popped on a single mutable interpreter state.** Block evaluation calls `push_environment`/`pop_environment`. Function calls use `push_closure`/`pop_closure`, which swap the current env to a fresh child of the *captured closure environment* (not the caller's), then restore.
- **Function calls implement returns via the result type.** `Return v` in `execution_result` is caught inside `evaluate_call` and converted back to `Ok v`. Anywhere else, `Return` propagates through `let*`.
- **Parser uses `token_pattern` to match on tagged-value tokens.** Tokens like `IDENTIFIER s` / `NUMBER n` / `STRING s` carry payloads, so you can't compare them to a literal pattern. `check_pattern` / `match_tokens_by_pattern` / `consume_by_pattern` operate on `token_pattern` (the payload-stripped enum) for these cases.
- **`for` is desugared in the parser** into `Block [initializer; While (cond, Block [body; increment])]`. There's no `For` node in the AST.
- **Compiler warnings are silenced** via `(flags (:standard -warn-error -A))` in `src/dune`. Treat new warnings as something to fix even though the build won't fail on them.

### Adding a new language feature

A typical change touches all four core modules in this order:

1. `token.ml` — add the token (and its `token_pattern` variant if the token carries a payload), update every match in the printers and `token_to_token_pattern`.
2. `scanner.ml` — emit the token from `scan_token` (and reserved-word table if it's a keyword).
3. `ast.ml` — add the `expr`/`statement` constructor and extend the AST printers.
4. `parser.ml` — slot the new production into the right precedence level.
5. `interpreter.ml` — handle the new node in `evaluate_expr` / `evaluate_statement`.

Pattern matches in OCaml are exhaustive, so the compiler will tell you everywhere you missed (subject to the `-warn-error -A` caveat above).
