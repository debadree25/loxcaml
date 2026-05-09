# AGENTS.md

Guidance for AI coding agents working in this repository. See `CLAUDE.md` for the full project briefing — this file is the short version.

## Project at a glance

LoxCaml is a treewalking interpreter for the Lox language (from *Crafting Interpreters*), implemented in OCaml with dune. Source lives in `src/`; the entrypoint is `src/main.ml`.

## Build, run, test

```sh
dune build                          # build
./run.sh <command> <file.lox>       # build + run
dune exec loxcaml -- <cmd> <file>   # alternative
dune test                           # run the alcotest suite in test/
dune fmt                            # format
```

`<command>` is one of `tokenize`, `parse`, `evaluate`, `run`. Exit codes follow Lox conventions: **65** for scan/parse errors, **70** for runtime errors.

`test.lox` at the root is a scratch input you can run manually; the real test suite lives in `test/test_loxcaml.ml` (alcotest).

## Architecture in one paragraph

Source → `Scanner` (`src/scanner.ml`) → tokens → `Parser` (`src/parser.ml`) → AST (`src/ast.ml`) → `Interpreter` (`src/interpreter.ml`) → values (`src/runtime.ml`). Each stage returns `Result` and short-circuits. The interpreter walks the AST against a mutable `interpreter_state` whose `environment` chain is push/pop-ed for blocks and swapped to a closure's captured environment for function calls.

## Things that bite

- **Two different `let*` operators exist.** `Utils.let*` is `Result.bind` (used by parser). `Interpreter.let*` is `execution_bind` over a three-arm `Ok | Error | Return v` type (used by evaluator). The file's `open`s determine which is in scope.
- **`Return v` is a control-flow value, not an error.** `evaluate_call` catches it and converts to `Ok v`; elsewhere it propagates through `let*` to unwind out of nested function bodies.
- **Tokens with payloads (`IDENTIFIER`, `NUMBER`, `STRING`) require `token_pattern` matching** in the parser via `check_pattern` / `match_tokens_by_pattern` / `consume_by_pattern`. Direct equality on the payload-bearing token won't match.
- **Errors print at the point of detection**, not at the top level. Returning `Error _` is a *signal*; the message has already gone to stderr.
- **`for` loops have no AST node** — the parser desugars them into `Block + While`.
- **Warnings are non-fatal** (`-warn-error -A` in `src/dune`). Don't ignore them; the compiler just won't enforce them.

## Adding a language feature

Touch the modules in this order — the compiler's exhaustiveness checks will guide the rest:

1. `token.ml` — new variant + update printers and `token_to_token_pattern`
2. `scanner.ml` — emit from `scan_token` (or add to the reserved-word table)
3. `ast.ml` — new `expr`/`statement` constructor + AST printer arm
4. `parser.ml` — wire into the right precedence level
5. `interpreter.ml` — new arm in `evaluate_expr` / `evaluate_statement`

## What not to do

- Don't reintroduce CodeCrafters scaffolding (`.codecrafters/`, `codecrafters.yml`, `/tmp/codecrafters-build-*` paths). This repo is no longer driven by CodeCrafters' runner.
- Don't add backwards-compatibility shims for the old package name `codecrafters_interpreter` — the package is now `loxcaml`.
- Don't push to a remote named `origin` blindly without checking — see `git remote -v` first.
