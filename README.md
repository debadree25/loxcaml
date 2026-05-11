# LoxCaml

A treewalking interpreter for the [Lox language](https://craftinginterpreters.com/the-lox-language.html), written in OCaml. Follows [Crafting Interpreters](https://craftinginterpreters.com/).

## Requirements

- OCaml `>= 4.14`
- [dune](https://dune.build/) `>= 3.16`
- `lwt`

The easiest way to get a working toolchain is the bundled setup script. It handles a fresh Linux or macOS environment end-to-end — installs system prereqs, opam, a local switch, and project deps. On Linux it also detects when bubblewrap is installed but can't sandbox (the usual Docker / WSL / restricted-kernel snag) and falls back to `opam init --disable-sandboxing` instead of failing partway:

```sh
./setup.sh
```

Or if you already have opam:

```sh
opam switch create . 5.2.0
opam install . --deps-only --with-test
```

## Build

```sh
dune build
```

## Test

```sh
dune test
```

## Run

The interpreter accepts a subcommand and a path to a Lox source file:

```sh
dune exec loxcaml -- <command> <file.lox>
# or, after a build:
./run.sh <command> <file.lox>
```

Commands:

| Command    | Output                                              |
| ---------- | --------------------------------------------------- |
| `tokenize` | Prints the token stream                             |
| `parse`    | Prints the parsed AST                               |
| `evaluate` | Evaluates a single expression and prints the value  |
| `run`      | Executes the program                                |

Example:

```sh
./run.sh run test.lox
```
