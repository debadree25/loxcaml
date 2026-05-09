#!/bin/sh
#
# Build and run loxcaml on a Lox source file.
#
# Usage: ./run.sh <command> <file.lox>
#   command: tokenize | parse | evaluate | run

set -e

cd "$(dirname "$0")"
dune build
exec ./_build/default/src/main.exe "$@"
