#!/bin/sh
#
# Run each bench/*.lox program through loxcaml and report:
#   - what the Lox program self-times via clock()
#   - the total wall-clock time (build excluded)
#
# Same .lox files will work against the bytecode VM once it exists —
# self-timing means the comparison is apples-to-apples without changes
# to this script.

set -e

cd "$(dirname "$0")/.."
dune build

EXE=./_build/default/src/main.exe

for f in bench/*.lox; do
  name=$(basename "$f" .lox)
  printf '\n=== %s ===\n' "$name"
  start=$(perl -MTime::HiRes=time -e 'printf "%.6f\n", time')
  "$EXE" run "$f"
  end=$(perl -MTime::HiRes=time -e 'printf "%.6f\n", time')
  printf 'wall-clock seconds: %.3f\n' "$(echo "$end - $start" | bc -l)"
done
