#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
lean_dir="$repo_root/lean"
tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

cd "$lean_dir"
elan run leanprover/lean4:v4.31.0 lake build >/dev/null

for puzzle in "$lean_dir"/testdata/*.sdk; do
  name="$(basename "$puzzle")"
  "$lean_dir/.lake/build/bin/sudokuSolve" < "$puzzle" > "$tmp_dir/$name.lean"
  racket "$repo_root/scripts/racket-solve.rkt" < "$puzzle" > "$tmp_dir/$name.racket"
  diff -u "$tmp_dir/$name.racket" "$tmp_dir/$name.lean"
  echo "ok $name"
done
