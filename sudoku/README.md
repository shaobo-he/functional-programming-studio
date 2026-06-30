# sudoku

Sudoku solvers in Racket and Lean 4, cross-checked against each other.

## Layout

```
sudoku.rkt       Racket: data model, parser, DFS backtracking solver
sudoku-cp.rkt    Racket: Norvig-style constraint-propagation solver (assign/eliminate + MRV search)
sudoku-gen.rkt   Racket: puzzle generator built on the CP propagator
test.rkt         Racket: board fixtures
lean/            Lean 4: a constraint-propagation solver, verified by the type-checker
  Sudoku/CP/     Model, Propagation, Search, IO
  testdata/*.sdk puzzles used by the diff test
scripts/
  racket-solve.rkt   stdin .sdk -> solved grid, using the Racket CP solver
  diff-sudoku.sh     run Lean and Racket on every testdata puzzle and assert they agree
```

## Run

Racket (a `.sdk` puzzle on stdin):

```
racket scripts/racket-solve.rkt < lean/testdata/leetcode.sdk
```

Lean (build = check the proofs, then run the solver):

```
cd lean && lake build
.lake/build/bin/sudokuSolve < testdata/leetcode.sdk
```

Cross-check both implementations on all test puzzles:

```
bash scripts/diff-sudoku.sh
```

The Lean solver's correctness is carried by the Lean type-checker — `lake build`
succeeding is the proof check. The diff test gives the Racket solver an
independent oracle (the verified Lean solver) on every puzzle.
