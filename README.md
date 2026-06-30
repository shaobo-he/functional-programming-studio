# functional-programming-studio

[![CI](https://github.com/shaobo-he/functional-programming-studio/actions/workflows/ci.yml/badge.svg)](https://github.com/shaobo-he/functional-programming-studio/actions/workflows/ci.yml)

A small studio of functional-programming projects, each exploring a problem in a
different language/paradigm.

```
santorini/   Haskell — a Santorini board-game engine + protocol referee
sudoku/      Racket + Lean — Sudoku solvers, and a formally verified Lean solver
```

## santorini/ — Haskell

A [Santorini](https://en.wikipedia.org/wiki/Santorini_(game)) engine that speaks
the [Utah CS6963 line-delimited JSON protocol](https://users.cs.utah.edu/~mflatt/cs6963/s19/santorini.html),
plus a **referee** that pits two protocol-speaking player programs against each
other and logs every move.

- **modern-player** — negamax MCTS + an exact MCTS-Solver + heavy rollouts;
  time-bounded and multi-core (root-parallel).
- **legacy-player** — the original course submission, repaired to be
  negamax-correct (a different game model + a zipper MCTS).
- **referee** — validates every move, enforces a per-move time limit, detects
  win/stalemate, and writes a full transcript.

One Cabal project, three executables:

```
cd santorini
cabal build            # build everything (-Wall clean)
cabal test             # unit tests (engine, sampler, JSON round-trip)
./play.sh              # modern vs legacy via the referee (see santorini/README.md)
```

See [`santorini/README.md`](santorini/README.md) for the protocol, the engines,
and match/replay details.

## sudoku/ — Racket + Lean

Sudoku solvers in two languages, cross-checked against each other:

- **Racket** (`sudoku.rkt`, `sudoku-cp.rkt`, `sudoku-gen.rkt`): a DFS
  backtracking solver and a Norvig-style constraint-propagation solver, plus a
  generator.
- **Lean 4** (`lean/`): a Sudoku constraint-propagation solver whose correctness
  is established by the Lean type-checker — building the project *is* checking
  the proofs.
- **`scripts/diff-sudoku.sh`**: runs the Lean solver and the Racket solver on
  every puzzle in `lean/testdata/` and asserts they agree.

```
cd sudoku
( cd lean && lake build )          # build + check the Lean proofs
bash scripts/diff-sudoku.sh        # Lean vs Racket agreement on all test puzzles
```

## CI

[GitHub Actions](.github/workflows/ci.yml) runs on every push / PR to `main`:

- **Haskell** — `cabal build` (`-Wall`), `cabal test`, and a referee smoke game.
- **Sudoku** — `lake build` (checks the Lean proofs) and the Lean↔Racket diff
  test (exercises the Racket solver against the verified Lean one).
