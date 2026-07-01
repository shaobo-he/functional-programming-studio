# Santorini AlphaZero

This directory contains an AlphaZero-style player and self-play trainer for the
repository's basic Santorini rules. It handles the 5x5, two-worker game without
god powers. The existing Haskell implementation remains the authoritative game
model and the main evaluation opponent.

The implementation is deliberately small enough for one workstation:

- Python and PyTorch for training and inference.
- A 466,201-parameter residual policy/value network.
- Batched PUCT search across concurrent self-play games.
- Sparse replay targets and all eight square-board symmetries.
- A line-oriented player compatible with the existing Haskell referee.
- Differential tests against `Santorini.Core`.

## Design

### State

The neural input has shape `7 x 5 x 5`:

1. Five one-hot planes for tower levels 0 through 4.
2. One plane for the two workers belonging to the player to move.
3. One plane for the opponent's two workers.

Every state is normalized to the player-to-move perspective. Worker identities
are not encoded in the neural input.

### Actions

The policy has 1,800 fixed logits:

```
25 origin cells * 8 move directions * 9 build choices
```

The ninth build choice means "no build" and is legal only for a winning step
onto level 3. Illegal logits are masked by the game engine. Encoding the origin
cell instead of a worker number keeps the action format independent of worker
ordering.

### Network

The default network has a 64-channel stem, six residual blocks, a spatial
72-plane policy head, and a scalar value head. The value is from the current
player's perspective and lies in `[-1, 1]`.

### Search and self-play

`BatchedMCTS` runs PUCT. During self-play, each search round selects one leaf
from every active game and evaluates those leaves as one neural batch. Root
Dirichlet noise and move sampling are enabled during early self-play moves.

Games start from uniformly randomized legal worker placements. Training
examples store only legal action indices and normalized visit probabilities;
dense 1,800-element targets are created only for the current optimizer batch.
Each sampled training example receives a random dihedral board symmetry.

Terminal wins and stalemates are handled exactly by the rules engine. Neural
values are used only for nonterminal search leaves.

## Correctness boundary

`src/santorini_az/game.py` is a Python port of `Santorini.Core`. To prevent the
trainer from silently learning a different game, `app/Oracle.hs` exposes every
legal Haskell successor for a JSON board. `tests/test_differential.py` compares
complete Python and Haskell successor sets over random openings and trajectories.

The remaining tests cover action round trips, symmetries, terminal states,
timed search, replay replacement, network checkpoints, an optimizer step, and
protocol placement.

## Environment

Create a project-local virtual environment:

```sh
cd santorini/alphazero
python3 -m venv .venv
.venv/bin/python -m pip install -e .
```

The checked development environment uses PyTorch 2.12.1 with CUDA 13.0. The
virtual environment, mutable training checkpoints, Python caches, and match
logs are not committed. The inference-only five-iteration pilot weights are
versioned as `checkpoints/pilot-5iter.pt`.

## Tests

Build the Haskell oracle first, then run the Python suite:

```sh
cd santorini
cabal build santorini-oracle
cd alphazero
.venv/bin/python -m unittest discover -s tests -v
```

Run the existing Haskell suite separately:

```sh
cd santorini
cabal test all --test-show-details=direct
```

## Benchmark

Measure end-to-end neural evaluation, including state encoding and copying
outputs back to the CPU:

```sh
cd santorini/alphazero
.venv/bin/santorini-az-benchmark --device cuda
```

Measured on the repository machine (Ryzen 5 5600X, RTX 3060 Ti 8 GiB):

| Batch | Evaluations/second |
|------:|-------------------:|
| 32    | 19,348 |
| 64    | 35,816 |
| 128   | 60,913 |
| 256   | 95,115 |
| 512   | 124,192 |
| 1024  | 127,278 |

Complete self-play with 512 concurrent games produced about 61,000 games/hour
at 16 MCTS simulations. Training batch 1,024 gave the best useful throughput;
batch 2,048 did not improve it materially.

## Training

A minimal smoke run is:

```sh
cd santorini/alphazero
.venv/bin/santorini-az-train \
  --iterations 1 \
  --games 8 \
  --parallel-games 8 \
  --simulations 16 \
  --train-steps 4 \
  --batch-size 32
```

Running without overrides uses the measured workstation defaults:

```sh
.venv/bin/santorini-az-train --device cuda
```

Those defaults are 512 games per iteration, 512 concurrent games, 64 MCTS
simulations, optimizer batch 1,024, and a 500,000-position replay capacity. The
latest checkpoint is written after every iteration to
`checkpoints/latest.pt`.

The initial five-iteration pilot generated 2,560 games and 78,506 positions.
Policy loss fell from 4.50 to 3.46 and value loss from 1.00 to 0.92.

## Playing

The Python entry point accepts a checkpoint and speaks the same JSON protocol as
the Haskell players:

```sh
SANTORINI_TIME_MS=1000 \
  .venv/bin/santorini-az-player checkpoints/latest.pt
```

The protocol player searches for 95% of `SANTORINI_TIME_MS`, preserving a small
response margin. `SANTORINI_AZ_SIMS=N` or `--simulations N` selects a fixed
simulation count instead. Placement is randomized by default; set
`SANTORINI_AZ_RANDOM_PLACEMENT=0` for the deterministic Haskell preference.

The Haskell referee requires a no-argument executable. Use the wrapper:

```sh
cd santorini
REFEREE=$(cabal list-bin referee)
MODERN=$(cabal list-bin modern-player)
"$REFEREE" ./alphazero/play-checkpoint.sh "$MODERN" 11 1000 match.log
```

Set `SANTORINI_AZ_CHECKPOINT` to make the wrapper load a checkpoint other than
its default. It loads a local `checkpoints/latest.pt` when one exists and falls
back to the committed `checkpoints/pilot-5iter.pt` on a fresh clone.

## Pilot results

The five-iteration checkpoint was evaluated with randomized legal placements:

- 16-4 against `modern-player` at 200 ms per turn using 128 simulations.
- 11-0 against `modern-player` at 1,000 ms per turn using timed search.
- No illegal moves, protocol failures, or timeouts in either evaluation.

These are small development matches, not a statistically rigorous strength
claim. They establish that the pipeline learns, produces legal protocol moves,
honors its time budget, and is competitive with the existing search engine.

## Current limitations

- Replay data is kept in memory and is not restored from a checkpoint. Restarting
  training restores model and optimizer state but begins a fresh replay buffer.
- MCTS tree traversal is single-threaded Python. Parallelism comes from batching
  many self-play games into GPU inference.
- The implementation uses conventional PUCT, not Gumbel AlphaZero.
- Worker placement is randomized rather than learned by the policy network.
- Mutable checkpoints include optimizer state and remain local. The committed
  pilot file contains inference weights only and cannot resume the optimizer.
