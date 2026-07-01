# santorini

All the Santorini code in one project: two MCTS engines plus a **referee**, all
speaking the [Utah CS6963 Santorini
protocol](https://users.cs.utah.edu/~mflatt/cs6963/s19/santorini.html). The
referee pits two protocol-speaking player programs against each other and records
every step of the game.

The "two versions" that play each other are:

- **modern** — `modern-player`: negamax MCTS + an exact MCTS-Solver + heavy
  rollouts (`Santorini.Engine.Modern`).
- **oldbot** — `legacy-player`: the *original course submission*, repaired —
  the same zipper MCTS made negamax-correct and protocol-compliant
  (`Santorini.Legacy.*`).

## Layout

```
src/Santorini/
  Core.hs            modern game model: rules, IntMap board, self-contained RNG, sampler + rollout
  Protocol.hs        line-delimited JSON <-> GameState, placement handshake, the player I/O loop
  Engine/Modern.hs   negamax MCTS + MCTS-Solver + heavy rollouts
  Search.hs          time-bounded shared-tree WU-UCT-style rollout driver
  Referee.hs         spawns two players, validates moves, enforces a time limit, logs every step
  Legacy/
    Game.hs          repaired original engine: game model + negamax rollout (own HashMap model)
    MCTS.hs          repaired original zipper MCTS + anytime API
app/
  ModernPlayer.hs    -> modern-player   executable
  LegacyPlayer.hs    -> legacy-player   executable (the repaired original bot)
  Referee.hs         -> referee         executable
  Oracle.hs          -> santorini-oracle development rules oracle
alphazero/
  src/santorini_az/  Python rules, network, batched PUCT, self-play, trainer
  tests/             Python tests, including Haskell/Python differential tests
```

## Build

```
cabal build          # one project, four executables
```

Everything ships with GHC (`base`, `containers`, `aeson`, `bytestring`,
`process`, `filepath`, `time`, `unix`, `mtl`, `random`, `unordered-containers`),
so it builds offline.

## AlphaZero trainer

See [`alphazero/README.md`](alphazero/README.md) for the full architecture,
training, benchmarking, evaluation, and limitation notes.

The optional Python implementation uses the same no-god-powers rules through a
tested port of `Santorini.Core`. Its policy has 1,800 fixed action slots (origin
cell, move direction, build direction/no-build), masked to legal actions. The
network is a small 5x5 residual policy/value model, and self-play batches one
leaf from each active game into every neural evaluation.

Create an isolated environment and install the package:

```
cd alphazero
python3 -m venv .venv
python3 -m pip --python .venv/bin/python install -e .
```

Run all Python tests, including the differential test against the Haskell legal
successor oracle:

```
cabal build santorini-oracle          # from santorini/
cd alphazero
.venv/bin/python -m unittest discover -s tests -v
```

Run a small end-to-end training smoke test:

```
.venv/bin/santorini-az-train \
  --iterations 1 --games 8 --parallel-games 8 \
  --simulations 16 --train-steps 4 --batch-size 32
```

Measure actual neural leaf throughput before choosing self-play concurrency:

```
.venv/bin/santorini-az-benchmark
```

The machine-tuned defaults use 64 simulations, 512 concurrent games, a training
batch of 1,024, and a replay capacity of 500,000 positions. The default
checkpoint is `checkpoints/latest.pt`.

Measured on this repository's Ryzen 5 5600X and RTX 3060 Ti:

- The 466,201-parameter default network evaluates about 124,000 positions/s at
  batch 512 and saturates near 127,000 positions/s at batch 1,024.
- Complete self-play at batch 512 produces about 61,000 games/hour with 16 MCTS
  simulations. A 64-simulation run should be expected to be roughly four times
  slower before optimizer work.
- Training batch 1,024 has better sample throughput than 256; batch 2,048 gives
  no material additional gain.

The trained model can speak the existing referee protocol directly:

```
SANTORINI_TIME_MS=1000 alphazero/.venv/bin/santorini-az-player \
  alphazero/checkpoints/latest.pt
```

The protocol player searches to 95% of `SANTORINI_TIME_MS`, leaving time to
encode and flush its response. Set `SANTORINI_AZ_SIMS` or pass `--simulations`
to use a fixed simulation count instead.

For the referee, which expects a no-argument executable, use
`alphazero/play-checkpoint.sh`. Override its model with
`SANTORINI_AZ_CHECKPOINT=/path/to/model.pt`.

Self-play and the protocol player randomize legal worker placements by default.
Set `SANTORINI_AZ_RANDOM_PLACEMENT=0` to recover the deterministic Haskell
placement preference, and pass `--seed N` to reproduce protocol-player choices.

## The protocol

Each message is one line of JSON. A **board** is

```json
{"turn": 7,
 "players": [[[2,3],[4,4]], [[3,5],[2,5]]],
 "spaces":  [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}
```

with 1-based `[row,col]` coordinates and levels `0..4` (`4` = dome). The first
player listed is the one to move. **Placement handshake:** the first player
receives `[]` and returns `[[w1,w2]]`; the second receives `[[opp1,opp2]]` and
returns `[[opp1,opp2],[own1,own2]]`. A player must flush stdout after each line.

A player program takes **no command-line arguments**. Each move is budgeted by
*time*, not step count: the per-move think budget (milliseconds) is read from the
`SANTORINI_TIME_MS` environment variable (default 1000). Both players run an
anytime search to that deadline across **all cores** (built with `-threaded
-with-rtsopts=-N`). The modern player has one persistent logical tree: a
coordinator performs selection and backup, and worker threads evaluate rollouts
in parallel. Set `SANTORINI_WORKERS=6` to control only modern rollout workers or
`GHCRTS=-N6` to cap all Haskell execution.

## Run a match

```
referee <playerA> <playerB> [nGames] [thinkMs] [logFile] [--boards <file>]
```

`nGames` plays alternate who moves first (colour balanced). `thinkMs` is the
per-move think budget handed to each player (via `SANTORINI_TIME_MS`); the
referee enforces a **hard limit of `2*thinkMs + 1000` ms** per move and a
too-slow player loses. It prints the full transcript (with each move's elapsed
time) and writes it to `logFile` (default `referee-log.txt`).

With `--boards <file>` it also writes the **full board state after every ply**
as JSONL — one `{"game":G,"ply":P,"board":{turn,players,spaces}}` per line
(ply 0 is the initial post-placement position). This lets any later analysis
read positions directly, with no move-replay needed.

Convenience launcher (builds the project and resolves the binaries for you):

```
./play.sh                                # modern vs oldbot, 4 games, 1000ms/move
./play.sh modern oldbot 10 2000          # 10 games, 2s/move
./play.sh modern oldbot 10 1000 log b.jsonl   # also record per-ply boards to b.jsonl
./play.sh modern <path-to-bot>           # modern vs any protocol-compliant bot
GHCRTS=-N4 ./play.sh                      # cap each player to 4 cores
```

Because the referee just spawns two protocol-speaking executables, it can drive
**any** compliant bot — pass a path in place of `modern`/`oldbot`.

## Transcript

The referee logs, per game: both players' worker placements, then every turn as
`move (from) -> (to)  build (cell)` (or `winning step, no build`) with the move's
elapsed wall-clock time, the winner, the ply count, and the reason. Example:

```
Game 1  (first move: side A)
    place     modern-player    (A)  (2,2) (4,4)
    place     legacy-player    (B)  (2,4) (4,2)
    turn   1  modern-player    (A)  move (4,4) -> (5,5)   build (4,4)                  571ms
    ...
    turn  17  modern-player    (A)  move (4,5) -> (5,4)   (winning step, no build)       1ms   <== WIN
  -> side A wins | 17 plies | modern-player (A) reached level 3
```

## Engines

- **modern** (`Santorini.Engine.Modern`): recursive negamax UCT (one negation
  per ply), an exact MCTS-Solver (proves forced wins/losses and short-circuits),
  positional rule-guided rollouts. Its WU-UCT-style shared-tree driver counts
  outstanding rollouts during selection, but updates values only when those
  rollouts complete. One coordinator owns the immutable tree while pure rollout
  workers run in parallel, so there is no shared mutation. The selected subtree
  is retained across protocol turns. A decaying positional prior orders early
  expansion, while completed rollout values dominate as visits accumulate. Root
  choice preserves exact proofs and rejects moves that allow an immediate
  winning reply when a safe move exists. Variants: `base` (plain negamax UCT),
  `enh` (solver + heavy + prior, the default player), `enh-unbiased` (shared-tree
  ablation without the prior), `heavy`, `solver`. Set `SANTORINI_ENGINE` to a
  variant name.
- **oldbot** (`Santorini.Legacy.*`): the original course submission's zipper UCT,
  repaired to be adversarial (negamax backup, `1 - reward/visits` exploitation,
  terminal-root guard). It keeps its own game model, so it shares no engine code
  with `modern` — the referee just drives it as another protocol executable. Like
  `modern`, it is time-bounded and root-parallel (its own forkIO ensemble, one
  search tree per core).

### Tactics note

Plain rollout MCTS is tactically weak at high branching: a uniform rollout may
almost never make the opponent take its single winning reply, so a losing move
goes unpunished. The modern engine now rejects any root move that permits an
immediate winning reply when a safe move exists. The **MCTS-Solver** handles the
deeper case by propagating exact forced wins and losses through the tree; the
parallel combiner retains those proofs across all worker roots.

The trade-off: `oldbot` is the *faster* searcher — its uniform rollouts are cheap,
so it runs many more simulations per move, while `modern` pays for the solver and
heavy rollouts. Modern's edge is per-simulation quality, not raw throughput.
The latest shared-tree smoke matches scored 3–1 with six rollout workers and 4–0
with twelve at 200 ms/move. These are small stochastic comparisons, not strength
ratings. Both engines stop dispatching work at the per-move deadline; modern then
drains at most one outstanding rollout per worker before choosing its move.

On the Ryzen 5 5600X, the shared tree completed about 267, 1,196, and 1,537
guided rollouts/s with 1, 6, and 12 workers. Twelve independent root trees
completed about 1,736 rollouts/s, so sharing one coherent tree costs roughly 11%
of raw throughput. In a fixed-seed 11-game run against the committed AlphaZero
checkpoint, both `enh` and `enh-unbiased` scored 3–8. A direct 20-game ablation at
200 ms scored 11–9 for the positional prior. Treat these as regression fixtures,
not statistically reliable ratings.
