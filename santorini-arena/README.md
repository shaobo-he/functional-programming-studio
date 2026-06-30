# santorini-arena

The modern Santorini MCTS engine plus a **referee**, all speaking the [Utah
CS6963 Santorini protocol](https://users.cs.utah.edu/~mflatt/cs6963/s19/santorini.html).
The referee pits two protocol-speaking player programs against each other and
records every step of the game.

The "two versions" that play each other are:

- **modern** — this project's `modern-player` (negamax MCTS + MCTS-Solver +
  heavy rollouts).
- **oldbot** — the *original course submission*, repaired and living in
  `../santorini` (`santorini-bot`): the same zipper MCTS, fixed to be
  negamax-correct and protocol-compliant.

## Layout

```
src/Santorini/
  Core.hs            game model: rules, IntMap board, self-contained RNG, one move sampler + rollout
  Protocol.hs        line-delimited JSON <-> GameState, placement handshake, the player I/O loop
  Engine/Modern.hs   negamax MCTS + MCTS-Solver + heavy rollouts + pure root-parallel ensemble
  Referee.hs         spawns two players, mediates the protocol, validates moves, logs every step
app/
  ModernPlayer.hs    `modern-player` executable  (engine: enh)
  Referee.hs         `referee` executable
```

## Build

```
cabal build
```

(Only `base`, `containers`, `aeson`, `bytestring`, `process`, `filepath` — all
ship with GHC, so it builds offline.)

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
-with-rtsopts=-N`; one worker tree per capability, combined by summed root-child
visits — ~9× wall-clock speedup on a 12-core box).

## Run a match

```
referee <playerA> <playerB> [nGames] [thinkMs] [logFile]
```

`nGames` plays alternate who moves first (colour balanced). `thinkMs` is the
per-move think budget handed to each player (via `SANTORINI_TIME_MS`); the
referee enforces a **hard limit of `2*thinkMs + 1000` ms** per move and a
too-slow player loses. It prints the full transcript (with each move's elapsed
time) and writes it to `logFile` (default `referee-log.txt`).

Convenience launcher (builds both projects and resolves the binaries for you):

```
./play.sh                          # modern vs oldbot, 4 games, 1000ms/move
./play.sh modern oldbot 10 2000    # 10 games, 2s/move
./play.sh modern <path-to-bot>     # modern vs any protocol-compliant bot
```

Because the referee just spawns two protocol-speaking executables, it can drive
**any** compliant bot. `oldbot` resolves to the repaired original submission in
`../santorini` (`santorini-bot`), which is also time-bounded and multi-core.

## Transcript

The referee logs, per game: both players' worker placements, then every turn as
`move (from) -> (to)  build (cell)` (or `winning step, no build`) with the move's
elapsed wall-clock time, the winner, the ply count, and the reason. Example:

```
Game 1  (first move: side A)
    place     modern-player    (A)  (2,2) (4,4)
    place     santorini-bot    (B)  (2,4) (4,2)
    turn   1  modern-player    (A)  move (4,4) -> (5,5)   build (4,4)                  571ms
    ...
    turn  17  modern-player    (A)  move (4,5) -> (5,4)   (winning step, no build)       1ms   <== WIN
  -> side A wins | 17 plies | modern-player (A) reached level 3
```

## Engines

- **modern** (`Engine.Modern`): recursive negamax UCT (one negation per ply),
  an exact MCTS-Solver (proves forced wins/losses and short-circuits), heavy
  rule-guided rollouts, and a pure root-parallel ensemble. Variants: `base`
  (plain negamax UCT), `enh` (solver + heavy, the default player), `heavy`,
  `solver`.
- **oldbot** (`../santorini`): the original course submission's zipper UCT,
  repaired to be adversarial (negamax backup, `1 - reward/visits` exploitation,
  terminal-root guard). A separate, self-contained protocol bot, so the referee
  drives it as just another executable.

### Tactics note

Plain rollout MCTS (the `base` variant and `oldbot`) is tactically blind at high
branching: a uniform rollout almost never makes the opponent take its single
winning reply, so a losing move goes unpunished. The **MCTS-Solver** fixes this
— in a constructed must-defend position, `solver`/`enh` defend 20/20 where
`base`/`oldbot` defend ~0/20. That (not a logic difference in the negamax) is the
main reason `modern` beats `oldbot`.
