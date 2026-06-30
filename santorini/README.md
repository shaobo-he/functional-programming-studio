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
  Search.hs          time-bounded, root-parallel (multi-core) anytime driver
  Referee.hs         spawns two players, validates moves, enforces a time limit, logs every step
  Legacy/
    Game.hs          repaired original engine: game model + negamax rollout (own HashMap model)
    MCTS.hs          repaired original zipper MCTS + anytime API
app/
  ModernPlayer.hs    -> modern-player   executable
  LegacyPlayer.hs    -> legacy-player   executable (the repaired original bot)
  Referee.hs         -> referee         executable
```

## Build

```
cabal build          # one project, three executables
```

Everything ships with GHC (`base`, `containers`, `aeson`, `bytestring`,
`process`, `filepath`, `time`, `unix`, `mtl`, `random`, `unordered-containers`),
so it builds offline.

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
visits — ~9× wall-clock speedup on a 12-core box). Cap cores at runtime with
`GHCRTS=-N4`.

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
  heavy rule-guided rollouts. Variants: `base` (plain negamax UCT), `enh`
  (solver + heavy, the default player), `heavy`, `solver`.
- **oldbot** (`Santorini.Legacy.*`): the original course submission's zipper UCT,
  repaired to be adversarial (negamax backup, `1 - reward/visits` exploitation,
  terminal-root guard). It keeps its own game model, so it shares no engine code
  with `modern` — the referee just drives it as another protocol executable. Like
  `modern`, it is time-bounded and root-parallel (its own forkIO ensemble, one
  search tree per core).

### Tactics note

Plain rollout MCTS (the `base` variant and `oldbot`) is tactically blind at high
branching: a uniform rollout almost never makes the opponent take its single
winning reply, so a losing move goes unpunished. The **MCTS-Solver** fixes this
— in a constructed must-defend position, `solver`/`enh` defend 20/20 where
`base`/`oldbot` defend ~0/20. That (not a logic difference in the negamax) is the
main reason `modern` beats `oldbot`.
```
