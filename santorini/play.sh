#!/usr/bin/env bash
# Build the project and run a referee match between two bots.
#
#   ./play.sh [A] [B] [nGames] [thinkMs] [logFile] [boardsFile]
#
# A and B may be:  modern | oldbot | <path-to-any-protocol-bot>
#   modern -> the modern engine player (modern-player)
#   oldbot -> the repaired original course bot (legacy-player)
# thinkMs    -> per-move think budget given to each player (default 1000)
# boardsFile -> if given, also write the full board state per ply as JSONL
# Defaults: modern vs oldbot, 4 games, 1000ms/move, referee-log.txt
set -euo pipefail
cd "$(dirname "$0")"

cabal build -v0

MP=$(cabal list-bin modern-player)
LP=$(cabal list-bin legacy-player)
REF=$(cabal list-bin referee)

resolve () {
  case "$1" in
    modern)        echo "$MP" ;;
    oldbot|legacy) echo "$LP" ;;
    *)             echo "$1" ;;   # treat as an explicit path
  esac
}

A=$(resolve "${1:-modern}")
B=$(resolve "${2:-oldbot}")
N="${3:-4}"
MS="${4:-1000}"
LOG="${5:-referee-log.txt}"
BOARDS="${6:-}"

echo "A = $A"
echo "B = $B"
if [ -n "$BOARDS" ]; then
  "$REF" "$A" "$B" "$N" "$MS" "$LOG" --boards "$BOARDS"
else
  "$REF" "$A" "$B" "$N" "$MS" "$LOG"
fi
