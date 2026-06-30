#!/usr/bin/env bash
# Build the arena and run a referee match between two bots.
#
#   ./play.sh [A] [B] [nGames] [thinkMs] [logFile]
#
# A and B may be:  modern | oldbot | <path-to-any-protocol-bot>
#   modern -> the modern engine player (this project)
#   oldbot -> the repaired original course bot in ../santorini (santorini-bot)
# thinkMs -> per-move think budget given to each player (default 1000)
# Defaults: modern vs oldbot, 4 games, 1000ms/move, referee-log.txt
set -euo pipefail
cd "$(dirname "$0")"

cabal build -v0

MP=$(cabal list-bin modern-player)
REF=$(cabal list-bin referee)

resolve () {
  case "$1" in
    modern) echo "$MP" ;;
    oldbot) ( cd ../santorini && cabal build -v0 && cabal list-bin santorini-bot ) ;;
    *)      echo "$1" ;;   # treat as an explicit path
  esac
}

A=$(resolve "${1:-modern}")
B=$(resolve "${2:-oldbot}")
N="${3:-4}"
MS="${4:-1000}"
LOG="${5:-referee-log.txt}"

echo "A = $A"
echo "B = $B"
"$REF" "$A" "$B" "$N" "$MS" "$LOG"
