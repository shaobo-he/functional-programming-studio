#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
latest="$root/checkpoints/latest.pt"
pilot="$root/checkpoints/pilot-5iter.pt"

if [[ -n "${SANTORINI_AZ_CHECKPOINT:-}" ]]; then
  checkpoint="$SANTORINI_AZ_CHECKPOINT"
elif [[ -f "$latest" ]]; then
  checkpoint="$latest"
else
  checkpoint="$pilot"
fi

exec "$root/.venv/bin/santorini-az-player" "$checkpoint" "$@"
