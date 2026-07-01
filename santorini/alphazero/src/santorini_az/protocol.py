from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Sequence

import numpy as np

from .game import State, apply_action
from .mcts import BatchedMCTS, SearchConfig, choose_action
from .network import TorchEvaluator, default_device, load_checkpoint

PLACEMENT_PREFERENCE = (
    (2, 2),
    (4, 4),
    (2, 4),
    (4, 2),
    (3, 3),
) + tuple((row, col) for row in range(1, 6) for col in range(1, 6))


def place_workers(
    occupied: Sequence[Sequence[int]], rng: np.random.Generator | None = None
) -> list[list[int]]:
    occupied_cells = {tuple(worker) for worker in occupied}
    if rng is not None:
        available = [
            [row, col]
            for row in range(1, 6)
            for col in range(1, 6)
            if (row, col) not in occupied_cells
        ]
        indices = rng.choice(len(available), size=2, replace=False)
        return [available[int(index)] for index in indices]

    available: list[list[int]] = []
    for candidate in PLACEMENT_PREFERENCE:
        if candidate not in occupied_cells and list(candidate) not in available:
            available.append(list(candidate))
        if len(available) == 2:
            return available
    raise ValueError("not enough cells for placement")


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Protocol-speaking AlphaZero player")
    result.add_argument("checkpoint", type=Path)
    result.add_argument("--device", default="auto")
    fixed_simulations = os.getenv("SANTORINI_AZ_SIMS")
    result.add_argument(
        "--simulations",
        type=int,
        default=None if fixed_simulations is None else int(fixed_simulations),
        help="fixed search size; overrides the per-turn time budget",
    )
    result.add_argument(
        "--time-ms",
        type=int,
        default=int(os.getenv("SANTORINI_TIME_MS", "1000")),
    )
    result.add_argument("--seed", type=int)
    return result


def main() -> None:
    args = parser().parse_args()
    device = default_device(args.device)
    model, _ = load_checkpoint(args.checkpoint, device)
    rng = np.random.default_rng(args.seed)
    random_placement = os.getenv("SANTORINI_AZ_RANDOM_PLACEMENT", "1") != "0"
    search = BatchedMCTS(
        TorchEvaluator(model, device),
        SearchConfig(
            simulations=args.simulations or 1_000_000,
            dirichlet_fraction=0.0,
        ),
        rng,
    )
    time_limit = None if args.simulations is not None else max(1, args.time_ms) * 0.95 / 1000

    for line in sys.stdin:
        try:
            message = json.loads(line)
            if isinstance(message, list):
                occupied = message[0] if message else []
                ours = place_workers(occupied, rng if random_placement else None)
                response = [ours] if not message else [message[0], ours]
            else:
                state = State.from_json(message)
                root = search.search([state], False, time_limit)[0]
                action = choose_action(root, rng, 0.0)
                response = apply_action(state, action).to_json()
            print(json.dumps(response, separators=(",", ":")), flush=True)
        except (KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
            print(json.dumps({"error": str(error)}, separators=(",", ":")), flush=True)


if __name__ == "__main__":
    main()
