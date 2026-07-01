from __future__ import annotations

import argparse
from pathlib import Path
import time

import numpy as np

from .game import protocol_opening
from .mcts import BatchedMCTS, SearchConfig
from .network import (
    NetworkConfig,
    PolicyValueNet,
    TorchEvaluator,
    default_device,
    load_checkpoint,
)


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Benchmark batched neural leaf evaluation")
    result.add_argument("--device", default="auto")
    result.add_argument("--channels", type=int, default=64)
    result.add_argument("--blocks", type=int, default=6)
    result.add_argument("--iterations", type=int, default=100)
    result.add_argument(
        "--batches", type=int, nargs="+", default=[1, 8, 32, 64, 128, 256]
    )
    result.add_argument("--checkpoint", type=Path)
    result.add_argument("--search-seconds", type=float, default=0.0)
    result.add_argument(
        "--search-batches", type=int, nargs="+", default=[1, 8, 32, 64, 128, 256]
    )
    return result


def main() -> None:
    args = parser().parse_args()
    device = default_device(args.device)
    if args.checkpoint is None:
        model = PolicyValueNet(NetworkConfig(args.channels, args.blocks)).to(device)
    else:
        model, _ = load_checkpoint(args.checkpoint, device)
    evaluator = TorchEvaluator(model, device)
    state = protocol_opening()
    parameters = sum(parameter.numel() for parameter in model.parameters())
    print(f"device={device} parameters={parameters:,}", flush=True)

    for batch_size in args.batches:
        states = [state] * batch_size
        for _ in range(5):
            evaluator.evaluate(states)
        start = time.perf_counter()
        for _ in range(args.iterations):
            evaluator.evaluate(states)
        elapsed = time.perf_counter() - start
        evaluations = args.iterations * batch_size
        print(
            f"batch={batch_size:4d} evaluations_per_second={evaluations / elapsed:10.1f} "
            f"batch_ms={elapsed * 1000 / args.iterations:8.3f}",
            flush=True,
        )

    if args.search_seconds > 0.0:
        print("single_root_search", flush=True)
        for batch_size in args.search_batches:
            search = BatchedMCTS(
                evaluator,
                SearchConfig(
                    simulations=1_000_000,
                    inference_batch_size=batch_size,
                    dirichlet_fraction=0.0,
                ),
                np.random.default_rng(1),
            )
            start = time.perf_counter()
            root = search.search(
                [state], False, time_limit_seconds=args.search_seconds
            )[0]
            elapsed = time.perf_counter() - start
            visits = int(np.sum(root.visits))
            print(
                f"batch={batch_size:4d} visits={visits:7d} "
                f"visits_per_second={visits / elapsed:10.1f} "
                f"elapsed={elapsed:8.3f}",
                flush=True,
            )


if __name__ == "__main__":
    main()
