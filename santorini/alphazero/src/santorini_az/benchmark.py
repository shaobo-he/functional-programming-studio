from __future__ import annotations

import argparse
import time

from .game import protocol_opening
from .network import NetworkConfig, PolicyValueNet, TorchEvaluator, default_device


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Benchmark batched neural leaf evaluation")
    result.add_argument("--device", default="auto")
    result.add_argument("--channels", type=int, default=64)
    result.add_argument("--blocks", type=int, default=6)
    result.add_argument("--iterations", type=int, default=100)
    result.add_argument(
        "--batches", type=int, nargs="+", default=[1, 8, 32, 64, 128, 256]
    )
    return result


def main() -> None:
    args = parser().parse_args()
    device = default_device(args.device)
    model = PolicyValueNet(NetworkConfig(args.channels, args.blocks)).to(device)
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


if __name__ == "__main__":
    main()
