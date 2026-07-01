from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import torch
from torch import nn

from .game import ACTION_SIZE, input_planes
from .mcts import BatchedMCTS, SearchConfig
from .network import (
    NetworkConfig,
    PolicyValueNet,
    TorchEvaluator,
    default_device,
    load_checkpoint,
    save_checkpoint,
)
from .selfplay import ReplayBuffer, SelfPlayConfig, SelfPlayRunner, augment_example


def train_batch(
    model: PolicyValueNet,
    optimizer: torch.optim.Optimizer,
    replay: ReplayBuffer,
    batch_size: int,
    device: torch.device,
    rng: np.random.Generator,
) -> tuple[float, float]:
    examples = [
        augment_example(example, int(rng.integers(8)))
        for example in replay.sample(batch_size, rng)
    ]
    inputs = np.stack([input_planes(example.state) for example in examples])
    target_policy = np.zeros((batch_size, ACTION_SIZE), dtype=np.float32)
    for row, example in enumerate(examples):
        target_policy[row, example.actions] = example.probabilities
    target_value = np.asarray([example.value for example in examples], dtype=np.float32)

    input_tensor = torch.from_numpy(inputs).to(device)
    policy_tensor = torch.from_numpy(target_policy).to(device)
    value_tensor = torch.from_numpy(target_value).to(device)

    model.train()
    optimizer.zero_grad(set_to_none=True)
    logits, values = model(input_tensor)
    policy_loss = -(policy_tensor * nn.functional.log_softmax(logits, dim=1)).sum(1).mean()
    value_loss = nn.functional.mse_loss(values, value_tensor)
    (policy_loss + value_loss).backward()
    optimizer.step()
    return float(policy_loss.detach()), float(value_loss.detach())


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Train Santorini by AlphaZero self-play")
    result.add_argument("--checkpoint", type=Path, default=Path("checkpoints/latest.pt"))
    result.add_argument("--device", default="auto")
    result.add_argument("--iterations", type=int, default=100)
    result.add_argument("--games", type=int, default=512)
    result.add_argument("--parallel-games", type=int, default=512)
    result.add_argument("--simulations", type=int, default=64)
    result.add_argument("--train-steps", type=int, default=128)
    result.add_argument("--batch-size", type=int, default=1024)
    result.add_argument("--replay-capacity", type=int, default=500_000)
    result.add_argument("--channels", type=int, default=64)
    result.add_argument("--blocks", type=int, default=6)
    result.add_argument("--learning-rate", type=float, default=2e-3)
    result.add_argument("--seed", type=int, default=20260630)
    result.add_argument("--fixed-placement", action="store_true")
    return result


def main() -> None:
    args = parser().parse_args()
    rng = np.random.default_rng(args.seed)
    torch.manual_seed(args.seed)
    device = default_device(args.device)

    if args.checkpoint.exists():
        model, payload = load_checkpoint(args.checkpoint, device)
        start_iteration = int(payload.get("step", 0))
    else:
        model = PolicyValueNet(NetworkConfig(args.channels, args.blocks)).to(device)
        payload = {}
        start_iteration = 0
    optimizer = torch.optim.AdamW(model.parameters(), lr=args.learning_rate, weight_decay=1e-4)
    if "optimizer" in payload:
        optimizer.load_state_dict(payload["optimizer"])

    evaluator = TorchEvaluator(model, device)
    search = BatchedMCTS(evaluator, SearchConfig(simulations=args.simulations), rng)
    selfplay = SelfPlayRunner(
        search,
        SelfPlayConfig(
            games_per_batch=args.parallel_games,
            random_placements=not args.fixed_placement,
        ),
        rng,
    )
    replay = ReplayBuffer(args.replay_capacity)

    print(
        f"device={device} parameters={sum(p.numel() for p in model.parameters()):,}",
        flush=True,
    )
    for iteration in range(start_iteration + 1, start_iteration + args.iterations + 1):
        remaining = args.games
        while remaining > 0:
            batch_games = min(remaining, args.parallel_games)
            replay.extend(selfplay.play_batch(batch_games))
            remaining -= batch_games

        losses: list[tuple[float, float]] = []
        if len(replay) >= args.batch_size:
            for _ in range(args.train_steps):
                losses.append(
                    train_batch(model, optimizer, replay, args.batch_size, device, rng)
                )
        policy_loss = np.mean([loss[0] for loss in losses]) if losses else float("nan")
        value_loss = np.mean([loss[1] for loss in losses]) if losses else float("nan")
        save_checkpoint(args.checkpoint, model, optimizer, iteration)
        print(
            f"iteration={iteration} replay={len(replay)} "
            f"policy_loss={policy_loss:.4f} value_loss={value_loss:.4f}",
            flush=True,
        )


if __name__ == "__main__":
    main()
