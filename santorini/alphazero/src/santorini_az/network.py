from __future__ import annotations

from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Sequence

import numpy as np
import torch
from torch import nn

from .game import ACTION_PLANES, ACTION_SIZE, BOARD_SIZE, INPUT_CHANNELS, State
from .mcts import encoded_batch


@dataclass(frozen=True, slots=True)
class NetworkConfig:
    channels: int = 64
    blocks: int = 6
    value_hidden: int = 64


class ResidualBlock(nn.Module):
    def __init__(self, channels: int) -> None:
        super().__init__()
        self.conv1 = nn.Conv2d(channels, channels, 3, padding=1, bias=False)
        self.bn1 = nn.BatchNorm2d(channels)
        self.conv2 = nn.Conv2d(channels, channels, 3, padding=1, bias=False)
        self.bn2 = nn.BatchNorm2d(channels)
        self.relu = nn.ReLU(inplace=True)

    def forward(self, inputs: torch.Tensor) -> torch.Tensor:
        residual = inputs
        outputs = self.relu(self.bn1(self.conv1(inputs)))
        outputs = self.bn2(self.conv2(outputs))
        return self.relu(outputs + residual)


class PolicyValueNet(nn.Module):
    def __init__(self, config: NetworkConfig = NetworkConfig()) -> None:
        super().__init__()
        self.config = config
        self.stem = nn.Sequential(
            nn.Conv2d(INPUT_CHANNELS, config.channels, 3, padding=1, bias=False),
            nn.BatchNorm2d(config.channels),
            nn.ReLU(inplace=True),
        )
        self.blocks = nn.Sequential(
            *(ResidualBlock(config.channels) for _ in range(config.blocks))
        )
        self.policy = nn.Conv2d(config.channels, ACTION_PLANES, 1)
        self.value = nn.Sequential(
            nn.Conv2d(config.channels, 8, 1, bias=False),
            nn.BatchNorm2d(8),
            nn.ReLU(inplace=True),
            nn.Flatten(),
            nn.Linear(8 * BOARD_SIZE * BOARD_SIZE, config.value_hidden),
            nn.ReLU(inplace=True),
            nn.Linear(config.value_hidden, 1),
            nn.Tanh(),
        )

    def forward(self, inputs: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        trunk = self.blocks(self.stem(inputs))
        policy = self.policy(trunk)
        policy = policy.permute(0, 2, 3, 1).reshape(-1, ACTION_SIZE)
        value = self.value(trunk).squeeze(1)
        return policy, value


class TorchEvaluator:
    def __init__(self, model: PolicyValueNet, device: torch.device) -> None:
        self.model = model.to(device)
        self.device = device

    def evaluate(self, states: Sequence[State]) -> tuple[np.ndarray, np.ndarray]:
        if not states:
            return (
                np.empty((0, ACTION_SIZE), dtype=np.float32),
                np.empty(0, dtype=np.float32),
            )
        self.model.eval()
        inputs = torch.from_numpy(encoded_batch(states)).to(self.device)
        with torch.inference_mode(), torch.autocast(
            device_type=self.device.type,
            dtype=torch.float16,
            enabled=self.device.type == "cuda",
        ):
            logits, values = self.model(inputs)
        return (
            logits.float().cpu().numpy(),
            values.float().cpu().numpy(),
        )


def default_device(requested: str = "auto") -> torch.device:
    if requested == "auto":
        return torch.device("cuda" if torch.cuda.is_available() else "cpu")
    return torch.device(requested)


def save_checkpoint(
    path: Path,
    model: PolicyValueNet,
    optimizer: torch.optim.Optimizer | None = None,
    step: int = 0,
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "network_config": asdict(model.config),
        "model": model.state_dict(),
        "step": step,
    }
    if optimizer is not None:
        payload["optimizer"] = optimizer.state_dict()
    torch.save(payload, path)


def load_checkpoint(
    path: Path, device: torch.device
) -> tuple[PolicyValueNet, dict[str, object]]:
    payload = torch.load(path, map_location=device, weights_only=False)
    config = NetworkConfig(**payload["network_config"])
    model = PolicyValueNet(config).to(device)
    model.load_state_dict(payload["model"])
    return model, payload
