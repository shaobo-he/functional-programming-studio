from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

import numpy as np

try:
    import torch
except ModuleNotFoundError:
    torch = None


@unittest.skipIf(torch is None, "PyTorch is not installed")
class NetworkTest(unittest.TestCase):
    def test_committed_pilot_checkpoint_loads(self) -> None:
        from santorini_az.network import load_checkpoint

        checkpoint = Path(__file__).resolve().parents[1] / "checkpoints/pilot-5iter.pt"
        model, payload = load_checkpoint(checkpoint, torch.device("cpu"))
        self.assertEqual(payload["step"], 5)
        self.assertEqual(sum(parameter.numel() for parameter in model.parameters()), 466_201)

    def test_forward_evaluator_and_checkpoint(self) -> None:
        from santorini_az.game import ACTION_SIZE, legal_actions, protocol_opening
        from santorini_az.network import (
            NetworkConfig,
            PolicyValueNet,
            TorchEvaluator,
            load_checkpoint,
            save_checkpoint,
        )

        config = NetworkConfig(channels=16, blocks=2, value_hidden=16)
        model = PolicyValueNet(config)
        inputs = torch.zeros((2, 7, 5, 5), dtype=torch.float32)
        logits, values = model(inputs)
        self.assertEqual(tuple(logits.shape), (2, ACTION_SIZE))
        self.assertEqual(tuple(values.shape), (2,))
        self.assertTrue(torch.all(values >= -1.0))
        self.assertTrue(torch.all(values <= 1.0))

        evaluator = TorchEvaluator(model, torch.device("cpu"))
        policy, value = evaluator.evaluate([protocol_opening()])
        self.assertEqual(policy.shape, (1, ACTION_SIZE))
        self.assertEqual(value.shape, (1,))
        self.assertTrue(np.isfinite(policy).all())

        with tempfile.TemporaryDirectory() as directory:
            checkpoint = Path(directory) / "model.pt"
            save_checkpoint(checkpoint, model, step=7)
            restored, payload = load_checkpoint(checkpoint, torch.device("cpu"))
            self.assertEqual(restored.config, config)
            self.assertEqual(payload["step"], 7)

        from santorini_az.selfplay import Example, ReplayBuffer
        from santorini_az.train import train_batch

        state = protocol_opening()
        actions = np.asarray([legal_actions(state)[0]], dtype=np.int32)
        probabilities = np.asarray([1.0], dtype=np.float32)
        replay = ReplayBuffer(4)
        replay.extend(Example(state, actions, probabilities, 1.0) for _ in range(4))
        optimizer = torch.optim.AdamW(model.parameters(), lr=1e-3)
        losses = train_batch(
            model,
            optimizer,
            replay,
            4,
            torch.device("cpu"),
            np.random.default_rng(1),
        )
        self.assertTrue(all(np.isfinite(loss) for loss in losses))


if __name__ == "__main__":
    unittest.main()
