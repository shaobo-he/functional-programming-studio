from __future__ import annotations

import unittest

import numpy as np

from santorini_az.game import legal_actions, protocol_opening
from santorini_az.mcts import (
    BatchedMCTS,
    SearchConfig,
    UniformEvaluator,
    choose_action,
    visit_policy,
)
from santorini_az.selfplay import Example, ReplayBuffer, SelfPlayConfig, SelfPlayRunner


class MCTSTest(unittest.TestCase):
    def setUp(self) -> None:
        self.rng = np.random.default_rng(17)
        self.search = BatchedMCTS(
            UniformEvaluator(), SearchConfig(simulations=16), self.rng
        )

    def test_search_visits_only_legal_root_actions(self) -> None:
        state = protocol_opening()
        root = self.search.search([state], False)[0]
        self.assertEqual(set(root.actions), set(legal_actions(state)))
        self.assertEqual(int(np.sum(root.visits)), 16)
        actions, probabilities = visit_policy(root)
        self.assertEqual(set(actions), set(legal_actions(state)))
        self.assertAlmostEqual(float(np.sum(probabilities)), 1.0)
        self.assertIn(choose_action(root, self.rng), legal_actions(state))

    def test_multiple_roots_share_an_evaluation_batch(self) -> None:
        roots = self.search.search([protocol_opening(), protocol_opening()], False)
        self.assertEqual(len(roots), 2)
        self.assertTrue(all(int(np.sum(root.visits)) == 16 for root in roots))

    def test_time_limit_stops_before_simulation_cap(self) -> None:
        search = BatchedMCTS(
            UniformEvaluator(), SearchConfig(simulations=1_000_000), self.rng
        )
        root = search.search([protocol_opening()], False, 0.01)[0]
        self.assertGreater(int(np.sum(root.visits)), 0)
        self.assertLess(int(np.sum(root.visits)), 1_000_000)

    def test_self_play_emits_normalized_sparse_targets(self) -> None:
        runner = SelfPlayRunner(
            BatchedMCTS(
                UniformEvaluator(), SearchConfig(simulations=2), self.rng
            ),
            SelfPlayConfig(games_per_batch=1, temperature_moves=4),
            self.rng,
        )
        examples = runner.play_batch()
        self.assertGreater(len(examples), 0)
        for example in examples:
            self.assertIn(example.value, (-1.0, 1.0))
            self.assertEqual(len(example.actions), len(example.probabilities))
            self.assertAlmostEqual(float(np.sum(example.probabilities)), 1.0)

    def test_replay_buffer_overwrites_oldest_slots(self) -> None:
        state = protocol_opening()
        action = np.asarray([legal_actions(state)[0]], dtype=np.int32)
        probability = np.asarray([1.0], dtype=np.float32)
        replay = ReplayBuffer(3)
        replay.extend(
            Example(state, action, probability, float(value)) for value in range(5)
        )
        self.assertEqual(len(replay), 3)
        self.assertEqual({example.value for example in replay.examples}, {2.0, 3.0, 4.0})


if __name__ == "__main__":
    unittest.main()
