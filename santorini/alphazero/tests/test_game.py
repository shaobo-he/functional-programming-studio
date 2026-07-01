from __future__ import annotations

import unittest

import numpy as np

from santorini_az.game import (
    ACTION_SIZE,
    State,
    apply_action,
    decode_action,
    encode_action,
    input_planes,
    legal_actions,
    mover_won,
    pos,
    protocol_opening,
    random_opening,
    terminal_value,
    transform_action,
    transform_state,
)


class GameTest(unittest.TestCase):
    def test_protocol_opening_has_expected_branching_factor(self) -> None:
        self.assertEqual(len(legal_actions(protocol_opening())), 80)

    def test_actions_round_trip_and_produce_legal_states(self) -> None:
        state = protocol_opening()
        for action in legal_actions(state):
            self.assertGreaterEqual(action, 0)
            self.assertLess(action, ACTION_SIZE)
            self.assertEqual(encode_action(*decode_action(action)), action)
            successor = apply_action(state, action)
            self.assertEqual(successor.turn, state.turn + 1)
            self.assertEqual(successor.players[0], state.players[1])
            self.assertEqual(sum(successor.board), 1)

    def test_json_round_trip(self) -> None:
        state = apply_action(protocol_opening(), legal_actions(protocol_opening())[7])
        self.assertEqual(State.from_json(state.to_json()), state)

    def test_winning_move_has_no_build_and_ends_game(self) -> None:
        board = [0] * 25
        board[pos(0, 0)] = 2
        board[pos(0, 1)] = 3
        state = State(
            12,
            ((pos(0, 0), pos(4, 4)), (pos(2, 2), pos(2, 3))),
            tuple(board),
        )
        action = encode_action(pos(0, 0), pos(0, 1), None)
        self.assertIn(action, legal_actions(state))
        successor = apply_action(state, action)
        self.assertTrue(mover_won(successor))
        self.assertEqual(terminal_value(successor), -1.0)
        self.assertEqual(sum(successor.board), sum(state.board))

    def test_all_symmetries_preserve_legal_actions(self) -> None:
        state = apply_action(protocol_opening(), legal_actions(protocol_opening())[11])
        original = legal_actions(state)
        for symmetry in range(8):
            transformed_state = transform_state(state, symmetry)
            transformed_actions = {transform_action(action, symmetry) for action in original}
            self.assertEqual(transformed_actions, set(legal_actions(transformed_state)))

    def test_input_planes_are_one_hot_and_player_relative(self) -> None:
        planes = input_planes(protocol_opening())
        self.assertEqual(planes.shape, (7, 5, 5))
        np.testing.assert_array_equal(np.sum(planes[:5], axis=0), np.ones((5, 5)))
        self.assertEqual(np.sum(planes[5]), 2)
        self.assertEqual(np.sum(planes[6]), 2)

    def test_random_opening_has_four_distinct_workers(self) -> None:
        state = random_opening(np.random.default_rng(8))
        workers = state.players[0] + state.players[1]
        self.assertEqual(len(set(workers)), 4)
        self.assertGreater(len(legal_actions(state)), 0)

    def test_stalemate_is_a_loss_for_player_to_move(self) -> None:
        board = [4] * 25
        players = ((pos(0, 0), pos(0, 1)), (pos(4, 3), pos(4, 4)))
        for worker in players[0] + players[1]:
            board[worker] = 0
        state = State(100, players, tuple(board))
        self.assertEqual(legal_actions(state), ())
        self.assertEqual(terminal_value(state), -1.0)


if __name__ == "__main__":
    unittest.main()
