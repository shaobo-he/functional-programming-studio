from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path

import numpy as np

from santorini_az.game import (
    State,
    apply_action,
    legal_actions,
    protocol_opening,
    random_opening,
)

SANTORINI_ROOT = Path(__file__).resolve().parents[2]


def signature(state: State) -> tuple[object, ...]:
    return state.turn, state.players, state.board


class HaskellOracle:
    def __init__(self) -> None:
        executable = subprocess.run(
            ["cabal", "list-bin", "santorini-oracle"],
            cwd=SANTORINI_ROOT,
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()
        self.process = subprocess.Popen(
            [executable],
            cwd=SANTORINI_ROOT,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )

    def successors(self, state: State) -> list[State]:
        assert self.process.stdin is not None
        assert self.process.stdout is not None
        self.process.stdin.write(json.dumps(state.to_json(), separators=(",", ":")) + "\n")
        self.process.stdin.flush()
        line = self.process.stdout.readline()
        if not line:
            assert self.process.stderr is not None
            raise RuntimeError(self.process.stderr.read())
        return [State.from_json(value) for value in json.loads(line)]

    def close(self) -> None:
        if self.process.stdin is not None:
            self.process.stdin.close()
        self.process.terminate()
        self.process.wait(timeout=10)


class DifferentialTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.oracle = HaskellOracle()

    @classmethod
    def tearDownClass(cls) -> None:
        cls.oracle.close()

    def test_successor_sets_match_haskell_over_trajectory(self) -> None:
        rng = np.random.default_rng(20260630)
        state = protocol_opening()
        for _ in range(20):
            python_successors = [apply_action(state, action) for action in legal_actions(state)]
            haskell_successors = self.oracle.successors(state)
            self.assertEqual(
                {signature(successor) for successor in python_successors},
                {signature(successor) for successor in haskell_successors},
            )
            if not python_successors:
                break
            state = python_successors[int(rng.integers(len(python_successors)))]

    def test_random_opening_successors_match_haskell(self) -> None:
        rng = np.random.default_rng(77)
        for _ in range(12):
            state = random_opening(rng)
            python_successors = [apply_action(state, action) for action in legal_actions(state)]
            haskell_successors = self.oracle.successors(state)
            self.assertEqual(
                {signature(successor) for successor in python_successors},
                {signature(successor) for successor in haskell_successors},
            )


if __name__ == "__main__":
    unittest.main()
