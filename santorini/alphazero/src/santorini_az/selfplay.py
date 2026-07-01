from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable

import numpy as np

from .game import (
    State,
    apply_action,
    protocol_opening,
    random_opening,
    terminal_value,
    transform_sparse_policy,
    transform_state,
)
from .mcts import BatchedMCTS, Node, choose_action, visit_policy


@dataclass(frozen=True, slots=True)
class Example:
    state: State
    actions: np.ndarray
    probabilities: np.ndarray
    value: float


class ReplayBuffer:
    def __init__(self, capacity: int) -> None:
        self.capacity = capacity
        self.examples: list[Example] = []
        self.position = 0

    def extend(self, examples: Iterable[Example]) -> None:
        for example in examples:
            if len(self.examples) < self.capacity:
                self.examples.append(example)
            else:
                self.examples[self.position] = example
            self.position = (self.position + 1) % self.capacity

    def sample(self, count: int, rng: np.random.Generator) -> list[Example]:
        if count > len(self.examples):
            raise ValueError("sample larger than replay buffer")
        indices = rng.choice(len(self.examples), size=count, replace=False)
        return [self.examples[int(index)] for index in indices]

    def __len__(self) -> int:
        return len(self.examples)


@dataclass(frozen=True, slots=True)
class SelfPlayConfig:
    games_per_batch: int = 8
    temperature_moves: int = 12
    random_placements: bool = True


class SelfPlayRunner:
    def __init__(
        self,
        search: BatchedMCTS,
        config: SelfPlayConfig,
        rng: np.random.Generator,
    ) -> None:
        self.search = search
        self.config = config
        self.rng = rng

    def play_batch(self, games: int | None = None) -> list[Example]:
        count = games or self.config.games_per_batch
        states = [self._opening() for _ in range(count)]
        trajectories: list[list[tuple[State, np.ndarray, np.ndarray]]] = [
            [] for _ in range(count)
        ]
        cached_roots: list[Node | None] = [None] * count
        active = list(range(count))
        completed: list[Example] = []

        while active:
            roots = self.search.search(
                [states[index] for index in active],
                True,
                reusable_roots=[cached_roots[index] for index in active],
            )
            next_active: list[int] = []
            for game_index, root in zip(active, roots, strict=True):
                state = states[game_index]
                temperature = (
                    1.0
                    if len(trajectories[game_index]) < self.config.temperature_moves
                    else 0.0
                )
                actions, probabilities = visit_policy(root, temperature)
                action = choose_action(root, self.rng, temperature)
                edge = int(np.flatnonzero(root.actions == action)[0])
                cached_roots[game_index] = root.child(edge)
                trajectories[game_index].append((state, actions, probabilities))
                next_state = apply_action(state, action, validate=False)
                states[game_index] = next_state
                outcome = terminal_value(next_state)
                if outcome is None:
                    next_active.append(game_index)
                else:
                    value = outcome
                    for old_state, old_actions, old_probabilities in reversed(
                        trajectories[game_index]
                    ):
                        value = -value
                        completed.append(
                            Example(old_state, old_actions, old_probabilities, value)
                        )
            active = next_active
        return completed

    def _opening(self) -> State:
        if self.config.random_placements:
            return random_opening(self.rng)
        return protocol_opening()


def augment_example(example: Example, symmetry: int) -> Example:
    actions, probabilities = transform_sparse_policy(
        example.actions, example.probabilities, symmetry
    )
    return Example(
        transform_state(example.state, symmetry),
        actions,
        probabilities,
        example.value,
    )
