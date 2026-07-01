from __future__ import annotations

from dataclasses import dataclass, field
import time
from typing import Protocol, Sequence

import numpy as np

from .game import ACTION_SIZE, State, apply_action, input_planes, legal_actions, mover_won


class Evaluator(Protocol):
    def evaluate(self, states: Sequence[State]) -> tuple[np.ndarray, np.ndarray]:
        """Return policy logits [batch, ACTION_SIZE] and values [batch]."""


class UniformEvaluator:
    """Deterministic evaluator used by tests and CPU-only smoke runs."""

    def evaluate(self, states: Sequence[State]) -> tuple[np.ndarray, np.ndarray]:
        return (
            np.zeros((len(states), ACTION_SIZE), dtype=np.float32),
            np.zeros(len(states), dtype=np.float32),
        )


@dataclass(slots=True)
class Node:
    state: State
    actions: np.ndarray = field(default_factory=lambda: np.empty(0, dtype=np.int32))
    priors: np.ndarray = field(default_factory=lambda: np.empty(0, dtype=np.float32))
    visits: np.ndarray = field(default_factory=lambda: np.empty(0, dtype=np.int32))
    pending: np.ndarray = field(default_factory=lambda: np.empty(0, dtype=np.int32))
    value_sums: np.ndarray = field(default_factory=lambda: np.empty(0, dtype=np.float32))
    children: dict[int, Node] = field(default_factory=dict)
    expanded: bool = False
    terminal: float | None = None

    def __post_init__(self) -> None:
        if mover_won(self.state):
            self.terminal = -1.0
            return
        self.actions = np.asarray(legal_actions(self.state), dtype=np.int32)
        if len(self.actions) == 0:
            self.terminal = -1.0

    def expand(self, logits: np.ndarray) -> None:
        if self.expanded or self.terminal is not None:
            return
        selected = logits[self.actions].astype(np.float64)
        selected -= np.max(selected)
        priors = np.exp(selected)
        self.priors = (priors / np.sum(priors)).astype(np.float32)
        self.visits = np.zeros(len(self.actions), dtype=np.int32)
        self.pending = np.zeros(len(self.actions), dtype=np.int32)
        self.value_sums = np.zeros(len(self.actions), dtype=np.float32)
        self.expanded = True

    def select(self, cpuct: float) -> int:
        q = np.divide(
            self.value_sums,
            self.visits,
            out=np.zeros_like(self.value_sums),
            where=self.visits != 0,
        )
        total_visits = self.visits + self.pending
        exploration = (
            cpuct
            * self.priors
            * np.sqrt(float(np.sum(total_visits)) + 1.0)
            / (1.0 + total_visits)
        )
        return int(np.argmax(q + exploration))

    def child(self, edge: int) -> Node:
        action = int(self.actions[edge])
        child = self.children.get(action)
        if child is None:
            child = Node(apply_action(self.state, action, validate=False))
            self.children[action] = child
        return child


@dataclass(frozen=True, slots=True)
class SearchConfig:
    simulations: int = 64
    inference_batch_size: int = 1
    cpuct: float = 1.5
    dirichlet_alpha: float = 0.3
    dirichlet_fraction: float = 0.25


class BatchedMCTS:
    """PUCT search with batched leaf evaluation and reusable roots."""

    def __init__(
        self,
        evaluator: Evaluator,
        config: SearchConfig = SearchConfig(),
        rng: np.random.Generator | None = None,
    ) -> None:
        self.evaluator = evaluator
        self.config = config
        self.rng = rng or np.random.default_rng()

    def search(
        self,
        states: Sequence[State],
        add_root_noise: bool,
        time_limit_seconds: float | None = None,
        reusable_roots: Sequence[Node | None] | None = None,
    ) -> list[Node]:
        deadline = (
            None if time_limit_seconds is None else time.perf_counter() + time_limit_seconds
        )
        if reusable_roots is not None and len(reusable_roots) != len(states):
            raise ValueError("reusable_roots must match states")
        candidates = reusable_roots or [None] * len(states)
        roots = [
            self._reuse_root(state, candidate)
            for state, candidate in zip(states, candidates, strict=True)
        ]
        expandable = [root for root in roots if root.terminal is None]
        self._evaluate_and_expand([root for root in expandable if not root.expanded])
        if add_root_noise:
            for root in expandable:
                noise = self.rng.dirichlet(
                    np.full(len(root.actions), self.config.dirichlet_alpha)
                )
                fraction = self.config.dirichlet_fraction
                root.priors = ((1.0 - fraction) * root.priors + fraction * noise).astype(
                    np.float32
                )

        completed = 0
        batch_size = max(1, self.config.inference_batch_size)
        while completed < self.config.simulations:
            if deadline is not None and time.perf_counter() >= deadline:
                break
            pending: list[Node] = []
            records: list[tuple[list[tuple[Node, int]], Node]] = []
            reservations = min(batch_size, self.config.simulations - completed)
            for root in expandable:
                for _ in range(reservations):
                    trace, leaf = self._reserve(root)
                    records.append((trace, leaf))
                    if leaf.terminal is None and not leaf.expanded:
                        pending.append(leaf)

            values_by_id = self._evaluate_and_expand(pending)
            for trace, leaf in records:
                value = leaf.terminal
                if value is None:
                    value = values_by_id[id(leaf)]
                for parent, edge in reversed(trace):
                    parent.pending[edge] -= 1
                    value = -value
                    parent.visits[edge] += 1
                    parent.value_sums[edge] += value
            completed += reservations
        return roots

    def _reserve(self, root: Node) -> tuple[list[tuple[Node, int]], Node]:
        node = root
        trace: list[tuple[Node, int]] = []
        while node.expanded and node.terminal is None:
            edge = node.select(self.config.cpuct)
            node.pending[edge] += 1
            trace.append((node, edge))
            node = node.child(edge)
        return trace, node

    @staticmethod
    def _reuse_root(state: State, candidate: Node | None) -> Node:
        if candidate is None:
            return Node(state)
        if candidate.state == state:
            return candidate
        for child in candidate.children.values():
            if child.state == state:
                return child
        return Node(state)

    def _evaluate_and_expand(self, nodes: Sequence[Node]) -> dict[int, float]:
        unique: dict[State, Node] = {node.state: node for node in nodes}
        if not unique:
            return {}
        batch = list(unique.values())
        logits, values = self.evaluator.evaluate([node.state for node in batch])
        if logits.shape != (len(batch), ACTION_SIZE):
            raise ValueError("evaluator returned an invalid policy shape")
        if values.shape != (len(batch),):
            raise ValueError("evaluator returned an invalid value shape")
        result: dict[int, float] = {}
        state_values: dict[State, float] = {}
        for node, policy, value in zip(batch, logits, values, strict=True):
            node.expand(policy)
            state_values[node.state] = float(value)
        for node in nodes:
            result[id(node)] = state_values[node.state]
            if not node.expanded:
                source = unique[node.state]
                node.actions = source.actions.copy()
                node.priors = source.priors.copy()
                node.visits = source.visits.copy()
                node.pending = source.pending.copy()
                node.value_sums = source.value_sums.copy()
                node.expanded = True
        return result


def visit_policy(root: Node, temperature: float = 1.0) -> tuple[np.ndarray, np.ndarray]:
    if root.terminal is not None or len(root.actions) == 0:
        return np.empty(0, dtype=np.int32), np.empty(0, dtype=np.float32)
    counts = root.visits.astype(np.float64)
    if not np.any(counts):
        probabilities = root.priors.astype(np.float64)
    elif temperature <= 1e-6:
        probabilities = np.zeros_like(counts)
        probabilities[int(np.argmax(counts))] = 1.0
    else:
        probabilities = np.power(counts, 1.0 / temperature)
        probabilities /= np.sum(probabilities)
    return root.actions.copy(), probabilities.astype(np.float32)


def choose_action(
    root: Node,
    rng: np.random.Generator,
    temperature: float = 0.0,
    tactical_guard: bool = False,
) -> int:
    actions, probabilities = visit_policy(root, temperature)
    if len(actions) == 0:
        raise ValueError("cannot choose an action from a terminal root")
    if tactical_guard:
        candidates = _tactical_candidates(root.state, actions)
        actions = actions[candidates]
        probabilities = probabilities[candidates]
        total = float(np.sum(probabilities))
        if total == 0.0:
            probabilities = root.priors[candidates].copy()
            total = float(np.sum(probabilities))
        probabilities /= total
    return int(rng.choice(actions, p=probabilities))


def _tactical_candidates(state: State, actions: np.ndarray) -> np.ndarray:
    successors = [apply_action(state, int(action), validate=False) for action in actions]
    wins = np.asarray([mover_won(successor) for successor in successors])
    if np.any(wins):
        return np.flatnonzero(wins)
    safe = np.asarray([not _has_immediate_win(successor) for successor in successors])
    return np.flatnonzero(safe) if np.any(safe) else np.arange(len(actions))


def _has_immediate_win(state: State) -> bool:
    return any(
        mover_won(apply_action(state, action, validate=False))
        for action in legal_actions(state)
    )


def encoded_batch(states: Sequence[State]) -> np.ndarray:
    return np.stack([input_planes(state) for state in states])
