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
        self.value_sums = np.zeros(len(self.actions), dtype=np.float32)
        self.expanded = True

    def select(self, cpuct: float) -> int:
        q = np.divide(
            self.value_sums,
            self.visits,
            out=np.zeros_like(self.value_sums),
            where=self.visits != 0,
        )
        exploration = (
            cpuct
            * self.priors
            * np.sqrt(float(np.sum(self.visits)) + 1.0)
            / (1.0 + self.visits)
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
    cpuct: float = 1.5
    dirichlet_alpha: float = 0.3
    dirichlet_fraction: float = 0.25


class BatchedMCTS:
    """PUCT search that evaluates one leaf from every active root per batch."""

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
    ) -> list[Node]:
        deadline = (
            None if time_limit_seconds is None else time.perf_counter() + time_limit_seconds
        )
        roots = [Node(state) for state in states]
        expandable = [root for root in roots if root.terminal is None]
        self._evaluate_and_expand(expandable)
        if add_root_noise:
            for root in expandable:
                noise = self.rng.dirichlet(
                    np.full(len(root.actions), self.config.dirichlet_alpha)
                )
                fraction = self.config.dirichlet_fraction
                root.priors = ((1.0 - fraction) * root.priors + fraction * noise).astype(
                    np.float32
                )

        for _ in range(self.config.simulations):
            if deadline is not None and time.perf_counter() >= deadline:
                break
            pending: list[Node] = []
            records: list[tuple[list[tuple[Node, int]], Node]] = []
            for root in expandable:
                node = root
                trace: list[tuple[Node, int]] = []
                while node.expanded and node.terminal is None:
                    edge = node.select(self.config.cpuct)
                    trace.append((node, edge))
                    node = node.child(edge)
                records.append((trace, node))
                if node.terminal is None and not node.expanded:
                    pending.append(node)

            values_by_id = self._evaluate_and_expand(pending)
            for trace, leaf in records:
                value = leaf.terminal
                if value is None:
                    value = values_by_id[id(leaf)]
                for parent, edge in reversed(trace):
                    value = -value
                    parent.visits[edge] += 1
                    parent.value_sums[edge] += value
        return roots

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
    root: Node, rng: np.random.Generator, temperature: float = 0.0
) -> int:
    actions, probabilities = visit_policy(root, temperature)
    if len(actions) == 0:
        raise ValueError("cannot choose an action from a terminal root")
    return int(rng.choice(actions, p=probabilities))


def encoded_batch(states: Sequence[State]) -> np.ndarray:
    return np.stack([input_planes(state) for state in states])
