from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Iterable, Sequence

import numpy as np

BOARD_SIZE = 5
CELL_COUNT = BOARD_SIZE * BOARD_SIZE
DIRECTIONS = tuple(
    (dr, dc)
    for dr in (-1, 0, 1)
    for dc in (-1, 0, 1)
    if dr != 0 or dc != 0
)
NO_BUILD = len(DIRECTIONS)
ACTION_PLANES = len(DIRECTIONS) * (len(DIRECTIONS) + 1)
ACTION_SIZE = CELL_COUNT * ACTION_PLANES
INPUT_CHANNELS = 7

Pos = int
Player = tuple[Pos, Pos]
Players = tuple[Player, Player]


def pos(row: int, col: int) -> Pos:
    return row * BOARD_SIZE + col


def row_col(cell: Pos) -> tuple[int, int]:
    return divmod(cell, BOARD_SIZE)


def offset(cell: Pos, direction: tuple[int, int]) -> Pos:
    row, col = row_col(cell)
    dr, dc = direction
    return pos(row + dr, col + dc)


def adjacent(cell: Pos, direction: tuple[int, int]) -> Pos | None:
    row, col = row_col(cell)
    dr, dc = direction
    new_row, new_col = row + dr, col + dc
    return pos(new_row, new_col) if in_bounds(new_row, new_col) else None


def in_bounds(row: int, col: int) -> bool:
    return 0 <= row < BOARD_SIZE and 0 <= col < BOARD_SIZE


@dataclass(frozen=True, slots=True)
class State:
    turn: int
    players: Players
    board: tuple[int, ...]

    def __post_init__(self) -> None:
        if len(self.board) != CELL_COUNT:
            raise ValueError(f"board must contain {CELL_COUNT} cells")
        if any(level < 0 or level > 4 for level in self.board):
            raise ValueError("tower levels must be between 0 and 4")
        workers = self.players[0] + self.players[1]
        if len(set(workers)) != 4:
            raise ValueError("workers must occupy four distinct cells")
        if any(cell < 0 or cell >= CELL_COUNT for cell in workers):
            raise ValueError("worker outside board")

    @classmethod
    def from_json(cls, value: dict[str, Any]) -> State:
        rows = value["spaces"]
        if len(rows) != BOARD_SIZE or any(len(row) != BOARD_SIZE for row in rows):
            raise ValueError("spaces must be a 5x5 matrix")

        def decode_player(raw: Sequence[Sequence[int]]) -> Player:
            if len(raw) != 2 or any(len(worker) != 2 for worker in raw):
                raise ValueError("each player must have two [row, col] workers")
            workers = tuple(pos(worker[0] - 1, worker[1] - 1) for worker in raw)
            return workers  # type: ignore[return-value]

        raw_players = value["players"]
        if len(raw_players) != 2:
            raise ValueError("players must contain exactly two players")
        players = (decode_player(raw_players[0]), decode_player(raw_players[1]))
        board = tuple(level for row in rows for level in row)
        return cls(int(value["turn"]), players, board)

    def to_json(self) -> dict[str, Any]:
        def encode_player(player: Player) -> list[list[int]]:
            return [[row + 1, col + 1] for row, col in map(row_col, player)]

        return {
            "turn": self.turn,
            "players": [encode_player(self.players[0]), encode_player(self.players[1])],
            "spaces": [
                list(self.board[start : start + BOARD_SIZE])
                for start in range(0, CELL_COUNT, BOARD_SIZE)
            ],
        }


def empty_state(players: Players, turn: int = 0) -> State:
    return State(turn, players, (0,) * CELL_COUNT)


def protocol_opening() -> State:
    return empty_state(
        (
            (pos(1, 1), pos(3, 3)),
            (pos(1, 3), pos(3, 1)),
        )
    )


def random_opening(rng: np.random.Generator) -> State:
    workers = tuple(int(x) for x in rng.choice(CELL_COUNT, size=4, replace=False))
    return empty_state(((workers[0], workers[1]), (workers[2], workers[3])))


def direction_index(start: Pos, end: Pos) -> int:
    sr, sc = row_col(start)
    er, ec = row_col(end)
    try:
        return DIRECTIONS.index((er - sr, ec - sc))
    except ValueError as error:
        raise ValueError("cells are not adjacent") from error


def encode_action(origin: Pos, destination: Pos, build: Pos | None) -> int:
    move_direction = direction_index(origin, destination)
    build_direction = NO_BUILD if build is None else direction_index(destination, build)
    return origin * ACTION_PLANES + move_direction * (len(DIRECTIONS) + 1) + build_direction


def decode_action(action: int) -> tuple[Pos, Pos, Pos | None]:
    if action < 0 or action >= ACTION_SIZE:
        raise ValueError("action outside policy space")
    origin, plane = divmod(action, ACTION_PLANES)
    move_direction, build_direction = divmod(plane, len(DIRECTIONS) + 1)
    destination = offset(origin, DIRECTIONS[move_direction])
    build = (
        None
        if build_direction == NO_BUILD
        else offset(destination, DIRECTIONS[build_direction])
    )
    return origin, destination, build


def _valid_adjacent(
    state: State,
    origin: Pos,
    candidate: Pos,
    players: Players,
    moving: bool,
) -> bool:
    row, col = row_col(candidate)
    if not in_bounds(row, col):
        return False
    if candidate in players[0] or candidate in players[1]:
        return False
    if state.board[candidate] > 3:
        return False
    return not moving or state.board[candidate] <= state.board[origin] + 1


def legal_actions(state: State) -> tuple[int, ...]:
    if mover_won(state):
        return ()
    current, opponent = state.players
    actions: list[int] = []
    for worker_index, origin in enumerate(current):
        for move_direction in DIRECTIONS:
            destination = adjacent(origin, move_direction)
            if destination is None:
                continue
            if not _valid_adjacent(state, origin, destination, state.players, True):
                continue
            moved = list(current)
            moved[worker_index] = destination
            moved_players: Players = ((moved[0], moved[1]), opponent)
            if state.board[destination] == 3:
                actions.append(encode_action(origin, destination, None))
                continue
            for build_direction in DIRECTIONS:
                build = adjacent(destination, build_direction)
                if build is None:
                    continue
                if _valid_adjacent(state, destination, build, moved_players, False):
                    actions.append(encode_action(origin, destination, build))
    return tuple(actions)


def apply_action(state: State, action: int, *, validate: bool = True) -> State:
    origin, destination, build = decode_action(action)
    if validate and action not in legal_actions(state):
        raise ValueError("illegal action")

    current, opponent = state.players
    moved = list(current)
    moved[moved.index(origin)] = destination
    board = list(state.board)
    if build is not None:
        board[build] += 1
    return State(
        state.turn + 1,
        (opponent, (moved[0], moved[1])),
        tuple(board),
    )


def mover_won(state: State) -> bool:
    return any(state.board[worker] == 3 for worker in state.players[1])


def terminal_value(state: State) -> float | None:
    if mover_won(state) or not legal_actions(state):
        return -1.0
    return None


def input_planes(state: State) -> np.ndarray:
    planes = np.zeros((INPUT_CHANNELS, BOARD_SIZE, BOARD_SIZE), dtype=np.float32)
    for cell, level in enumerate(state.board):
        row, col = row_col(cell)
        planes[level, row, col] = 1.0
    for cell in state.players[0]:
        row, col = row_col(cell)
        planes[5, row, col] = 1.0
    for cell in state.players[1]:
        row, col = row_col(cell)
        planes[6, row, col] = 1.0
    return planes


def transform_coordinates(row: int, col: int, symmetry: int) -> tuple[int, int]:
    if symmetry < 0 or symmetry >= 8:
        raise ValueError("symmetry must be in range 0..7")
    if symmetry >= 4:
        col = BOARD_SIZE - 1 - col
    for _ in range(symmetry % 4):
        row, col = col, BOARD_SIZE - 1 - row
    return row, col


def transform_pos(cell: Pos, symmetry: int) -> Pos:
    return pos(*transform_coordinates(*row_col(cell), symmetry))


def transform_state(state: State, symmetry: int) -> State:
    board = [0] * CELL_COUNT
    for cell, level in enumerate(state.board):
        board[transform_pos(cell, symmetry)] = level

    def transform_player(player: Player) -> Player:
        return (transform_pos(player[0], symmetry), transform_pos(player[1], symmetry))

    return State(
        state.turn,
        (transform_player(state.players[0]), transform_player(state.players[1])),
        tuple(board),
    )


def transform_action(action: int, symmetry: int) -> int:
    origin, destination, build = decode_action(action)
    return encode_action(
        transform_pos(origin, symmetry),
        transform_pos(destination, symmetry),
        None if build is None else transform_pos(build, symmetry),
    )


def transform_sparse_policy(
    actions: Iterable[int], probabilities: Iterable[float], symmetry: int
) -> tuple[np.ndarray, np.ndarray]:
    transformed = np.fromiter(
        (transform_action(action, symmetry) for action in actions), dtype=np.int32
    )
    return transformed, np.asarray(tuple(probabilities), dtype=np.float32)
