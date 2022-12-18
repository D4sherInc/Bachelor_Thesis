"""Observations classes
used by rl_environment to get State Observations from Games
every Game needs separate Tensors
fitting observer object created based on given PrologGame object"""
import abc

import numpy as np


class PrologObserver(abc.ABC):
    @abc.abstractmethod
    def __init__(self, params):
        pass

    @abc.abstractmethod
    def set_from(self, state, player):
        pass

    @abc.abstractmethod
    def string_from(self, state, player):
        pass


class TicTacToeObserver(PrologObserver):
    def __init__(self, params):
        """init an empty observation tensor"""
        if params:
            raise ValueError(f"Observation parameters not supported; passed {params}")

        # distinguish between games: different game boards / states -> different game

        self._NUM_PLAYERS = 2
        self._NUM_ROWS = 3
        self._NUM_COLS = 3
        self._NUM_CELLS = self._NUM_ROWS * self._NUM_COLS

        shape = (1 + self._NUM_PLAYERS, self._NUM_ROWS, self._NUM_COLS)
        self.tensor = np.zeros(np.prod(shape), np.float32)
        self.dict = {"observation": np.reshape(self.tensor, shape)}

    def set_from(self, state, player):
        """Updates `tensor` and `dict` to reflect `state` from PoV of `player`."""
        del player
        # We update the observation via the shaped tensor since indexing is more
        # convenient than with the 1-D tensor. Both are views onto the same memory.
        obs = self.dict["observation"]
        obs.fill(0)

        for row in range(self._NUM_ROWS):
            for col in range(self._NUM_COLS):
                # state.board is list of 3x3, needs to be ndarray of 3x3
                cell_state = ".ox".index(np.array(state.game_state)[row, col])
                obs[cell_state, row, col] = 1

    def string_from(self, state, player):
        """Observation of `state` from the PoV of `player`, as a string."""
        del player
        return _board_to_string(state.game_state)


class NimObserver(PrologObserver):
    def __init__(self, params):
        """init an empty observation tensor"""
        if params:
            raise ValueError(f"Observation parameters not supported; passed {params}")

        # distinguish between games: different game boards / states -> different game

        shape = 10
        self.tensor = np.zeros(np.prod(shape), np.float32)
        self.dict = {"observation": np.reshape(self.tensor, shape)}

    def set_from(self, state, player):
        """Updates `tensor` and `dict` to reflect `state` from PoV of `player`."""
        del player
        # We update the observation via the shaped tensor since indexing is more
        # convenient than with the 1-D tensor. Both are views onto the same memory.
        obs = self.dict["observation"]
        obs.fill(0)
        # TODO: custom size based on remaining game
        if isinstance(state.game_state, int):
            return

        for row in range(_NUM_ROWS):
            for col in range(_NUM_COLS):
                # state.board is list of 3x3, needs to be ndarray of 3x3
                cell_state = ".ox".index(np.array(state.game_state)[row, col])
                obs[cell_state, row, col] = 1


class Connect4Observer(PrologObserver):
    def __init__(self, params, GameInfo):
        """init an empty observation tensor"""
        if params:
            raise ValueError(f"Observation parameters not supported; passed {params}")

        # distinguish between games: different game boards / states -> different game

        self._NUM_PLAYERS = GameInfo.num_players
        self._NUM_COLS = int(GameInfo.max_game_length / GameInfo.num_distinct_actions)
        self._NUM_ROWS = GameInfo.num_distinct_actions

        shape = (1 + self._NUM_PLAYERS, self._NUM_ROWS, self._NUM_COLS)

        self.tensor = np.zeros(np.prod(shape), np.float32)
        self.dict = {"observation": np.reshape(self.tensor, shape)}

    def set_from(self, state, player):
        """Updates `tensor` and `dict` to reflect `state` from PoV of `player`."""
        del player
        # We update the observation via the shaped tensor since indexing is more
        # convenient than with the 1-D tensor. Both are views onto the same memory.
        obs = self.dict["observation"]
        obs.fill(0)

        for row in range(self._NUM_ROWS):
            for col in range(self._NUM_COLS):
                # state.board is list of 6x7, needs to be ndarray of 6x7
                cell_state = "-XO".index(np.array(state.game_state)[row, col])
                obs[cell_state, row, col] = 1

    def string_from(self, state, player):
        """Observation of `state` from the PoV of `player`, as a string."""
        del player
        return _board_to_string(state.game_state)


def _board_to_string(board):
    """brings the board into a readable 3x3 representation
    with '.' for empty fields, from '0's which is used in Prolog
    returns board as String"""
    r = ""
    for index, el in enumerate(board):
        r += "." if el == 0 else el
        if index % 3 == 2:
            r += "\n"
    return r
