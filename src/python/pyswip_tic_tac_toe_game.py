"""Python file for Tic Tac Toe
used as bridge between Prolog definition of Tic Tac Toe
and reinforcement learning environments and algorithms of
OpenSpiel
uses PySWIP to query Prolog"""
import pyswip
from pyswip import Prolog
import pyspiel
import numpy as np
from open_spiel.python.observation import IIGObserverForPublicInfoGame

prolog = Prolog()

_NUM_PLAYERS = 2
_NUM_ROWS = 3
_NUM_COLS = 3
_NUM_CELLS = _NUM_ROWS * _NUM_COLS
_GAME_TYPE = pyspiel.GameType(
        short_name="python_tic_tac_toe",
        long_name="Python Tic-Tac-Toe",
        dynamics=pyspiel.GameType.Dynamics.SEQUENTIAL,
        chance_mode=pyspiel.GameType.ChanceMode.DETERMINISTIC,
        information=pyspiel.GameType.Information.PERFECT_INFORMATION,
        utility=pyspiel.GameType.Utility.ZERO_SUM,
        reward_model=pyspiel.GameType.RewardModel.TERMINAL,
        max_num_players=_NUM_PLAYERS,
        min_num_players=_NUM_PLAYERS,
        provides_information_state_string=True,
        provides_information_state_tensor=False,
        provides_observation_string=True,
        provides_observation_tensor=True,
        parameter_specification={})
_GAME_INFO = pyspiel.GameInfo(
        num_distinct_actions=_NUM_CELLS,
        max_chance_outcomes=0,
        num_players=2,
        min_utility=-1.0,
        max_utility=1.0,
        utility_sum=0.0,
        max_game_length=_NUM_CELLS)


class TicTacToeGame(pyspiel.Game):
    """A Prolog Version of Tic-Tac_Toe"""

    def __init__(self, params=None):
        super().__init__(_GAME_TYPE, _GAME_INFO, params or dict())

    def new_initial_state(self):
        """Returns a state corresponding to the start of a game"""
        return TicTacToeState(self)

    def make_py_observer(self, iig_obs_type=None, params=None):
        """return an object usre for observing game state"""
        if ((iig_obs_type is None) or
                (iig_obs_type.public_info and not iig_obs_type.perfect_recall)):
            return BoardObserver(params)
        else:
            return IIGObserverForPublicInfoGame(iig_obs_type, params)

    def num_players(self):
        return _NUM_PLAYERS


class TicTacToeState(pyspiel.State):
    # class TicTacToeState(ProspielQuery):
    """Query class to get results from Tic_Tac_Toe.pl
    returns boolean for end, and the winning player as string"""

    def __init__(self, game):
        super().__init__(game)
        # TODO: query empty
        prolog.consult("../prolog/tic_tac_toe_without_saving_states.pl")
        q = list(prolog.query("init(InitState, CurrentPlayer, Player0_score)"))
        query = q[0]
        self.cur_player = query["CurrentPlayer"]
        self._player0_score = query["Player0_score"]
        prolog_board = query["InitState"]
        self.board = translate_from_prolog(prolog_board)
        self.terminal = False

    def current_player(self):
        """return current player"""
        return self.cur_player

    def legal_actions(self, player):
        board = self.board
        query = list(prolog.query("legal_actions(%s, Legal_actions)" % board))[0]
        legal_actions = query["Legal_actions"]
        return legal_actions if self.cur_player == player else []

    def is_terminal(self):
        return self.terminal
        # board = self.board
        # legal_actions = list(prolog.query("Board = %s, legal_actions(Board, Legal_actions)"
        #                                   % board))[0]['Legal_Moves']
        # # if no more available moves: end
        # if len(legal_actions) == 0:
        #     return True, "No one. Cat's game!"
        #
        # win = list(prolog.query("Board = %s, wingame(Board, Player)"))
        #
        # if len(win) == 0:
        #     return False, "No one (yet)"
        # else:
        #     return True, win[0]['Player']

    def apply_action(self, move):
        """applies action to board
        returns the new board"""
        board = self.board
        queue = list(prolog.query("Board = %s, Move = %s, apply_action(Board, Move, NewBoard)" % (board, move)))
        new_list = queue[0]
        new_board = new_list["NewBoard"]
        if isinstance(new_board, bytes):
            b = list(prolog.query("board(B)"))[0]["B"]
            translate_from_prolog(b)
            raise ValueError(b, board, new_board)

        new_board = translate_from_prolog(new_board)
        self.board = new_board
        self.cur_player = 1 - self.cur_player
        self.terminal = bool(list(prolog.query("is_terminal(%s)" % self.board)))
        # get the winner and assert points, if there is a winner
        if self.terminal:
            query = list(prolog.query("returns(%s, Player, Points)" % self.board))[0]
            points = query["Points"]
            self._player0_score = points if query["Player"] == "x" else -points
        return new_board

    def get_next_player(self):
        """returns the next player based on current stage"""
        cur_player = self.cur_player
        queue = list(prolog.query("Player = %s, other_player(Player, Next_Player)" % cur_player))
        return queue[0]['Next_Player'] if queue else False

    def __str__(self):
        return _board_to_string(self.board)

    def _action_to_string(self, player, action):
        row, col = _coordinates(action)
        return "{}({},{})".format("x" if player == 0 else "o", row, col)

    def returns(self):
        """return total rewards for current game state"""
        return [self._player0_score, -self._player0_score]

    def is_chance_node(self):
        return False


class BoardObserver:
    def __init__(self, params):
        """init an empty observation tensor"""
        if params:
            raise ValueError(f"Observation parameters not supported; passed {params}")

        shape = (1 + _NUM_PLAYERS, _NUM_ROWS, _NUM_COLS)
        self.tensor = np.zeros(np.prod(shape), np.float32)
        self.dict = {"observation": np.reshape(self.tensor, shape)}

    def set_from(self, state, player):
        """Updates `tensor` and `dict` to reflect `state` from PoV of `player`."""
        del player
        # We update the observation via the shaped tensor since indexing is more
        # convenient than with the 1-D tensor. Both are views onto the same memory.
        obs = self.dict["observation"]
        obs.fill(0)
        for row in range(_NUM_ROWS):
            for col in range(_NUM_COLS):
                # state.board is list of 3x3, needs to be ndarray of 3x3
                cell_state = ".ox".index(np.array(state.board)[row, col])
                obs[cell_state, row, col] = 1

    def string_from(self, state, player):
        """Observation of `state` from the PoV of `player`, as a string."""
        del player
        return _board_to_string(state.board)


def _coordinates(action):
    return action // _NUM_COLS, action % _NUM_ROWS
    pass


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


def translate_from_prolog(l):
    """Helper method
    translate pyswip values back to pythonic values"""
    for list_index, list_element in enumerate(l):
        if isinstance(list_element, list):
            for index, el in enumerate(l):
                l[index] = translate_from_prolog(el)
            return l
        for index, el in enumerate(l):
            l[index] = el.value if isinstance(el, pyswip.Atom) else el
        return l
