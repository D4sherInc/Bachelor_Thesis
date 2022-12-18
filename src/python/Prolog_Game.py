"""Python file for Tic Tac Toe
same as pyswip_tic_tac_toe_game_saving.py
but uses the prolog definition with saving states in Prolog
"""

import pyswip
from pyswip import Prolog
import pyspiel
import Prolog_Observer
from open_spiel.python.observation import IIGObserverForPublicInfoGame

prolog = Prolog()

# _NUM_PLAYERS = 2
_NUM_ROWS = 3
_NUM_COLS = 3
_NUM_CELLS = _NUM_ROWS * _NUM_COLS

_SUPPORTED_GAMES = ["nim", "tic_tac_toe_bridge", "connect4"]


class PrologGame(pyspiel.Game):
    """A Prolog Version of Tic-Tac_Toe"""

    def __init__(self, game_string=None, params=None):
        # TODO: make decision over what game to load dynamic
        if game_string is not None:
            prolog.consult("../prolog/%s.pl" % game_string)
            self.game_name = game_string
        else:
            prolog.consult("../prolog/tic_tac_toe_bridge.pl")
            self.game_name = "tic_tac_toe_without_saving_states"

        gameTypes = list(prolog.query("getGameTypes(GameTypes)"))[0]["GameTypes"]
        gameInfos = list(prolog.query("getGameInfos(GameInfos)"))[0]["GameInfos"]
        self._GAME_TYPE, self._GAME_INFO = _assign_game_attributes(gameTypes, gameInfos)
        self._NUM_PLAYERS = self._GAME_TYPE.max_num_players
        super().__init__(self._GAME_TYPE, self._GAME_INFO, params or dict())

    def new_initial_state(self):
        """Returns a state corresponding to the start of a game"""
        return PrologGameState(self)

    def make_py_observer(self, iig_obs_type=None, params=None):
        """return an object user for observing game state
        called with every observation of a state"""
        if ((iig_obs_type is None) or
                (iig_obs_type.public_info and not iig_obs_type.perfect_recall)):
            match self.game_name:
                case "tic_tac_toe_bridge":
                    return Prolog_Observer.TicTacToeObserver(params)
                case "nim":
                    return Prolog_Observer.NimObserver(params)
                case "connect4":
                    return Prolog_Observer.Connect4Observer(params, self._GAME_INFO)



        else:
            return IIGObserverForPublicInfoGame(iig_obs_type, params)

    def num_players(self):
        return self._NUM_PLAYERS


class PrologGameState(pyspiel.State):
    """Query class to get results from tic_tac_toe.pl
    returns boolean for end, and the winning player as string"""

    def __init__(self, game):
        super().__init__(game)
        # TODO: query empty
        q = list(prolog.query("init(InitState, CurrentPlayer)"))
        query = q[0]
        self.cur_player = query["CurrentPlayer"]
        self._player0_score = 0
        prolog_board = query["InitState"]
        self.game_state = translate_from_prolog(prolog_board)[1]
        self.terminal = False

    def current_player(self):
        """return current player"""
        return self.cur_player

    def legal_actions(self, player):
        """get the current legal actions"""
        gamestate = self.game_state
        query = list(prolog.query("legal_actions([%s, %s], Legal_actions)" % (self.cur_player, gamestate)))
        if not query:
            return []
        l = query[0]
        legal_actions = l["Legal_actions"]
        return legal_actions if self.cur_player == player else []

    def is_terminal(self):
        return self.terminal

    def apply_action(self, move):
        """applies action to game_state
        returns the new game_state"""
        game_state = self.game_state
        queue = list(prolog.query("GameState = [%s, %s], Move = %s, apply_action(GameState, Move, NewGameState)" % (
                self.cur_player, game_state, move)))
        if not queue:
            raise ValueError("Tried to apply_action on gamestate " + str(game_state) + " with move " + str(move) +
                             " and prolog query failed")

        new_list = queue[0]
        new_game_state = new_list["NewGameState"]
        # if isinstance(new_board, bytes):
        #     b = list(prolog.query("game_state(B)"))[0]["B"]
        #     translate_from_prolog(b)
        #     raise ValueError(b, game_state, new_board)

        new_game_state = translate_from_prolog(new_game_state)
        self.game_state = new_game_state[1]
        self.cur_player = 1 - self.cur_player
        self.terminal = bool(list(prolog.query("is_terminal([_, %s])" % self.game_state)))
        # check for winner (-> points)
        if self.terminal:
            query = list(prolog.query("returns([%s, %s], Player, Points)" % (self.cur_player, self.game_state)))[0]
            points = query["Points"]
            self._player0_score = points if query["Player"] == 0 else -points
        return self.game_state

    def get_next_player(self):
        """returns the next player based on current stage"""
        cur_player = self.cur_player
        queue = list(prolog.query("Player = %s, other_player(Player, Next_Player)" % cur_player))
        return queue[0]['Next_Player'] if queue else False

    def __str__(self):
        # return _board_to_string(self.game_state)
        return str(self.game_state)

    def _action_to_string(self, player, action):
        row, col = _coordinates(action)
        return "{}({},{})".format("x" if player == 0 else "o", row, col)

    def returns(self):
        """return total rewards for current game state"""
        return [self._player0_score, -self._player0_score]

    def is_chance_node(self):
        return False


def _coordinates(action):
    return action // _NUM_COLS, action % _NUM_ROWS
    pass


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


def _assign_game_attributes(gameTypes, gameInfos):
    """ get pyspiel.GameType and pyspiel.GameInfo arguments from Prolog game
    return pyspiel.GameType, pyspiel.GameInfo object"""
    types = {}
    infos = {}

    for attr in gameTypes:
        match attr[0]:
            case "short_name":
                types.update(short_name=attr[1].decode("utf-8"))
            case "long_name":
                types.update(long_name=attr[1].decode("utf-8"))
            case "dynamics":
                match attr[1]:
                    case "sequential":
                        types.update(dynamics=pyspiel.GameType.Dynamics.SEQUENTIAL)
                    case "mean_field":
                        types.update(dynamics=pyspiel.GameType.Dynamics.MEAN_FIELD)
                    case "simultaneous":
                        types.update(dynamics=pyspiel.GameType.Dynamics.SIMULTANEOUS)
            case "chance_mode":
                match attr[1]:
                    case "deterministic":
                        types.update(chance_mode=pyspiel.GameType.ChanceMode.DETERMINISTIC)
                    case "explicit_stochastic":
                        types.update(chance_mode=pyspiel.GameType.ChanceMode.EXPLICIT_STOCHASTIC)
                    case "sampled_stochastic":
                        types.update(chance_mode=pyspiel.GameType.ChanceMode.SAMPLED_STOCHASTIC)
            case "information":
                match attr[1]:
                    case "imperfect_information":
                        types.update(information=pyspiel.GameType.Information.IMPERFECT_INFORMATION)
                    case "one_shot":
                        types.update(information=pyspiel.GameType.Information.ONE_SHOT)
                    case "perfect_information":
                        types.update(information=pyspiel.GameType.Information.PERFECT_INFORMATION)
            case "utility":
                match attr[1]:
                    case "constant_sum":
                        types.update(utility=pyspiel.GameType.Utility.CONSTANT_SUM)
                    case "general_sum":
                        types.update(utility=pyspiel.GameType.Utility.GENERAL_SUM)
                    case "identical":
                        types.update(utility=pyspiel.GameType.Utility.IDENTICAL)
                    case "zero_sum":
                        types.update(utility=pyspiel.GameType.Utility.ZERO_SUM)
            case "reward_model":
                match attr[1]:
                    case "rewards":
                        types.update(reward_model=pyspiel.GameType.RewardModel.REWARDS)
                    case "terminal":
                        types.update(reward_model=pyspiel.GameType.RewardModel.TERMINAL)
            case "max_num_players":
                types.update(max_num_players=attr[1])
            case "min_num_players":
                types.update(min_num_players=attr[1])
            case "provides_information_state_string":
                types.update(provides_information_state_string=True if attr[1].decode("utf-8") == "True" else False)
            case "provides_information_state_tensor":
                types.update(provides_information_state_tensor=True if attr[1].decode("utf-8") == "True" else False)
            case "provides_observation_string":
                types.update(provides_observation_string=True if attr[1].decode("utf-8") == "True" else False)
            case "provides_observation_tensor":
                types.update(provides_observation_tensor=True if attr[1].decode("utf-8") == "True" else False)
            case "parameter_specification":
                # TODO: check for multiple params, not just empty ones
                types.update(parameter_specification={})

    for attr in gameInfos:
        match attr[0]:
            case "num_distinct_actions":
                infos.update(num_distinct_actions=attr[1])
            case "max_chance_outcomes":
                infos.update(max_chance_outcomes=attr[1])
            case "num_players":
                infos.update(num_players=attr[1])
            case "min_utility":
                infos.update(min_utility=float(attr[1]))
            case "max_utility":
                infos.update(max_utility=float(attr[1]))
            case "utility_sum":
                infos.update(utility_sum=float(attr[1]))
            case "max_game_length":
                infos.update(max_game_length=attr[1])

    return pyspiel.GameType(**types), pyspiel.GameInfo(**infos)
