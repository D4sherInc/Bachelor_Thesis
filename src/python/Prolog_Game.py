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


class PrologGame(pyspiel.Game):
    """A Prolog Version of Tic-Tac_Toe"""

    supported_games = ["nim", "tic_tac_toe", "connect4"]

    def __init__(self, game_string=None, params=None):
        if game_string in self.supported_games:
            prolog.consult("../prolog/%s_bridge.pl" % game_string)
            self.game_name = game_string
        else:
            raise ValueError("Missing game. Supported Games: " + game for game in self.supported_games)

        gameTypes = list(prolog.query("getGameTypes(GameTypes)"))[0]["GameTypes"]
        gameInfos = list(prolog.query("getGameInfos(GameInfos)"))[0]["GameInfos"]
        self._GAME_TYPE, self._GAME_INFO = _assign_game_attributes(gameTypes, gameInfos)
        self._NUM_PLAYERS = self._GAME_TYPE.max_num_players

        super().__init__(self._GAME_TYPE, self._GAME_INFO, params or dict())
        pyspiel.register_game(self._GAME_TYPE, PrologGame)

    def new_initial_state(self):
        """Returns a state corresponding to the start of a game"""
        return PrologGameState(self)

    def make_py_observer(self, iig_obs_type=None, params=None):
        """return an object used for observing game state
        called with every observation of a state
        separate observer per game"""
        if ((iig_obs_type is None) or
                (iig_obs_type.public_info and not iig_obs_type.perfect_recall)):
            match self.game_name:
                case "tic_tac_toe":
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
        prolog_game_state = query["InitState"]
        self.game_state = translate_from_prolog(prolog_game_state)[1]
        self.cur_player = query["CurrentPlayer"]
        self._player0_score = 0

    def __str__(self):
        # return _board_to_string(self.game_state)
        if not isinstance(self.game_state, list):
            return str(self.game_state)
        r = ""
        for row in self.game_state:
            r += str(row) + "\n"

        return r

    def current_player(self):
        """return current player"""
        return self.cur_player

    def legal_actions(self, player=None):
        """get the current legal actions"""
        game_state = self.game_state
        query = list(prolog.query("legal_actions([%s, %s], Legal_actions)" % (self.cur_player, game_state)))
        if not query:
            return []
        l = query[0]
        legal_actions = l["Legal_actions"]
        return legal_actions if self.cur_player == player or player is None else []

    def is_terminal(self):
        return bool(list(prolog.query("is_terminal([_, %s])" % self.game_state)))

    def apply_action(self, move):
        """applies action to game_state
        returns the new game_state"""
        game_state = self.game_state
        queue = list(prolog.query("GameState = [%s, %s], Move = %s, apply_action(GameState, Move, NewGameState)" % (
                self.cur_player, game_state, move)))
        if not queue:
            raise ValueError("Tried to apply_action on game_state " + str(game_state) + " with move " + str(move) +
                             " and prolog query failed")

        new_list = queue[0]
        new_game_state = new_list["NewGameState"]

        new_game_state = translate_from_prolog(new_game_state)
        self.game_state = new_game_state[1]
        self.cur_player = 1 - self.cur_player

        # check for winner (-> points)
        if bool(list(prolog.query("is_terminal([_, %s])" % self.game_state))):
            query = list(prolog.query("returns([%s, %s], Player, Points)" % (self.cur_player, self.game_state)))[0]
            points = query["Points"]
            self._player0_score = points if query["Player"] == 0 else -points

        return self.game_state

    def get_next_player(self):
        """returns the next player based on current stage
        only 2 player games so far"""
        cur_player = self.cur_player
        queue = list(prolog.query("Player = %s, other_player(Player, Next_Player)" % cur_player))
        return queue[0]['Next_Player'] if queue else False

    def returns(self):
        """return total rewards for current game state"""
        return [self._player0_score, -self._player0_score]

    def is_chance_node(self):
        """return if current_state is a chance_node
        supported games so far without chance_nodes
        method needed for environment"""
        return False

    def action_to_string(self, player, action):
        """returns the given action as human-readable"""
        # TODO: implement on Prolog side
        query = list(prolog.query("action_to_string(%s, %s, Action_String)" % (player, action)))
        action_string = query[0]["Action_String"]
        return translate_from_prolog(action_string)


def translate_from_prolog(l):
    """Helper method
    translate pyswip values back to pythonic values"""
    if isinstance(l, list):
        for list_index, list_element in enumerate(l):
            if isinstance(list_element, list):
                for index, el in enumerate(l):
                    l[index] = translate_from_prolog(el)
                return l
            for index, el in enumerate(l):
                if isinstance(el, pyswip.Atom):
                    l[index] = el.value
                elif el is bytes:
                    l[index] = el.decode()
                else:
                    l[index] = el
            return l
    if isinstance(l, pyswip.Atom):
        return l.value
    elif hasattr(l, "decode"):
        return l.decode("utf-8")
    else:
        return l


def _assign_game_attributes(game_types, game_infos):
    """ get pyspiel.GameType and pyspiel.GameInfo arguments from Prolog game
    return pyspiel.GameType, pyspiel.GameInfo object"""
    types = {}
    infos = {}

    for attr in game_types:
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
                    case _:
                        raise GameSettingsError(
                            "dynamics needs to be 'sequential', 'mean_field' or 'simultaneous', was",
                            attr[1])
            case "chance_mode":
                match attr[1]:
                    case "deterministic":
                        types.update(chance_mode=pyspiel.GameType.ChanceMode.DETERMINISTIC)
                    case "explicit_stochastic":
                        types.update(chance_mode=pyspiel.GameType.ChanceMode.EXPLICIT_STOCHASTIC)
                    case "sampled_stochastic":
                        types.update(chance_mode=pyspiel.GameType.ChanceMode.SAMPLED_STOCHASTIC)
                    case _:
                        raise GameSettingsError("chance_mode needs to be 'deterministic', 'explicit_stochastic' or "
                                                "'sampled_stochastic', was", attr[1])

            case "information":
                match attr[1]:
                    case "imperfect_information":
                        types.update(information=pyspiel.GameType.Information.IMPERFECT_INFORMATION)
                    case "one_shot":
                        types.update(information=pyspiel.GameType.Information.ONE_SHOT)
                    case "perfect_information":
                        types.update(information=pyspiel.GameType.Information.PERFECT_INFORMATION)
                    case _:
                        raise GameSettingsError("information needs to be 'imperfect_information', 'one_shot' or "
                                                "'perfect_information', was", attr[1])
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
                    case _:
                        raise GameSettingsError("utility needs to be: 'constant_sum', 'general_sum', 'identical' or "
                                                "'zero_sum', was", attr[1])
            case "reward_model":
                match attr[1]:
                    case "rewards":
                        types.update(reward_model=pyspiel.GameType.RewardModel.REWARDS)
                    case "terminal":
                        types.update(reward_model=pyspiel.GameType.RewardModel.TERMINAL)
                    case _:
                        raise GameSettingsError("reward_model needs to be 'rewards' or 'terminal', was", attr[1])
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

    for attr in game_infos:
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


class GameSettingsError(Exception):
    """Raised when GameType or GameInfo attribute was wrong"""
    pass
