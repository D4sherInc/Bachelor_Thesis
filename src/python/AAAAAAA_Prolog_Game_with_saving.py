from pyswip import Prolog
import pyspiel
from Prolog_Game import translate_from_prolog, GameSettingsError

prolog = Prolog()


class PrologGameWithSaving(pyspiel.Game):
    """A Prolog Version of Tic-Tac_Toe"""

    # supported_games = ["nim", "tic_tac_toe", "connect4"]

    def __init__(self, game_string=None, params=None):
        prolog = Prolog()

        prolog.consult("../prolog/AAAAAAA_saving_ttt.pl")
        gameTypes = list(prolog.query("getGameTypes(GameTypes)"))[0]["GameTypes"]
        gameInfos = list(prolog.query("getGameInfos(GameInfos)"))[0]["GameInfos"]
        translate_from_prolog(gameTypes)
        translate_from_prolog(gameInfos)
        self._GAME_TYPE, self._GAME_INFO = _assign_game_attributes(gameTypes, gameInfos)
        self._NUM_PLAYERS = self._GAME_TYPE.max_num_players
        super().__init__(self._GAME_TYPE, self._GAME_INFO, params or dict())

    def new_initial_state(self):
        """Returns a state corresponding to the start of a game"""
        return PrologGameState(self)

    def make_py_observer(self, iig_obs_type=None, params=None):
        """return an object used for observing game state
        called with every observation of a state
        separate observer per game"""
        pass

    def num_players(self):
        pass


class PrologGameState(pyspiel.State):
    """A Prolog Version of a Game state"""

    def __init__(self, game):
        super().__init__(game)
        queue = list(prolog.query("init(Hash)"))
        q = queue[0]
        self.state_hash = q["Hash"]

    def __str__(self):
        board = list(prolog.query("gamestate(%s, Board, _, _, _)" % self.state_hash))[0]["Board"]
        translate_from_prolog(board)
        return str(board)

    def current_player(self):
        player = list(prolog.query("gamestate(%s, _, CurrentPlayer, _, _)" % self.state_hash))[0]["CurrentPlayer"]
        return player

    def legal_actions(self, player=None):
        legal_actions = list(prolog.query("gamestate(%s, _, _, LegalActions, _)" % self.state_hash))[0]["LegalActions"]
        return legal_actions

    def is_terminal(self):
        return bool(list(prolog.query("is_terminal(%s)" % self.state_hash)))

    def apply_action(self, move):
        queue = list(prolog.query("apply_action(%s, %s, NewHash)" % (self.state_hash, move)))
        if not queue:
            raise ValueError("apply_action on gamestate %s with move %s. Prolog query failed" % (
                    self.__str__(), move))
        self.state_hash = queue[0]["NewHash"]

    def get_next_player(self):
        return list(prolog.query("gamestate(%s, _, P1, _, _), other_player(P1, P2)" % self.state_hash))[0]["P2"]

    def returns(self):
        return list(prolog.query("returns(%s, R)" % self.state_hash))[0]['R']

    def is_chance_node(self):
        return False

    def action_to_string(self, player, action):
        pass


def _assign_game_attributes(game_types, game_infos):
    """ get pyspiel.GameType and pyspiel.GameInfo arguments from Prolog game
    return pyspiel.GameType, pyspiel.GameInfo object
    strings are represented with single quotes in Prolog"""
    types = {}
    infos = {}

    for attr in game_types:
        match attr[0]:
            case "short_name":
                types.update(short_name=attr[1])
            case "long_name":
                types.update(long_name=attr[1])
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
                types.update(provides_information_state_string=True if attr[1] == 'True' else False)
            case "provides_information_state_tensor":
                types.update(provides_information_state_tensor=True if attr[1] == 'True' else False)
            case "provides_observation_string":
                types.update(provides_observation_string=True if attr[1] == 'True' else False)
            case "provides_observation_tensor":
                types.update(provides_observation_tensor=True if attr[1] == 'True' else False)
            case "parameter_specification":
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
