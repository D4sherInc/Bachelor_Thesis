:- use_module(blank_game, []).

% -----------------------------------------------------------------------------------------
% here come the interface predicates
% GameState is of Structure [Current_Player, State].
% State: e.g. 3x3 Board for Tic Tac Toe; Remaining Sticks as int for Nim

init(GameState, Current_Player):-
    fail.

legal_actions(GameState, Legal_actions) :-
    fail.

apply_action(GameState, Action, NewGameState) :-
    fail.

apply_actions(GameState, [], GameState):- !.
apply_actions(GameState, [M|Oves], FinalState) :-
    apply_action(GameState, M, NextState),
    apply_actions(NextState, Oves, FinalState).

is_terminal(GameState) :-
    fail.

returns(GameState, Player, Points_for_that_Player) :-
    fail.

% action_string(Player, Action, String)
action_string(Player, Action, String) :-
    fail.

% -----------------------------------------------------------------------------------------
% optional internal predicates
% e.g. reassigning player names to IDs (pyspiel using IDs, prolog using ground terms like "min", "max")
% e.g. reassigning actions to IDs (pyspiel using IDs, prolog using ground terms like "take1", "take2", "take3")


% -----------------------------------------------------------------------------------------
% here come the pyspiel GameTypes and GameInfos
% need to be specified before running OpenSpiel

getGameTypes(GameType) :-
    findall([Attr, Val], gametype(Attr, Val), GameType).

getGameInfos(GameInfo) :-
    findall([Attr, Val], gameinfo(Attr, Val), GameInfo).

gametype(short_name, "prolog_blank_game").
gametype(long_name, "Prolog Blank Game").
gametype(dynamics, sequential).
gametype(chance_mode, deterministic).
gametype(information, perfect_information).
gametype(utility, zero_sum).
gametype(reward_model, terminal).
gametype(max_num_players, 2).
gametype(min_num_players, 2).
gametype(provides_information_state_string, "True").
gametype(provides_information_state_tensor, "False").
gametype(provides_observation_string, "False").
gametype(provides_observation_tensor, "True").
gametype(parameter_specification, "{}").

gameinfo(num_distinct_actions, N).
gameinfo(max_chance_outcomes, 0).
gameinfo(num_players, 2).
gameinfo(min_utility, -1).
gameinfo(max_utility, 1).
gameinfo(utility_sum, 0).
gameinfo(max_game_length, N).