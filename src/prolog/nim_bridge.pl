:- use_module(nim, []).
% -----------------------------------------------------------------------------------------
% interface predicates
% called by PrologGame.py

init(InitState, PID) :-
    nim:start(pos(P1, Sticks)),
    player_ID_(PID, P1),
    InitState = [PID, Sticks].

current_player([P1, _], P1).

legal_actions([_, N], [0, 1, 2]):-
    N >= 0.

apply_action(GameState, A_ID, NewGameState) :-
    GameState = [Current_Player, Sticks],
    action_ID_(A_ID, Action),
    player_ID_(Current_Player, P1),
    nim:game_move(pos(P1, Sticks), Action, pos(P2, NewSticks)),
    player_ID_(Next_Player, P2),
    NewGameState = [Next_Player, NewSticks].

apply_actions(GameState, [], GameState) :- !.
apply_actions(GameState, [M|Oves], FinalState) :-
    apply_action(GameState, M, NextState),
    apply_actions(NextState, Oves, FinalState).

is_terminal([_, N]) :-
    N =< 0.

returns(GameState, PlayerID, 1):-
    GameState = [PlayerID, Sticks],
    player_ID_(PlayerID, Player),
    nim:win(pos(Player, Sticks), Player), !.


returns(_, min, 0).

% action_string(Player, Action, String)
action_to_string(0, 0, "Player 1 takes 1") :- !.
action_to_string(0, 1, "Player 1 takes 2") :- !.
action_to_string(0, 2, "Player 1 takes 3") :- !.
action_to_string(1, 0, "Player 2 takes 1") :- !.
action_to_string(1, 1, "Player 2 takes 2") :- !.
action_to_string(1, 2, "Player 2 takes 3").
% -----------------------------------------------------------------------------------------
% internal predicates

player_ID_(0, min).
player_ID_(1, max).

action_ID_(0, take1).
action_ID_(1, take2).
action_ID_(2, take3).

% -----------------------------------------------------------------------------------------
% pyspiel GameType and GameInfo

getGameTypes(GameType) :-
    findall([Attr, Val], gametype(Attr, Val), GameType).

getGameInfos(GameInfo) :-
    findall([Attr, Val], gameinfo(Attr, Val), GameInfo).

gametype(short_name, "prolog_nim").
gametype(long_name, "Prolog Nim").
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

gameinfo(num_distinct_actions, N) :-
    findall(X, action_ID_(_ID, X), L),
    length(L, N).
gameinfo(max_chance_outcomes, 0).
gameinfo(num_players, 2).
gameinfo(min_utility, -1).
gameinfo(max_utility, 1).
gameinfo(utility_sum, 0).
gameinfo(max_game_length, N) :-
    nim:start(pos(_, N)).
