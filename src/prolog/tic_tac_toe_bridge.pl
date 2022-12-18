:- use_module(tic_tac_toe, []).
% -----------------------------------------------------------------------------------------
% interface predicates
% called by PrologGame.py

init(InitState, Current_Player) :-
    Current_Player is 0,
    tic_tac_toe:initial(Board),
    InitState = [Current_Player, Board].

current_player([_, Current_Player], Current_Player).

legal_actions(GameState, Legal_actions) :-
    GameState = [_, Board],
    flatten(Board, FlattendBoard),
    findall(Index, nth0(Index, FlattendBoard, '.'), Legal_actions).

apply_action(GameState, Move, NewGameState) :-
    GameState = [Current_Player, Board],
    dif(Current_Player, none),
    player_ID_(Current_Player, Pl_Symbol),
    tic_tac_toe:move(Board, Pl_Symbol, Move, NewBoard),
    tic_tac_toe:other_player(Pl_Symbol, Pl2_Symbol),
    player_ID_(Next_Player, Pl2_Symbol),
    NewGameState = [Next_Player, NewBoard].

is_terminal(GameState) :-
    GameState = [_, Board],
    flatten(Board, FlattendBoard),
    findall(I, nth0(I, FlattendBoard, '.'), []), !.

is_terminal(GameState) :-
    GameState = [_, Board],
    tic_tac_toe:wingame(Board, _).

returns(GameState, Player_ID, 1) :-
    GameState = [_, Board],
    tic_tac_toe:wingame(Board, Player), !,
    player_ID_(Player_ID, Player).
returns(_,x,0).

% -----------------------------------------------------------------------------------------
% internal predicate

player_ID_(0, x).
player_ID_(1, o).

process_error(illegal_action) :-
    write("Prolog Custom Error: given Move not legal on current board!").

% -----------------------------------------------------------------------------------------
% pyspiel GameType and GameInfo

getGameTypes(GameType) :-
    findall([Attr, Val], gametype(Attr, Val), GameType).

getGameInfos(GameInfo) :-
    findall([Attr, Val], gameinfo(Attr, Val), GameInfo).

gametype(short_name, "prolog_tic_tac_toe").
gametype(long_name, "Prolog Tic-Tac-Toe").
gametype(dynamics, sequential).
gametype(chance_mode, deterministic).
gametype(information, perfect_information).
gametype(utility, zero_sum).
gametype(reward_model, terminal).
gametype(max_num_players, 2).
gametype(min_num_players, 2).
gametype(provides_information_state_string, "True").
gametype(provides_information_state_tensor, "False").
gametype(provides_observation_string, "True").
gametype(provides_observation_tensor, "True").
gametype(parameter_specification, "{}").

gameinfo(num_distinct_actions, 9).
gameinfo(max_chance_outcomes, 0).
gameinfo(num_players, 2).
gameinfo(min_utility, -1).
gameinfo(max_utility, 1).
gameinfo(utility_sum, 0).
gameinfo(max_game_length, 9).
