:- use_module(connect4, []).

% -----------------------------------------------------------------------------------------
% interface predicates
% called by PrologGame.py

init(InitState, Current_Player)  :-
    connect4:initial(board(Board)),
    InitState =[Current_Player, Board],
    Current_Player = 0.

legal_actions(GameState, Legal_Actions) :-
    GameState = [_, Board],
    Board = [[P0|_],[P1|_],[P2|_],[P3|_],[P4|_],[P5|_],[P6|_]],
    findall(Index, nth0(Index, [P0, P1, P2, P3, P4, P5, P6], '-'), Legal_Actions).

apply_action(GameState, A_ID, NewGameState) :-
    GameState = [Current_Player_ID, Board],
    NewGameState = [Next_Player_ID, NewBoard],
    player_ID_(Current_Player_ID, P1),
    connect4:other_player(P1, P2),
    player_ID_(Next_Player_ID, P2),
    connect4:play(P1, A_ID, board(Board), board(NewBoard)).

is_terminal([_, Board]) :-
    connect4:full(board(Board)), !.

is_terminal([Current_Player_ID, Board]) :-
    player_ID_(Current_Player_ID, P1),
    connect4:wins(P1, board(Board)).

is_terminal([Current_Player_ID, Board]) :-
    player_ID_(Current_Player_ID, P1),
    connect4:other_player(P1, P2),
    connect4:wins(P2, board(Board)).


returns([_, Board], Winner_ID, 1) :-
    dif(Winner, '-'),
    connect4:wins(Winner, board(Board)),
    player_ID_(Winner_ID, Winner).

returns(_, 'X', 0).

% action_to_string(Player, Action, String)
action_to_string(0, 0, "X to Column 0") :- !.
action_to_string(0, 1, "X to Column 1") :- !.
action_to_string(0, 2, "X to Column 2") :- !.
action_to_string(0, 3, "X to Column 3") :- !.
action_to_string(0, 4, "X to Column 4") :- !.
action_to_string(0, 5, "X to Column 5") :- !.
action_to_string(0, 6, "X to Column 6") :- !.
action_to_string(1, 0, "O to Column 0") :- !.
action_to_string(1, 1, "O to Column 1") :- !.
action_to_string(1, 2, "O to Column 2") :- !.
action_to_string(1, 3, "O to Column 3") :- !.
action_to_string(1, 4, "O to Column 4") :- !.
action_to_string(1, 5, "O to Column 5") :- !.
action_to_string(1, 6, "O to Column 6").

% -----------------------------------------------------------------------------------------
% internal predicats

player_ID_(0, 'X').
player_ID_(1, 'O').

other_player('X', 'O').
other_player('O', 'X').

% -----------------------------------------------------------------------------------------
% pyspiel GameInfo and GameType

getGameTypes(GameType) :-
    findall([Attr, Val], gametype(Attr, Val), GameType).

getGameInfos(GameInfo) :-
    findall([Attr, Val], gameinfo(Attr, Val), GameInfo).

gametype(short_name, "prolog_connect4").
gametype(long_name, "Prolog Connect 4").
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

gameinfo(num_distinct_actions, N) :-
    connect4:initial(board(Board)),
    length(Board, N).
gameinfo(max_chance_outcomes, 0).
gameinfo(num_players, 2).
gameinfo(min_utility, -1).
gameinfo(max_utility, 1).
gameinfo(utility_sum, 0).
gameinfo(max_game_length, N) :-
    connect4:initial(board(Board)),
    length(Board, L1),
    Board = [C|_],
    length(C, L2),
    N is L1 * L2.
