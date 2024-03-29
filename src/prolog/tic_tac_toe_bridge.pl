:- use_module(tic_tac_toe, []).

% -----------------------------------------------------------------------------------------
% interface predicates
% called by PrologGame.py

init(InitState, Current_Player) :-
    Current_Player is 0,
    tic_tac_toe:initial(Board),
    InitState = [Current_Player, Board].

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

apply_actions(GameState, [], GameState) :- !.
apply_actions(GameState, [M|Oves], FinalState) :-
    apply_action(GameState, M, NextState),
    apply_actions(NextState, Oves, FinalState).

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

% action_string(Player, Action, String)
action_to_string(0, 0, "x(0,0)") :- !.
action_to_string(0, 1, "x(0,1)") :- !.
action_to_string(0, 2, "x(0,2)") :- !.
action_to_string(0, 3, "x(1,0)") :- !.
action_to_string(0, 4, "x(1,1)") :- !.
action_to_string(0, 5, "x(1,2)") :- !.
action_to_string(0, 6, "x(2,0)") :- !.
action_to_string(0, 7, "x(2,1)") :- !.
action_to_string(0, 8, "x(2,2)") :- !.
action_to_string(1, 0, "o(0,0)") :- !.
action_to_string(1, 1, "o(0,1)") :- !.
action_to_string(1, 2, "o(0,2)") :- !.
action_to_string(1, 3, "o(1,0)") :- !.
action_to_string(1, 4, "o(1,1)") :- !.
action_to_string(1, 5, "o(1,2)") :- !.
action_to_string(1, 6, "o(2,0)") :- !.
action_to_string(1, 7, "o(2,1)") :- !.
action_to_string(1, 8, "o(2,2)").

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

% -----------------------------------------------
% interface tests
% -----------------------------------------------

:- use_module(library(clpfd)).

:- begin_tests(tic_tac_toe_interface_tests).

test(init_current_player) :-
    init(InitState, Current_player),
    InitState = [Current_player, Board],
    Board = [['.','.','.'],['.','.','.'],['.','.','.']],
    Current_player == 0.

test(legal_actions1) :-
    Board = [['x','.','o'],['.','o','.'],['x','.','.']],
    GameState = [0, Board],
    legal_actions(GameState, Legal_actions),
    Legal_actions == [1,3,5,7,8].

test(legalactions2) :-
    Board = [['x','x','o'],['o','o','x'],['x','o','x']],
    GameState = [_, Board],
    legal_actions(GameState, Legal_actions),
    Legal_actions == [].

test(apply_action1) :-
    Board = [['x','.','o'],['.','o','.'],['x','.','.']],
    GameState = [0, Board],
    apply_action(GameState, 1, NewGameState),
    NewGameState = [_, NewBoard],
    NewBoard == [['x','x','o'],['.','o','.'],['x','.','.']].

test(apply_action2, [fail]) :-
    Board = [['x','.','o'],['.','o','.'],['x','.','.']],
    GameState = [0, Board],
    apply_action(GameState, 2, _NewBoard).

test(apply_action3, [fail]) :-
    Board = [['x','x','o'],['o','o','x'],['x','x','o']],
    GameState = [none, Board],
    apply_action(GameState, 2, _NewBoard).

test(is_terminal_no_more_moves) :-
    Board = [['x','x','o'],['o','o','x'],['x','x','o']],
    GameState = [_, Board],
    is_terminal(GameState).

test(is_terminal_winner) :-
    Board = [['x','o','.'],['x','.','o'],['x','.','.']],
    is_terminal([_, Board]).

test(is_terminal_init,[fail]) :-
    init(InitState, _),
    is_terminal(InitState).

test(is_terminal_not_finished, [fail]) :-
    Board = [['x','o','.'],['x','.','o'],['.','.','.']],
    is_terminal([_, Board]).


:- end_tests(tic_tac_toe_interface_tests).
