:- use_module(tic_tac_toe, [initial/1, wingame/2, other_player/2, move/4]).

% gamestate(UniqueHash, Board, CurrentPlayerID, LegalActionsList, Winner)
:- dynamic gamestate/5.

init(Hash) :-
    initial(Board),
    term_hash(Board, Hash),
    InitState = gamestate(Hash, Board, 0, [0,1,2,3,4,5,6,7,8], none),
    try_assert_(InitState).

legal_actions(Hash, LegalActions) :-
    gamestate(Hash, _, _, LegalActions, _).

apply_action(Hash, Move, NewHash) :-
    gamestate(Hash, Board, Current_Player_ID, _, none),
    player_ID_(Current_Player_ID, Current_Player),
    move(Board, Current_Player, Move, NewBoard),
    other_player(Current_Player, Next_Player),
    player_ID_(Next_Player_ID, Next_Player),
    legal_actions_(NewBoard, NewLegalActions, Winner),
    term_hash(NewBoard, NewHash),
    NewGameState = gamestate(NewHash, NewBoard, Next_Player_ID, NewLegalActions, Winner),
    try_assert_(NewGameState).

is_terminal(Hash) :-
    gamestate(Hash, _, _, [], _), !.

returns(Hash, [0, 0]) :-
    gamestate(Hash, _, _ , _, none), !.

returns(Hash, [P1, -P1]) :-
    gamestate(Hash, _, _, [], Winner),
    (Winner =:= "x" -> P1 is 1; P1 is -1).

% -----------------------------------------------------------------------------------------
% internal predicates
legal_actions_(Board, [], Winner_ID) :-
    wingame(Board, Winner),
    other_player(Winner_ID, Winner).


legal_actions_(Board, LegalActions, none) :-
    flatten(Board, FlattendBoard),
    findall(Index, nth0(Index, FlattendBoard, '.'), LegalActions).

% player_ID_(Player_ID_, Player_Symbol)
player_ID_(0, x).
player_ID_(1, o).


% checks if gamestate is already saved in database -> no add needed if already saved
try_assert_(gamestate(Hash, _, _, _, _)) :-
    gamestate(Hash, _, _, _, _), !.
try_assert_(AssertMe) :-
    assertz(AssertMe).

% -----------------------------------------------------------------------------------------
% pyspiel GameType and GameInfo

getGameTypes(GameType) :-
    findall([Attr, Val], gametype(Attr, Val), GameType).

getGameInfos(GameInfo) :-
    findall([Attr, Val], gameinfo(Attr, Val), GameInfo).

gametype(short_name, 'prolog_tic_tac_toe').
gametype(long_name, 'Prolog Tic-Tac-Toe').
gametype(dynamics, sequential).
gametype(chance_mode, deterministic).
gametype(information, perfect_information).
gametype(utility, zero_sum).
gametype(reward_model, terminal).
gametype(max_num_players, 2).
gametype(min_num_players, 2).
gametype(provides_information_state_string, 'True').
gametype(provides_information_state_tensor, 'False').
gametype(provides_observation_string, 'True').
gametype(provides_observation_tensor, 'True').
gametype(parameter_specification, '{}').

gameinfo(num_distinct_actions, 9).
gameinfo(max_chance_outcomes, 0).
gameinfo(num_players, 2).
gameinfo(min_utility, -1).
gameinfo(max_utility, 1).
gameinfo(utility_sum, 0).
gameinfo(max_game_length, 9).