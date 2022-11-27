% all applicable methods:
%   init (done)
%   current_player (dynamic:done)
%   legal_actions (dynamic:done)
%   apply_action (done)
%   action_to_string (optional, can by done in python)
%   is_terminal
%   returns


% tic_tac_toe
% zero_sum 2 player game

:- use_module(library(clpfd)).

% Interface predicates

:- dynamic board/1.
:- dynamic current_player/1.
:- dynamic legal_actions/1.

% init(-InitState, -Current_Player, -player0_score)
% initilization of board, starting player, player0_score
% reset by retracting (deleting) all  currently existing states
init(InitState, 0, 0) :-
    reset_game_,
    board(InitState),
    assert(current_player(0)).

% apply action to current board
% save the New Game State, remove the old
% swap players
% update legal moves by removing the one used action
apply_action(GameState, Move, NewGameState) :-
    legal_actions(Legal_Actions),
    member(Move, Legal_Actions),
    !,
    board(GameState),
    current_player(Current_Player_ID),
    player_(current_player(Current_Player_ID), Current_Player_Symbol),
    move(GameState, Current_Player_Symbol, Move, NewGameState),

    retract(board(GameState)),
    assert(board(NewGameState)),

    retract(current_player(Current_Player_ID)),
    other_player(Current_Player_Symbol, Other_Player_Symbol),
    player_(current_player(Other_Player_ID), Other_Player_Symbol),
    assert(current_player(Other_Player_ID)),
    update_legal_actions_(NewGameState, Move).

is_terminal(Board) :-
    \+ member(0, Board), !.
is_terminal(Board) :-
    wingame(Board, _).

% internal predicate
% resets all dynamic predicates
% sets a new blank board and all actions as legal
reset_game_ :-
    retractall(board(_)),
    retractall(current_player(_)),
    retractall(legal_actions(_)),

    assert(board([0,0,0,0,0,0,0,0,0])),
    assert(legal_actions([0,1,2,3,4,5,6,7,8])).

% translate PlayerID to symbol to play
player_(current_player(0), x).
player_(current_player(1), o).

% ------------------------------------------
% actual game implementation

% board([0,0,0,0,0,0,0,0,0]).

legal_moves(Board, Legal_moves) :-
	legal_moves(Board, 1, Legal_moves).

legal_moves([], _, []).
legal_moves([0|T], N, [N|Tail]) :-
	!,
	M is N + 1,
	legal_moves(T, M, Tail).
legal_moves([_|T], N, Tail) :-
	M is N + 1,
	legal_moves(T, M, Tail).


% update: if one player won / board is full
update_legal_actions_(Board, _) :-
    is_terminal(Board), !,
    retractall(legal_actions(_)),
    assert(legal_actions([])).
% if game is not finished: remove used move from legal actions
update_legal_actions_(_, Move) :-
    legal_actions(Legal_Actions),
    nth0(_,Legal_Actions, Move, NL), !,
    retract(legal_actions(Legal_Actions)),
    assert(legal_actions(NL)).


wingame(Board, Player) :-
    dif(Player, 0),
    (win_lines(Board, Player); win_columns(Board, Player); win_diagonal(Board, Player)).

win_lines([X,X,X|_], X) :- !.
win_lines([_,_,_|Tail], X) :-
    win_lines(Tail, X).

win_columns([X,_,_, X,_,_, X,_,_], X) :- !.
win_columns([_,X,_, _,X,_, _,X,_], X) :- !.
win_columns([_,_,X, _,_,X, _,_,X], X).

win_diagonal([  X,_,_,
                _,X,_,
                _,_,X], X) :- !.
win_diagonal([  _,_,X,
                _,X,_,
                X,_,_], X).

display([A,B,C,D,E,F,G,H,I]) :-
	format('~w~n~w~n~w~n', [[A,B,C], [D,E,F], [G,H,I]]).

other_player(x, o) :- !.
other_player(o, x).

move([0,B,C,D,E,F,G,H,I], Player, 0, [Player,B,C,D,E,F,G,H,I]).
move([A,0,C,D,E,F,G,H,I], Player, 1, [A,Player,C,D,E,F,G,H,I]).
move([A,B,0,D,E,F,G,H,I], Player, 2, [A,B,Player,D,E,F,G,H,I]).
move([A,B,C,0,E,F,G,H,I], Player, 3, [A,B,C,Player,E,F,G,H,I]).
move([A,B,C,D,0,F,G,H,I], Player, 4, [A,B,C,D,Player,F,G,H,I]).
move([A,B,C,D,E,0,G,H,I], Player, 5, [A,B,C,D,E,Player,G,H,I]).
move([A,B,C,D,E,F,0,H,I], Player, 6, [A,B,C,D,E,F,Player,H,I]).
move([A,B,C,D,E,F,G,0,I], Player, 7, [A,B,C,D,E,F,G,Player,I]).
move([A,B,C,D,E,F,G,H,0], Player, 8, [A,B,C,D,E,F,G,H,Player]).