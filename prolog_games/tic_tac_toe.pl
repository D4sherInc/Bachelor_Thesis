% tic_tac_toe
% zero_sum 2 player game

:- use_module(library(clpfd)).

board([0,0,0,0,0,0,0,0,0]).

start_legal_moves([1,2,3,4,5,6,7,8,9]).
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

other_player(x, o).
other_player(o, x).

move([0,B,C,D,E,F,G,H,I], Player, 1, [Player,B,C,D,E,F,G,H,I]).
move([A,0,C,D,E,F,G,H,I], Player, 2, [A,Player,C,D,E,F,G,H,I]).
move([A,B,0,D,E,F,G,H,I], Player, 3, [A,B,Player,D,E,F,G,H,I]).
move([A,B,C,0,E,F,G,H,I], Player, 4, [A,B,C,Player,E,F,G,H,I]).
move([A,B,C,D,0,F,G,H,I], Player, 5, [A,B,C,D,Player,F,G,H,I]).
move([A,B,C,D,E,0,G,H,I], Player, 6, [A,B,C,D,E,Player,G,H,I]).
move([A,B,C,D,E,F,0,H,I], Player, 7, [A,B,C,D,E,F,Player,H,I]).
move([A,B,C,D,E,F,G,0,I], Player, 8, [A,B,C,D,E,F,G,Player,I]).
move([A,B,C,D,E,F,G,H,0], Player, 9, [A,B,C,D,E,F,G,H,Player]).