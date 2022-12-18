:- module(tic_tac_toe, []).
:- use_module(library(clpfd)).

% -----------------------------------------------------------------------------------------
% game logic

initial([['.','.','.'],['.','.','.'],['.','.','.']]).

wingame(Board, Player) :-
    dif(Player, '.'),
    (   (win_lines(Board, Player), !);
        (win_columns(Board, Player), !);
        (win_diagonal(Board, Player), !)).

win_lines([[X,X,X]|_], X) :- !.
win_lines([_|Tail], X) :-
    win_lines(Tail, X).

win_columns([[X,_,_], [X,_,_], [X,_,_]], X) :- !.
win_columns([[_,X,_], [_,X,_], [_,X,_]], X) :- !.
win_columns([[_,_,X], [_,_,X], [_,_,X]], X).

win_diagonal([  [X,_,_],
                [_,X,_],
                [_,_,X]], X) :- !.
win_diagonal([  [_,_,X],
                [_,X,_],
                [X,_,_]], X).

other_player(x, o) :- !.
other_player(o, x).

move([['.',B,C],[D,E,F],[G,H,I]], Player, 0, [[Player,B,C],[D,E,F],[G,H,I]]).
move([[A,'.',C],[D,E,F],[G,H,I]], Player, 1, [[A,Player,C],[D,E,F],[G,H,I]]).
move([[A,B,'.'],[D,E,F],[G,H,I]], Player, 2, [[A,B,Player],[D,E,F],[G,H,I]]).
move([[A,B,C],['.',E,F],[G,H,I]], Player, 3, [[A,B,C],[Player,E,F],[G,H,I]]).
move([[A,B,C],[D,'.',F],[G,H,I]], Player, 4, [[A,B,C],[D,Player,F],[G,H,I]]).
move([[A,B,C],[D,E,'.'],[G,H,I]], Player, 5, [[A,B,C],[D,E,Player],[G,H,I]]).
move([[A,B,C],[D,E,F],['.',H,I]], Player, 6, [[A,B,C],[D,E,F],[Player,H,I]]).
move([[A,B,C],[D,E,F],[G,'.',I]], Player, 7, [[A,B,C],[D,E,F],[G,Player,I]]).
move([[A,B,C],[D,E,F],[G,H,'.']], Player, 8, [[A,B,C],[D,E,F],[G,H,Player]]).
