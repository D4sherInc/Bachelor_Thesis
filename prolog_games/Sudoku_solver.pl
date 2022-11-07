% Prolog Sudoku Solver (C) 2007 Markus Triska (triska@gmx.at)
% Public domain code.

:- use_module(library(bounds)).

% Pss is a list of lists representing the game board.

sudoku(Pss) :-
    flatten(Pss, Ps),
    Ps in 1..9,
    maplist(all_different, Pss),
    Pss = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    columns(R1, R2, R3, R4, R5, R6, R7, R8, R9),
    blocks(R1, R2, R3), blocks(R4, R5, R6), blocks(R7, R8, R9),
    label(Ps),
    format('~w~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n', [R1,R2,R3,R4,R5,R6,R7,R8,R9]).

columns([], [], [], [], [], [], [], [], []).
columns([A|As],[B|Bs],[C|Cs],[D|Ds],[E|Es],[F|Fs],[G|Gs],[H|Hs],[I|Is]) :-
    all_different([A,B,C,D,E,F,G,H,I]),
    columns(As, Bs, Cs, Ds, Es, Fs, Gs, Hs, Is).

blocks([], [], []).
blocks([X1,X2,X3|R1], [X4,X5,X6|R2], [X7,X8,X9|R3]) :-
    all_different([X1,X2,X3,X4,X5,X6,X7,X8,X9]),
    blocks(R1, R2, R3).

/* sudoku([ [ 5, 3,A3,A4, 7,A6,A7,A8,A9],
  *         [ 6,B2,B3, 1, 9, 5,B7,B8,B9],
  *         [C1, 9, 8,C4,C5,C6,C7, 6,C9],
  *         [ 8,D2,D3,D4, 6,D6,D7,D8, 3],
  *         [ 4,E2,E3, 8,E5, 3,E7,E8, 1],
  *         [ 7,F2,F3,F4, 2,F6,F7,F8, 6],
  *         [G1, 6,G3,G4,G5,G6, 2, 8,G9],
  *         [H1,H2,H3, 4, 1, 9,H7,H8, 5],
  *         [I1,I2,I3,I4, 8,I6,I7, 7, 9]]).
   */