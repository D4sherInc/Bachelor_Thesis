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

% -----------------------------------------------
%  game logic tests
% -----------------------------------------------
:- use_module(library(plunit)).
:- begin_tests(tic_tac_toe_logic_tests).

test(win1_empty_fail, [fail]) :-
    Board = [['.','.','.'],
             ['.','.','.'],
             ['.','.','.']],
    wingame(Board, _Player).

test(win2_fully_played, [fail]) :-
    Board = [o, x, o,
             x, o, x,
             x, o, x],
    wingame(Board, _Player).

test(win3_horizontal, [nondet]) :-
    Board = [x,x,x,
             _,_,_,
             _,_,_],
    wingame(Board, x).

test(win4_horizontal2, [nondet]) :-
    Board = [_,_,_,
             o,o,o,
             _,_,_],
    wingame(Board, o).

test(win5_horizontal3, [nondet]) :-
    Board = [_,_,_,
             _,_,_,
             x,x,x],
    wingame(Board, x).

test(win6_vertical, [nondet]) :-
    Board = [o,_,_,
             o,_,_,
             o,_,_],
    wingame(Board, o).

test(win7_vertical, [nondet]) :-
    Board = [_,o,_,
             _,o,_,
             _,o,_],
    wingame(Board, o).

test(win8_vertical, [nondet]) :-
    Board = [_,_,o,
             _,_,o,
             _,_,o],
    wingame(Board, o).

test(win9_diagonal, [nondet]) :-
    Board = [x,_,_,
             _,x,_,
             _,_,x],
    wingame(Board, x).

test(win10_diagonal, [nondet]) :-
    Board = [_,_,x,
            _,x,_,
            x,_,_],
    wingame(Board, x).

test(move1_correct) :-
    Board = [['.','.','.'],
             ['.','.','.'],
             ['.','.','.']],
    Player = x,
    Move = 4,
    move(Board, Player, Move, NewBoard),
    NewBoard = [['.','.','.'],
                ['.', x ,'.'],
                ['.','.','.']].

test(move2_false, [fail]) :-
    Board = [[ x , o , x ],
             ['.', x ,'.'],
             [ o ,'.', o ]],
    Player = o,
    Move = 4,
    move(Board, Player, Move, _NewBoard).

test(move3_false, [fail]) :-
    Board = [[ x , o , x ],
             ['.', x ,'.'],
             [ o ,'.', o ]],
    Player = x,
    Move = 4,
    move(Board, Player, Move, _NewBoard).

:- end_tests(tic_tac_toe_logic_tests).
