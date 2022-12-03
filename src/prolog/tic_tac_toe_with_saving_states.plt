:- module(tic_tac_toe, []).
:- use_module(library(clpfd)).
:- use_module(library(plunit)).

:- begin_tests(tic_tac_toe_tests).

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

test(update_legal_actions_,[setup(debug_set_legal_actions_([0,1,2,3,5,6,7])),
                            cleanup(debug_retractall_)]) :-
    % update_legal_actions_(5),
    legal_actions(I),
    \+ legal_actions([0,1,2,3,5,6,7]).

:- end_tests(tic_tac_toe_tests).