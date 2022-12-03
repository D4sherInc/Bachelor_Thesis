:- module(tic_tac_toe, []).
:- use_module(library(clpfd)).
:- use_module(library(plunit)).


% -----------------------------------------------
%  game logic tests
% -----------------------------------------------
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

% -----------------------------------------------
% interface tests
% -----------------------------------------------

:- begin_tests(tic_tac_toe_interface_tests).

test(init_current_player) :-
    init(InitState, Current_player, _),
    InitState == [['.','.','.'],['.','.','.'],['.','.','.']],
    Current_player == 0.

test(legal_actions1) :-
    Board = [['x','.','o'],['.','o','.'],['x','.','.']],
    legal_actions(Board, Legal_actions),
    Legal_actions == [1,3,5,7,8].

test(legalactions2) :-
    Board = [['x','x','o'],['o','o','x'],['x','o','x']],
    legal_actions(Board, Legal_actions),
    Legal_actions == [].

test(apply_action1) :-
    Board = [['x','.','o'],['.','o','.'],['x','.','.']],
    apply_action(Board, 1, NewBoard),
    NewBoard == [['x','x','o'],['.','o','.'],['x','.','.']].

test(apply_action2, [fail]) :-
    Board = [['x','.','o'],['.','o','.'],['x','.','.']],
    apply_action(Board, 2, NewBoard),
    NewBoard == [['x','x','o'],['.','o','.'],['x','.','.']].

test(apply_action3, [fail]) :-
    Board = [['x','x','o'],['o','o','x'],['x','x','o']],
    apply_action(Board, 2, NewBoard),
    NewBoard == [['x','x','o'],['.','o','.'],['x','.','.']].

test(is_terminal_no_more_moves) :-
    Board = [['x','x','o'],['o','o','x'],['x','x','o']],
    is_terminal(Board).

test(is_terminal_winner) :-
    Board = [['x','o','.'],['x','.','o'],['x','.','.']],
    is_terminal(Board).

test(is_terminal_init,[fail]) :-
    InitState == [['.','.','.'],['.','.','.'],['.','.','.']],
    is_terminal(InitState).

test(is_terminal_not_finished, [fail]) :-
    Board = [['x','o','.'],['x','.','o'],['.','.','.']],
    is_terminal(Board).

:- end_tests(tic_tac_toe_interface_tests).
