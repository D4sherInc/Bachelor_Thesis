% all applicable methods:
%   init (done)
%   current_player (done)
%   legal_actions (done)
%   apply_action (done with catching error: illegal move->NewGameState stays variable)
%   action_to_string (optional, can by done in python)
%   is_terminal
%   returns


% tic_tac_toe
% zero_sum 2 player game

:- use_module(library(clpfd)).

% Interface predicates

% init(-InitState, -Current_Player, -player0_score)
% initilization of board, starting player, player0_score
% reset by retracting (deleting) all  currently existing states
% TODO: check for different starting players (0;1)
init(InitState, 0, 0) :-
    InitState = [['.','.','.'],['.','.','.'],['.','.','.']].

% calculate current player based on board
current_player(Board, Current_player) :-
    flatten(Board, FlattendBoard),
    exclude(=('.'), FlattendBoard, L),
    length(L, N),
    current_player(N, Current_player, _Current_player_ID).
current_player(9, none, -1):- !.
current_player(N, x, 0) :-
    0 is N mod 2, !.
current_player(_, o, 1).

% calculate legal actions based on board
legal_actions(Board, Legal_actions) :-
    flatten(Board, FlattendBoard),
    findall(Index, nth0(Index, FlattendBoard, '.'), Legal_actions).

% apply given action to given board
% return board from next game state
apply_action(GameState, Move, NewGameState) :-
    catch(try_apply_action_(GameState, Move, NewGameState), Error, process_error(Error)).

try_apply_action_(GameState, Move, _) :-
    legal_actions(GameState, Legal_actions),
    \+ member(Move, Legal_actions), !,
    throw(illegal_action).


try_apply_action_(GameState, Move, NewGameState) :-
    current_player(GameState, Current_player),
    dif(Current_player, none),
    move(GameState, Current_player, Move, NewGameState).


% calculate terminal state based on board
is_terminal(Board) :-
    flatten(Board, FlattendBoard),
    findall(I, nth0(I, FlattendBoard, '.'), []), !.

is_terminal(Board) :-
    wingame(Board, _).

% ------------------------------------------

% internal predicate

% translate PlayerID to symbol to play
player_(current_player(0), x).
player_(current_player(1), o).

process_error(illegal_action) :-
    write("Prolog Custom Error: given Move not legal on current board!").

% ------------------------------------------
% actual game logic

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