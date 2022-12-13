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
:- dynamic is_terminal/0.

% init(-InitState, -Current_Player, -player0_score)
% initilization of board, starting player, player0_score
% reset by retracting (deleting) all  currently existing states
% TODO: check for different starting players (0;1)
init(InitState, 0, 0) :-
    reset_game_,
    board(InitState).

% apply action to current board
% remove the old GameState, save the New Game State
% swap players (save them)
% update legal moves by removing the one used action
apply_action(GameState, _, "Prolog Custom Error: current GameState not the same as saved!") :-
    \+ board(GameState), !.
apply_action(_, Move, "Prolog Custom Error: given Move not legal!") :-
    legal_actions(Legal_actions),
    \+ member(Move, Legal_actions), !.
apply_action(GameState, Move, NewGameState) :-
    legal_actions(Legal_Actions),
    member(Move, Legal_Actions),
    board(GameState),
    current_player(Current_Player_ID),
    player_(current_player(Current_Player_ID), Current_Player_Symbol),
    move(GameState, Current_Player_Symbol, Move, NewGameState),

%    update_game_states
    retract(board(GameState)),
    assert(board(NewGameState)),

    retract(current_player(Current_Player_ID)),
    other_player(Current_Player_Symbol, Other_Player_Symbol),
    player_(current_player(Other_Player_ID), Other_Player_Symbol),
    assert(current_player(Other_Player_ID)),

    update_legal_actions_(NewGameState, Move).

is_terminal(Board) :-
    flatten(Board, FlattendBoard),
    findall(I, nth0(I, FlattendBoard, '.'), []),
    !,
    assert(is_terminal).

is_terminal(Board) :-
    wingame(Board, _),
    assert(is_terminal).

returns(Points) :-
    board(Board),
    wingame(Board, Player),
    !,
    (Player == "x"
    -> Points = [1, -1] ; Points = [-1, 1]).
 returns([0,0]).

% ------------------------------------------

% internal predicate
% resets all dynamic predicates
% sets a new blank board and all actions as legal
reset_game_ :-
    % reset dynamic predicates
    retractall(board(_)),
    retractall(current_player(_)),
    retractall(legal_actions(_)),
    retractall(is_terminal),

    assert(board([['.','.','.'],['.','.','.'],['.','.','.']])),
    assert(current_player(0)),
    assert(legal_actions([0,1,2,3,4,5,6,7,8])).

% translate PlayerID to symbol to play
player_(current_player(0), x).
player_(current_player(1), o).

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

% ------------------------------------------
% actual game logic

wingame(Board, Player) :-
    dif(Player, '.'),
    (win_lines(Board, Player); win_columns(Board, Player); win_diagonal(Board, Player)).

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

display([Row1, Row2, Row3]) :-
	format('~w~n~w~n~w~n', [Row1, Row2, Row3]).

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