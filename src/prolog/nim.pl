% all applicable methods:
%   init
%   current_player
%   legal_actions
%   apply_action
%   is_terminal
%   returns


% init(-InitState, -Current_player, -player0_score)
init(InitState, Pl, 0) :-
    start(InitState),
    InitState = pos(Pl, _Sticks).

% current_player(+GameState, -Current_player)
current_player(pos(P1, _), P1).

% legal_actions(+GameState, -Legal_actions)
legal_actions(pos(_, N), [take1, take2]):-
    dif(N, 0).

% apply_action(+GameState, +Action, -NewGameState)
apply_action(GameState, Action, NewGameState) :-
    trans(Action, GameState, NewGameState).

% is_terminal(+GameState)
is_terminal(GameState) :-
    win(GameState, _Winner).

% returns(GameState, Player, Points)
returns(GameState, Player, 1):-
    win(GameState, Player), !.
returns(_, min, 0).

% ------------------------------------------
% actual game logic

start(pos(max,9)).

trans(A,P,P2) :-
    game_move(P,A,P2).

game_move(pos(P1,M),take1,pos(P2,M1)) :-
    M>0,
    M1 is M-1,
    other_player(P1,P2).

game_move(pos(P1,M),take2,pos(P2,M1)) :-
    M>1,
    M1 is M-2,
    other_player(P1,P2).

player(pos(P,_B),P).


win(pos(B,0),B).


other_player(min, max).
other_player(max, min).

% ------------------------------------------
% pyspiel GameType and GameInfo stuff
