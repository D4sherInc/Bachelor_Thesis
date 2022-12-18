:- module(nim, []).
% ------------------------------------------
% actual game logic

start(pos(max,6)).

trans(A,P,P2) :-
    game_move(P,A,P2).

game_move(pos(P1,M),take1,pos(P2,M1)) :-
    M>0,
    M1 is M-1,
    other_player(P1,P2).

game_move(pos(P1,M),take2,pos(P2,M1)) :-
    M>0,
    M1 is M-2,
    other_player(P1,P2).

game_move(pos(P1,M),take3,pos(P2,M1)) :-
    M>0,
    M1 is M-3,
    other_player(P1,P2).

player(pos(P,_B),P).


win(pos(B, N), B) :-
    N =< 0.

other_player(min, max).
other_player(max, min).

