% iterated prisoners dilemma
% simultaneous non-zero-sum game

% both players decide to either 1) cooperate with or 2) defect the other player
% based on both players decisions, every player gets points based on the rewards table
% multiple rounds, ends on random chance -> randomized number of rounds
% goal: maximize own points over multiple playthroughs against the same opponent

% general rule: 
% defecting a cooperating player > both cooperating > both defecting > getting defected

% c = cooperate 
% d = defect 
% example for reward table: 
% p1/p2|  c    d
% -----|---------
%	c  | 3/3  0/5
%	d  | 5/0  1/1

% reward table
rewards(prisoner1-cooperate, prisoner2-cooperate, 3-3):- !.
rewards(prisoner1-cooperate, prisoner2-defect, 0-5):- !.
rewards(prisoner1-defect, prisoner2-cooperate, 5-0):- !.
rewards(prisoner1-defect, prisoner2-defect, 1-1):- !.

% possible actions
action(0, cooperate, c).
action(1, defect, d).

% random chance to end the game after every simultaneous action
random_ending_chance(0.125).

game_state(Continue_or_end) :-
	random(0.0,1.0, R),
	game_state(R, Continue_or_end).

game_state(R, continue) :-
	random_ending_chance(REC),
	R >= REC, !,
	format('game continues~n----------------------~n', []).
game_state(_, end):-
	format('game ends~n----------------------~n',[]).
	
% play with standard values: 
% game ending chance = 1/8
% moves per player completely randomized
play :-
	format('starting game: iterated prisoners dilemma~n',[]),
	play(player(prisoner1, 0, []), player(prisoner2, 0, []), continue).

% end was chosen by chance
play(player(P1, P1_Points, P1_History), player(P2, P2_Points, P2_History), end) :- !,
	format('~nfinal History: ~n~w: ~w~n~w: ~w~n',[P1, P1_History, P2, P2_History]),
	format('~nfinal points: ~n~w: ~w~n~w: ~w~n', [P1, P1_Points, P2, P2_Points]).

% game continues by chance
play(player(P1, P1_Points, P1_History), player(P2, P2_Points, P2_History), continue) :-
	% 1) choose random moves for both players
	random(0,2,P1_Move),
	random(0,2,P2_Move),
	action(P1_Move, P1_Move_translated, P1M),
	action(P2_Move, P2_Move_translated, P2M),
	format('chosen moves: ~n~w: ~w~n~w: ~w~n~n',[P1, P1_Move_translated,P2, P2_Move_translated]),

	% 2) add points and history of moves
	rewards(P1-P1_Move_translated, P2-P2_Move_translated, P1_bonus_points-P2_bonus_points),
	New_P1_Points is P1_Points + P1_bonus_points,
	New_P2_Points is P2_Points + P2_bonus_points,
	append(P1_History, [P1M], New_P1_History),
	append(P2_History, [P2M], New_P2_History),

	% 3) calculate by chance if game continues
	game_state(Continue_or_end),

	% 4) continue or end game
	play(player(P1, New_P1_Points, New_P1_History), player(P2, New_P2_Points, New_P2_History), Continue_or_end).