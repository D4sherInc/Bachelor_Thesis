% kuhn Poker

% starting deck: 3 cards (jack, queen, king)
% pot for rewards, both players "ant" (put rewards in pot)
% at showdown: higher card wins (J < Q < K)

% 1) P1, P2 ant
% 2) P1, P2 draw a card (third card is removed from play)
% 3) P1 decides: check or bet
%		a) P1 checks -> P2 decides: check or bet
%			aa) P2 checks -> showdown for the pot
%			ab) P2 bets -> P2 ants, P1 can fold or call
%				aba) P1 folds -> P2 wins Pot
%				abb) P1 calls -> P1 ants, showdown
%		b) P1 bets -> P1 ants, P2 decides: fold or call
%			ba) P2 folds -> P1 wins pot
%			bb) P2 calls -> P2 ants, showdown

% P1: manual player
% P2: bot



% all applicable methods:
%   init (done)
%   current_player
%   legal_actions [0,1,2]
%   apply_action
%   action_to_string (optional, can by done in python)
%   is_terminal
%   returns

% ------------------------------------------
% interface predicates
% init(-InitState, -Current_Player, -player0_score)
init(state(pot(), hand(p1,), hand(p2)), Current_Player, 0) :-


current_player() :-

legal_actions() :-

apply_action() :-

is_terminal() :-

returns() :-



% ------------------------------------------
% actual game implementation

deck([1-jack, 2-queen, 3-king]).
pot(0).

players(p1, p2).

ant(N, NN) :-
	NN is N + 1.

play :-
	players(P1, P2),
	deck(Deck),
	pot(Pot),
	format('playing a round of kuhn poker~n', []),
	% 1) P1, P2 ant 
	ant(Pot, Pot2),
	ant(Pot2, Pot3),

	% 2) P1, P2 draw a card
	random_member(N-Card, Deck),
	select(N-Card, Deck, Deck2),
	random_member(M-Card2, Deck2),
	Player1 = player(P1, N-Card),
	Player2 = player(P2, M-Card2),


	% 3) P1 decides: check or bet
	action1(Player1, P1_Action),
	apply_action(Player1, Player2, [P1_Action], Pot3).

% a) 
apply_action(Player1, Player2, [check], Pot) :-
	action2(Player2, Action),
	apply_action(Player1, Player2, [check, Action], Pot).

% aa)
apply_action(Player1, Player2, [check, check], Pot) :-
	showdown(Player1, Player2, Pot).

% ab)
apply_action(Player1, Player2, [check, bet], Pot) :-
	ant(Pot, Pot2),
	action3(Player1, Action),
	apply_action(Player1, Player2, [check, bet, Action], Pot2).

% aba)
apply_action(_, player(P2, _-Card), [check, bet, fold], Pot) :- 
	format('Player 1 folds.~w wins the Pot of ~w with the ~w', [P2, Pot, Card]).

% abb)
apply_action(Player1, Player2, [check, bet, call], Pot) :-
	ant(Pot, Pot2),
	showdown(Player1, Player2, Pot2).

% b)
apply_action(Player1, Player2, [bet], Pot) :-
	ant(Pot, Pot2),
	action4(Player2, Action),
	apply_action(Player1, Player2, [bet, Action], Pot2).

% ba)
apply_action(player(P1, _-Card), _, [bet, fold], Pot) :-
	format('Player 2 folds. ~w wins the Pot of ~w with the ~w',[P1, Pot, Card]).

% bb)
apply_action(Player1, Player2, [bet, call], Pot) :-
	ant(Pot, Pot2),
	showdown(Player1, Player2, Pot2).

showdown(player(P1, N-Card), player(_, M-_), Pot) :-
	N > M, !,
	format('~w wins showdown and the Pot of ~w with his ~w', [P1, Pot, Card]).

showdown(_, player(P2, _-Card), Pot) :-
	format('~w wins showdown and the Pot of ~w with his ~w', [P2, Pot, Card]).


action1(player(_, _-Card), Input) :-
	format('your card is ~w.~nwhat is your move? check or bet?~n',[Card]),
	read(Input),
	member(Input, [check, bet]).

% hard coded bot actions
action2(player(_, 1-jack), check).
action2(player(_, 2-queen), bet).
action2(player(_, 3-king), bet).

action3(_, Input) :-
	format('you checked, player 2 bet, what is your move?~nfold or call?~n',[]),
	read(Input),
	member(Input, [fold, call]).

% hard coded bot actions
action4(player(_, 1-jack), fold).
action4(player(_, 2-queen), call).
action4(player(_, 3-king), call).
