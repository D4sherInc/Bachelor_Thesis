# Prolog Games

Hier befinden sich die in Prolog definierten Spiele. Von 
[OpenSpiel](https://github.com/deepmind/open_spiel/tree/master/open_spiel/python/games) sind folgende Spiele 
für die eigene Entwicklung und Implementierung von Algorithmen empfohlen und hier in Prolog definiert:

* [Kuhn_Poker](https://github.com/D4sherInc/Bachelor_Thesis/blob/master/prolog_games/kuhn_poker.pl):
ein auf 3 Karten reduziertes Pokerspiel. 2 Spieler, abwechselnd unvollständige Information, mit Zufall
* [Tic_Tac_Toe](https://github.com/D4sherInc/Bachelor_Thesis/blob/master/prolog_games/tic_tac_toe.pl):
Tic-Tac-Toe (Drei-Gewinnt). 2 Spieler, abwechselnd, vollständige Information, ohne Zufall
* [iterated_prisoners_dilemma](https://github.com/D4sherInc/Bachelor_Thesis/blob/master/prolog_games/iterated_prisoners_dilemma.pl):
Gefangenendilemma. 2 Spieler, gleichzeitiger Zug

Es ist auch möglich, die Algorithmen aus C++ auf die Spiele anzuwenden, da deren Schnittstelle zu Python sehr
nah ist. Dies hat vermutlich eine gute Performance, falls die Abfrage nur aus dem Spielbaum besteht, mit dem der
Algorithmus selbst dann weiter arbeitet (z.B. CFR-Algorithmen). Es wird vermutlich eine schlechte Performance
liefern, falls der Algorithmus darauf besteht, öfters die Spielzustände abzufragen und auszuwerten (z.B. MCTS).