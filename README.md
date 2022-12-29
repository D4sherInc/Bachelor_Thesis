# Bachelor-Thesis: OpenSpiel-Anbindung an Prolog            

Das ist mein Repository zum Sammeln und Entwickeln aller Entwicklungsfortschritte, 
benutzte Libraries und sonstige Dateien zur Absprache mit allen Betreuern.

Inhalt des Repositories:

* [src/prolog](https://github.com/D4sherInc/Bachelor_Thesis/tree/master/src/prolog): selbst definierte oder
anderweitig vordefinierte Spiele, die von Algorithmen und Umgebungen aus OpenSpiel unterstützt werden. Diese Spiele
werden von PySWIP über eine Query angesprochen und deren Ergebnis wird weiter ausgewertet.

* [src/python](https://github.com/D4sherInc/Bachelor_Thesis/tree/master/src/python): selbst definierter Quellcode 
  zur Anbindung von Prolog Spielen an Python mitsamt Testphasen zum ausprobieren von Funktionalitäten in "testing 
  grounds"

genutzte Frameworks und Libraries:
* [open_spiel](https://github.com/deepmind/open_spiel): Sammlung an Umgebungen und Algorithmen für die Forschung 
beim allgemeinen verstärkten Lernen ("general reinforcement learning"). Dies beinhaltet die allgemeinen 
Algorithmen, Umgebungen und Spiele in C++ und Python, die als Grundlage zur Entwicklung der API dienen.


* [PySWIP](https://github.com/yuce/pyswip): Python-Library zur Anbindung von Prolog Code. Mithilfe von PySWIP 
werden die in Prolog geschriebenen Spiele angesprochen und in Python anwendbar. Algorithmen und Umgebungen aus
OpenSpiel werden vorgegeben, über PySWIP an die Prolog-Definitionen weitergegeben und über eine Query ausgewertet
und danach wieder in einer Python-Repräsentation ausgewertet.
