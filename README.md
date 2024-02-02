# OS-PF2
Der Clojure Code ist kommentiert und geht auf die einzelnen Funktionen ein.
Das Projekt ist als Leiningen-Projekt angelegt und kann auch entsprechend gestartet werden.

Als Parameter müssen die folgenden Werte geordnet angegeben werden:
- num-accounts : Gibt die Anzahl der zu erstellenden Accounts an.
- num-clients : Gibt die Anzahl der Clients/ Thread an.
- num-transfers : Anzahl der Transfere, die von den Clients durchgeführt werden soll.
- seed : Gibt den Seed an, um die Pseudorandomisierung zu vereinheitlichen.

Ein Beispielaufruf könnte zum Beispiel wie folgt aussehen:

    lein run -m bankkonto.core/main 10 5 100 seed    