%include thesis.fmt

\begin{kurzfassung*}

Software Transactional Memory (STM) vereinfacht nebenläufige Programmierung ungemein, indem es erlaubt Speicheroperationen zu atomaren Blöcken zusammenzufassen.
Ähnlich wie Datenbanktransaktionen, bieten STM Transaktionen Atomarität, Konsistenz und Isolation.
Anders als eine Datenbank, bietet STM keine \emph{Dauerhaftigkeit}.

Ich habe Haskell’s STM Implementierung mit einem Mechanismus erweitert, mit dem man \emph{finalizer} zu atomaren Blöcken hinzufügen kann.
Die neue Operation |atomicallyWithIO| erlaubt dem Programmierer die sichere Ausführung von beliebigen I/O-Operationen während der Commit-Phase einer Transaktion.
Dies kann dazu benutzt werden STM dauerhaft zu machen, es hat aber noch mehr Applikationen.
Zum Beispiel könnte ein Finalisierer den Benutzer fragen anstehende Resultate zu genehmigen, was interaktive Transaktionen ermöglicht.

Ein weiteres häufiges Problem mit STM ist \emph{contention}.
Viele Standarddatenstrukturen, wenn sie in einer transaktionalen Umgebung verwendet werden, erzeugen eine übermäßig hohe Anzahl an Konflikten.
Ich stelle eine neue STM-Datenstruktur vor, den \emph{transaktionalen Trie}, eine contention-freie Hashtabelle.
Er basiert auf dem lock-freien nebenläufigem Trie und benutzt lokale Seiteneffekte um unnötige Konflikte zu vermeiden, während er gleichzeitig die transaktionale Sicherheit bewahrt.

Sowohl finalizer als auch der transaktionale Trie sind Beispiele für das Kombinieren von Transaktionen mit Seiteneffekten.
Finalizer sind ein allgemeiner Ansatz von oben herab, während der transaktionale Trie Seiteneffekte auf der Mikroebene integriert.
Ich demonstriere die Effektivität von beiden in dem ich eine vollständige Beispielsanwendung baue die STM als Datenbanksprache benutzt, Dauerhaftigkeit bietet und contention vermeidet.

\end{kurzfassung*}
