%include thesis.fmt

\begin{kurzfassung*}

Software Transactional Memory (STM) vereinfacht nebenläufige Programmierung, indem es erlaubt Speicheroperationen zu atomaren Blöcken zusammenzufassen.
STM Transaktionen bieten Atomarität, Konsistenz und Isolation, und sind damit Datenbanktransaktionen ähnlich.
Im Unterschied zu Datenbanken bietet STM allerdings keine \emph{Dauerhaftigkeit}.

Ich habe Haskell’s STM Implementierung mit einem Mechanismus erweitert, mit dem man \emph{finalizer} zu atomaren Blöcken hinzufügen kann.
Die neue Operation |atomicallyWithIO| erlaubt dem Programmierer die sichere Ausführung von beliebigen I/O-Operationen während der Commit-Phase einer Transaktion.
Dies kann dazu benutzt werden STM dauerhaft zu machen, hat aber noch mehr Anwendungsmöglichkeiten.
Zum Beispiel könnte ein finalizer den Benutzer fragen anstehende Resultate zu genehmigen, was interaktive Transaktionen ermöglicht.

Ein weiteres häufiges Problem mit STM ist \emph{contention}.
Viele Standarddatenstrukturen, wenn sie in einer transaktionalen Umgebung verwendet werden, erzeugen eine übermäßig hohe Anzahl an Konflikten.
Ich stelle eine neue contention-freie STM-Datenstruktur vor, den \emph{transactional trie}.
Er basiert auf dem lock-free concurrent trie und benutzt lokale Seiteneffekte, um unnötige Konflikte zu vermeiden, während er gleichzeitig die transaktionale Sicherheit bewahrt.

Sowohl finalizer als auch der transactional trie sind Beispiele für das Kombinieren von Transaktionen mit Seiteneffekten.
Finalizer sind ein allgemeiner top-down Ansatz, während der transactional trie Seiteneffekte auf der Mikroebene integriert.
Ich demonstriere die Effektivität von beidem, in dem ich eine vollständige Beispielsanwendung baue, die STM als Datenbanksprache benutzt, Dauerhaftigkeit bietet und contention vermeidet.

\end{kurzfassung*}
