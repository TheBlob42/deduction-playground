# deduction-playground

Natural deduction in clojure

## TODO
* Generierung von core.logic Funktionen vereinfachen (mit Macros) (falls möglich)
* REPL ausbauen (Funktion zum Laden/Zurücksetzen von Regeln, Theoremen, Classicals)
* "given" zu "premise" umbenennen
* TESTEN!!! TESTEN!!! TESTEN!!!

## Unterschiede zu Jape
Im Gegensatz zu Jape werden die beiden Regeln "OR Elimination" und "EXISTS Elimniation" rückwärts ausgeführt, statt vorwärts. Dies hängt mit der Art und Weise zusammen
wie dieses Projekt Regeln zusammenbaut und ausführt. Der User muss statt (step-f "or-e" <Zeilen>) lediglich (step-b "or-e" <Zeilen) angeben, sonst ändert sich nichts.
