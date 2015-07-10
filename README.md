# deduction-playground

Natural deduction in clojure

## TODO
* Mehrfacheinfügung von Zeilen verhindern (beeinträchtig Fukntion nicht, aber sieht unschön aus) 
  * (z.B. [(and a b) (a) (...) (b)] - "and-e1" 1 => [(and a b) (a) (a) (...) (b)])
* Weitere Überprüfungen in "check-args" einfügen
  * Fehlermeldungen verbesserns/anpassen
* Forward Ergebnisse sollten immer unterhalb aller :premises/:assumptions eingefügt werden (statt direkt unter der "lastline") [nur Schönheitsmakel]
* Backward Schritte mit mehr als einer ausgehenden Zeile
* "choose-option" sollte auch premises als mögliche Ziele betrachten (nicht nur conclusions)
* Es fehlt eine Funktion, um Namen von neu eingeführten Variablen zu ändern