(ns clj.logic
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.pldb :refer :all])
  (:require [clojure.tools.macro :as macro])
  (:require [clojure.core.logic.fd :as fd])
  )  

stop

; 1. Grundlagen
(comment
  core.logic ist die Portierung von miniKanren (und Erweiterungen)
  von Scheme nach Clojure.

  miniKanren ist in Scheme eingebettet, kann als eine domänenspezifische
  Sprache für die Logikprogrammierung bzw. relationale Programmierung gesehen
  werden.

  miniKanren wurden entwickelt von Daniel P. Friedman, William E. Byrd und
  Oleg Kiselyov. Die Sprache wird auf originelle Weise in "The Reasoned Schemer" [1]
  der drei genannten Autoren eingeführt. William E. Byrd beschreibt miniKanren
  samt Erweiterungen in seiner Dissertation "Relational Programming in miniKanren:
  Techniques, Applications, and Implementations" [2].  http://minikanren.org [3] ist die
  Webseite für miniKanren und https://github.com/clojure/core.logic [4] die von core.logic.

  Kapitel über core.logic gibt es in den Büchern
  Michael Fogus, Chris Houser: The Joy of Clojure, 2nd edition [5] und
  Bruce Tate et al: Seven More Languages in Seven Weeks [6].


  Kanren ist das japanische Wort für Relation.
)

(comment
  Die grundlegende Struktur einer logischen Auswertung in Clojure hat die Form

  (run* [logical-variable]
       &goals)

  Die logische Variable wird gerne "q" benannt, "q" für Query. Sie enthält das Ergebnis
  der logischen Auswertung.

  Die "Goals" sind logische Ausdrücke bestehend aus logischen Variablen und Bedingungen.
  Ein "Goal" kann zutreffen ("succeed" oder "s#") oder nicht ("fail" oder "u#").

  (run* [q] &goals) versucht alle "Welten" zu finden, in denen die angegebenen logischen
  Bedingungen zutreffen. Logische Variablen werden dabei mit Termen unifiziert, so wie es durch
  die Bedingungen (goals) vorgegebn wird. Dabei spielt natürlich die Reihenfolge der Bedingungen
  keine Rolle.

  (run n [q] &goals) sucht n mögliche "Welten".
)

(run* [q]
      s#)
; => (_0)

(run* [q]
      u#)
; => ()

(comment
  Das erste Ergebnis ist (_0), was bedeutet, dass q durch die angegebenen logischen Bedingungen
  nicht beschränkt wird, _0 steht für eine ungebundene logische Variable, also jeden beliebigen
  Wert annehmen kann.

  Das zweite Ergebnis ist (), was bedeutet, dass keine die Bedingungen erfüllende "Welt"
  gefunden wurde. Was ja auch klar ist, denn u# steht ja für "fail", also den logischen
  Widerspruch.
)

(comment
  Der grundlegende Operator in core.logic ist ==, die Unifikation.

  Die Unifikation zweier Ausdrücke der Prädikatenlogik besteht darin, dass die Variablen
  der Ausdrücke so durch Terme ersetzt werden, dass sie gleich werden.

  Es gibt zu einer Menge von Ausdrücken im allgemeinen mehrere Unifkatoren. Zu jeder Menge von
  Ausdrücken gibt es einen kleinsten gemeinsamen Unifikator, der nicht notwendigerweise
  eindeutig ist.

  Mehr zum Thema: Franz Baader und Wayne Snyder: Unification theory in: Handbook of
  Automated Reasoning, edited by Alan Robinson and Andrei Voronkov, 2001

  Zu bemerken: bei der Unifikation in core.logic kann man die ganze Vielfalt der
  Datentypen von Clojure verwenden.
)

(run* [q]
      (== q "hello core.logic"))
; => ("hello core.logic"

(run* [q1 q2]
      (== q1 1)
      (== q2 2))
; => ([1 2])

(run* [q]
      (fresh [x y]
             (== x "David")
             (== y "Nolen")
             (== q {:name y :given-name x})))
; => ({:name "Nolen", :given-name "David"})

(run* [q]
      (== q 1)
      (== q 2))
; => ()

(comment
  Die "Goals" werden durch logisches und (Konjunktion) verbunden, deshalb
  scheitert die Suche nach einer "Welt" im letzten Beispiel, denn q kann nicht
  zugleich 1 und 2 sein.
)

(comment
  Der Operator (fresh ...) erzeugt neue, ungebundene logische Variablen,
  die in Ausdrücken verwendet werden können.

  Es gibt auch (all...), das wie fresh Bedingungen bündelt, jedoch keine
  logische Variable erzeugt.)

(run* [q]
      (fresh [x y]
             (== x y)
             (== q y)
             (== 1 x)))
; => (1)

(run* [q]
      (all
        (== q 1)))

(comment
  Mit logischem "und" allein kann man natürlich nicht viel erreichen: Man braucht auch die
  Disjunktion, das logische "oder".

  Der Operator dafür ist "conde". Die Bezeichnung kommt von "conditional every line" [1 S.12]

  Innerhalb eines Ausdrucks für "conde" werden die Ausdrücke wieder automatisch mit logischem
  "und" verbunden.
)

(run* [q]
     (conde
       [(== q 1)]
       [(== q 2)]
       [(== q 3)]))
; => (1 2 3)

(run* [q]
      (conde
        [(== q 1) (== q 2)]
        [(fresh [x] (== q x) (== x 3))]))
; => (3)

(run* [q]
      (conde
        [(conde [(== q :branch1-1)] [(== q :branch1-2)])]
        [(conde [(== q :branch2-1)] [(== q :branch2-2)])]
        ))
; => (:branch1-1 :branch2-1 :branch1-2 :branch2-2)

(run* [q]
      (fresh [x y]
             (== q [x y])
             (== x 1)
             (conde
               [(== y 1)]
               [(== y 2)]
               [(== x 2) (== y 2)])))
; => ([1 1] [1 2])  
; der letzte Zweig scheitert, weil x nicht zugleich mit 1 und 2 unifiziert werden kann

(comment
  Die bisher vorgestellten Konzepte:

  Die Einbettung der logische Programmierung in Clojure mittels "run" und
  die drei logischen Operatoren "==", "fresh" und "conde" sind der (kleine)
  Kern von core.logic.

  Was aber ist mit dem Operator "not"?
  Das "Problem" besteht darin, dass im allgemeinen nicht angegeben werden kann,
  für welche Terme etwa nicht gilt.

  TODO: closed world assumption in Prolog; ungleich in core.logic
  )


; 2. Relationen am Beispiel nützlicher solcher
(comment
  Aufbauend auf fresh, == und conde hat core-logic nützliche Relationen, die wir
  im Folgenden betrachen wollen:
  )

; nilo
(run* [q]
      (nilo nil))
; => (_0)

(run* [q]
      (nilo ()))
; => ()

(comment
  nilo ist die relationale Variante von nil?. nilo unifiziert sein Argument mit dem
  Wert nil.
)

; emptyo
(run* [q]
      (emptyo ()))
; => (_0)

(run* [q]
      (emptyo [:a :b]))
; => ()

(comment
  empyto ist die relationale Variante von empty?
)

; firsto
(run* [q]
      (firsto [:a :b :c] q))
; => (:a)

(run* [q]
      (fresh [x]
        (firsto [x :b :c] :a)
        (== q [x :b :c])))
; => ([:a :b :c])

(comment
  (firsto coll item) ist die Relation in der item das erste Element von coll ist.
  Es ist die relationale Variante von (first coll).
)

(comment
  An diesem Beispiel ist gut zu sehen, was es heißt, dass "funo" die
  relationale Variante von "fun" ist:

  Eine Funktion bildet seine Argumente auf einen Wert ab:
  (fun a1 a2 ... an) => v. Man kann dies sehen als Abbildung
  fun: D1 x D2 x ... x Dn -> W für den Definitionsbereich D1 x ... x Dn und
  den Wertebereich W.

  Daraus macht man eine Relation, in dem man den Wertebereich mit einbezieht:

  funo ist Teilmenge von D1 x D2 x ... x Dn x W

  Diese neue Sicht hat Konsequenzen:
  (1) funo kann mehrere v's zu denselben Argumenten haben.
  (2) Bei der Unifikation spielt es keine Rolle, ob wir eine logische Variable
      für ein Element von W oder eines der Di einsetzen. Unifikation geht in
      jede Richtung, gewissermaßen "vorwärts" oder "rückwärts"
  )

; resto
(run* [q]
      (resto [:a :b :c] q))
; => ((:b :c))

(run* [q]
      (fresh [x]
             (resto x [:b :c])
             (firsto x :a)
             (== q x)))
; => ((:a :b :c))

(run* [q]
      (resto '(a) q))
; => (()), die leere Folge

(comment
  resto ist die relationale Variante von rest:
  (resto coll rest) ist die Relation in der rest die coll ohne das erste Element ist.
)

; conso
(run* [q]
      (conso :a [:b :c] q))
; => ((:a :b :c))

(run* [q]
      (conso q [:b :c] [:a :b :c]))
; => (:a)

(run* [q]
      (conso :a q [:a :b :c]))
; => ((:b :c))

(comment
  (conso first rest coll) ist die Relation, in der first das erste Element von coll und
  rest der Rest ist.
  conso ist die relationale Variante von cons.)


(comment
  Woher kommt das kleine "o" zur Kennzeichnung von Relationen?

  In einem Vortrag über miniKanren sagt William Byrd, dass Dan Friedman die Konvention
  eingeführt hat, weil ein hochgestelltes "o" wie ein Fragezeichen wirkt, wenn man die
  Augen etwas zusammenkneift. Wie auch immer: nehmen wir das einfach als Konvention,
  mit der Relationen in der Notation ausgezeichnet werden.

  Die offizielle Begründung:
  "A relation, a function that returns a goal as its value, ends its name with a
  superscript 'o' (e.g., car° and null°)." [1 S. ix]
)

(comment
  Wir haben auch gesehen:

  Funktionen, die einen Wahrheitswert zum Ergebnis haben, (wie z.B. empty?) werden zu einer
  Relation mit derselben Anzahl von Argumenten.

  Andere Funktionen (wie z.B. (cons first rest)) bekommen als Relation ein Argument mehr - nämlich
  für ihren Wert (im Beispiel (conso first rest coll)).

  Als Relation können Funktionen "rückwärts" ausgeführt werden, denn der "Output" ist kein
  spezieller Parameter der Relation, sondern ebenso einer wie die anderen auch.
)

; membero
(run* [q]
      (membero :a [:a :b :c])
      (== q true))
; => (true)

(run* [q]
      (membero q [:a :b :c]))
; => (:a :b :c)

(run 1 [q]
      (membero :a q))
; => ((:a . _0))

(run 2 [q]
     (membero :a q))
; => ((:a . _0) (_0 :a . _1))

(run 12 [q]
     (membero :a q))
; => 12 Lösungen
; hier sieht man auch, dass ein goal unendlich viele Ergebnisse haben kann

(comment
  (membero item coll) ist die Relation, die angibt, dass coll item enthält.

  Eine spezielle Variante ist member1o, die nur jeweils ein Element findet.
)

(run* [q]
      (membero q [:a :b :a :b :a :b :c]))
; => (:a :b :a :b :a :b :c)

(run* [q]
     (member1o q [:a :b :a :b :a :b :c]))
; => (:a :b :c)

; appendo

(run* [q]
     (fresh [x]
            (appendo [:a] [:b :c] x)
            (== q x)))
; => ((:a :b :c))

(run* [q]
      (fresh [x]
             (appendo [:a] x [:a :b :c])
             (== q x)))
; => ((:b :c))

(run* [q]
      (fresh [x y]
             (appendo x y [:a :b :c])
             (== q [x y])))
; => ([() [:a :b :c]] [(:a) (:b :c)] [(:a :b) (:c)] [(:a :b :c) ()])
; alle Möglichkeiten, wie man 3 Elemente zu einem Vewktor von zwei Elementen verarbeiten kann

(comment
  (appendo coll1 coll2 colla) ist die Relation von colla die Aneinanderreihung der
  Element von coll1 und coll2 ist.
)

; permuteo
(run* [q]
      (permuteo [1 2 3] q))
; => ((1 2 3) (2 1 3) (1 3 2) (2 3 1) (3 1 2) (3 2 1))

(run* [q]
      (permuteo q [1 2 3]))
; => endet nicht!!

(run 1 [q]
      (permuteo q [1 2 3]))
; => ((1 2 3))

(run 2 [q]
     (permuteo q [1 2 3]))
; => ((2 1 3))

(comment
  (permuteo coll1 coll2) bildet in coll2 alle Permutationen der Elemente von coll1.
  Wird eventuell nicht terminieren, wenn coll1 nicht aus Grundtermen besteht.)

; everyg

(run* [q]
      (everyg #(membero % [:a :b]) [:a :b :a :b])
      (== q true))
; => (true)

(run* [q]
      (everyg #(membero % [:a]) [:a :b :a :b])
      (== q true))
; => ()

(run* [q]
      (everyg #(membero % [:a :b]) [:a :b :a :b :c])
      (== q true))
; => ()

(run* [q]
      (fresh [x y]
             (== q [x y])
             (everyg #(membero % [:a :b]) q)))
; => ([:a :a] [:a :b] [:b :a] [:b :b])

(comment
  (evergy #(goal %) coll) gilt, wenn das Ziel goal für jedes Element der Kollektion coll
  gilt.

  Ist keine richtige Relation. Wenn man im letzten Beispiel die Reihenfolge der Bedingungen
  vertaucht, bekommt man einen Fehler.
)

; 3. Elegante Formulierung von Goals mittels matche und defne

(comment 
  In core.logic gibt es die Relation appendo, die wir oben schon
  kennengelernt haben.

  Folgende Definition der Funktion ist aus core.logic übernommen.
  
  Wir wollen verstehen, wie defne funktioniert.
  
  (defne appendo
         "A relation where x, y, and z are proper collections,
         such that z is x appended to y"
         [x y z]
         ([() _ y])
         ([[a . d] _ [a . r]] (appendo d y r)))
  )

; Zunächst machen wir mal eine eigene Implementierung

(defn app1o
  [x y z]
  (conde
    [(== () x) (== y z)]
    [(fresh [h t res]
           (conso h t x)
           (conso h res z)
           (appendo t y res))]))

(run* [q]
      (app1o [1 2] [3 4] q))
; => ((1 2 3 4))

(run* [q]
      (app1o [1 2] q [1 2 3 4]))
; => ((3 4))

(run* [q]
      (app1o q [4] [1 2 3 4]))
; => ((1 2 3))

; Man kann diese Definition vereinfachen, in dem man matche
; (match every line) einsetzt

(comment
  Das erste Argument für matche ist das, was wir matchen wollen.
  
  Dann kommen Klauseln, wobei immer der erste Teil ein Pattern ist, das wir
  matchgen wollen. Dabei kann man den Punkt . als Symbol für cons verwenden.
  Kommt '_' vor, so ist das eine frische Variable, die mit allem unifiziert.
  
  Am besten wir machen das Beispiel weiter:)

(defn app2o
  [x y z]
  (matche [x y z]
          ([() y y]) 
          ([[h . t] y [h . res]] (app2o t y res))))

; In der ersten Zeile wird gleichgesetzt:
; (== x ()), (== y y), (== z y) 
; Das entspricht genau der Klausel oben

; In der zweiten Zeile wird gleichgesetzt:
; (== x [h . t]) mit h der Kopf von x und t der Rest
; (== y y) das klappt immer, d.h. y wird ignoriert
; (== z [h . res]) d.h. der Kopf von x muss der Kopf von z sein
; wenn das Matching so klappt wird die Funktion rekursiv aufgerufen
; mit dem Rest von x, dem alten y und dem Rest von z

(run* [q]
      (app2o [1 2] [3 4] q))
; => ((1 2 3 4))

(run* [q]
      (app2o [1 2] q [1 2 3 4]))
; => ((3 4))

(run* [q]
      (app2o q [4] [1 2 3 4]))
; => ((1 2 3))

(comment
  defne ist ein Makro, das die Definition einer Relation erlaubt, bei der
  matche gleich verwendet wird.
  
  Wieder unser Beispiel:)

(defne appo
  [x y z]
    ([() y y])
    ([[h . t] y [h . res]] (appo t y res)))

; Neben defne gibt es auch fne für anonyme Funktionen mit Pattern Matching

(run* [q]
      (appo [1 2] [3 4] q))
; => ((1 2 3 4))

(run* [q]
      (appo [1 2] q [1 2 3 4]))
; => ((3 4))

(run* [q]
      (appo q [4] [1 2 3 4]))
; => ((1 2 3))


;  Das ist nun fast der Code aus core.logic, nur dass im Matching y nicht
;  explizit vorkommt. Die erste Klausel z.B. lautet [() _ y]) und nicht [( y y)].
  
;  Das geht, weil (== y y) immer passt, genauso wie (== _ y), weil die frische Variable '_'
;  ja irgendeinen Wert, insbesondere y haben kann. y spielt keine Rolle beim Matchen.

; Weitere Beispiele für diese Technik findet man hier:
; Ryan Senior: Appendo the Great \url{http://objectcommando.com/blog/2011/10/13/appendo-the-great}

; Beispiele aus der Dissertation von W. Byrd, die die Basisoperatoren näher beleuchten

(defn anyo
  [g]
  (conde 
    [g]
    [(anyo g)]))

; anyo probiert das Ziel g immer und immer wieder

(run* [q]
      (conde
        [(anyo (== false q))]
        [(== true q)]))
; Dieses Beispiel läuft ewig, weil jeder Aufruf von anyo immer wieder gelingt

; Was passiert sieht man ganz gut, wenn man nur ein paar Ergebnisse verlangt:

(run 5 [q]
      (conde
        [(anyo (== false q))]
        [(== true q)]))
; => (true false false false false)

; Ein weiteres Beispiel aus der Diss von Byrd

(run 10 [q]
     (anyo
       (conde
         [(== 1 q)]
         [(== 2 q)]
         [(== 3 q)])))
;=> (1 2 3 1 2 3 1 2 3 1)

(def alwayso (anyo (== false false)))

(run 1 [q]
     (== true q)
     alwayso
     (== false q))
; Läuft ewig. Grund:
; 1. q wird mit true unifiziert
; 2. alwayso klappt unbeschränkt oft, aber
; 3. Jeder der Versuche wird verworfen, weil q nicht zugleich false sein kann

; Anders sieht es mit folgendem Beispiel aus

(run 5 [q]
     (conde
       [(== true q)]
       [(== false q)])
     alwayso
     (== false q))
; => (false false false false false)


; 4. Nicht-relationale Techniken

(comment 
  miniKanren und auch core.logic hat nicht-relationale Operatroen, die man in manchen
  Situationen anwenden kann.
  
  Man muss aber wissen, dass sie 1. eine bestimmte Reihenfolge der Goals voraussetzen und 
  2. eventuell nicht funktionieren, wenn man sie nur mit frischen Variablen aufruft.
  
  Es handelt sich bei core.logic um:
  - project -- "Extract the values bound to the specified logic vars. Non-relational."
  - pred    -- "Check a predicate against the value logic var. Non-relational."
  - is      -- "Set the value of a var to value of another var with the operation
                applied. Non-relational."
  - conda   -- "Soft cut. Once the head of a clause has succeeded all other clauses 
                will be ignored. Non-relational."
  - condu   -- "Committed choice. Once the head (first goal) of a clause
                has succeeded, remaining goals of the clause will only
                be run once. Non-relational."
  - onceo   -- garantiert, dass wenn ein gola probiert wird, es höchstens eine Antwort
               produziert
  - copy-term -- "Copies a term u into v. Non-relational."
  - lvaro   -- "A goal that succeeds if the argument is fresh. v must be a logic
                variable. Non-relational."
  - nonlvaro - "A goal that succeeds if the argument is not fresh. v must be a
                logic variable. Non-relational."
  )

; project 

; Mit project kann man Werte verwenden, die logischen Variablen zugeordnet sind

; Folgendes geht nicht, weil x eine logische Variable ist und nicht eine Zahl

(run* [q]
      (fresh [x]
             (== 5 x)
             (== (* x x) q)))
; => ClassCastException clojure.core.logic.LVar cannot be cast to java.lang.Number 

; damit das doch geht, muss man die logische Variable auf ihren zugeordneten Werte projezieren:

(run* [q]
      (fresh [x]
             (== 5 x)
             (project [x] (== (* x x) q))))
; => (25)

; aber dieser Operator ist nicht relational, weil diese Projektion natürlich nur geht,
; wenn die Unifikation schon gemacht wurde, d.h. folgendes geht nicht:

(run* [q]
      (fresh [x]
             (project [x] (== (* x x) q))
             (== 5 x)))
; => ClassCastException clojure.core.logic.LVar cannot be cast to java.lang.Number 

; Beispiel für pred

; hier der Code zu pred:
(comment
  (defmacro pred
    "Check a predicate against the value logic var. Non-relational."
    [v f]
    `(project [~v]
              (== (~f ~v) true)))  
  )

(run* [q]
      (== q 1)
      (pred q #(= % 1)))
; => (1)

(run* [q]
      (conde [(== q 1)] [(== q 2)] [(== q 3)])
      (pred q #(= % 1)))
; => (1)

; aber
(run* [q]
      (pred q #(= % 1))
      (== q 1))
; => ()
; pred scheitert, weil bei der Projektion der ungebundenen logischen Variablen q natürlich niemals
; ein Wert rauskommen kann, der 1 ist.


; Beispiel für is

; auch hier wieder der Code zunächst:

(comment
  (defmacro is
    "Set the value of a var to value of another var with the operation
     applied. Non-relational."
    [u v op]
    `(project [~v]
              (== ~u (~op ~v)))) 
  )

(run* [q]
      (fresh [x]
             (== x 1)
             (is q x inc)))
; => (2)

; aber
(run* [q]
      (fresh [x]
             (is q x inc)
             (== x 1)))
; => ClassCastException clojure.core.logic.LVar cannot be cast to java.lang.Number  

; conda und condu

; conde (conditional every line probiert jede Möglichkeit und eröffnet einen neuen
; Zweig beim Versuch der Unifikation

; Im Unterschied dazu kann bei conda oder condu immer nur eine der Möglichkeiten
; gelingen:
; Die Bedingungen werden in der angegebenen Reihenfolge probiert, außerdem wird
; innerhalb der Bedingungen das erste Goal als Test genommen, ob weiter versucht wird
; zu unifizieren.

; Sehen wir uns das Verhalten anhand der Beispiele aus Byrds Diss an:

(run* [q]
      (conda
        [(== 'olive q)]
        [(== 'oil q)]))
; => (olive)

; im Unterschied zu
(run* [q]
      (conde
        [(== 'olive q)]
        [(== 'oil q)]))
; => (olive oil)

(run* [q]
      (conda
        [(== 'virgin q) (== true false)]
        [(== 'olive q)]
        [(== 'oil q)]))
; => ()
; Warum?
; conda kann in der ersten Bedingung q mit 'virgin unifizieren. Damit ist die
; Entscheidung für diesen Zweig gefallen. Danach scheitert er, weil true nicht false ist!

; Zum Unterschied zwischen conda und condu:

(run* [q]
      (conda 
        [(== true false)]
        [alwayso])
      (== true q))

; das läuft ewig, weil conda die erste Bedingung verwirft und deshalb in der zweiten
; in eine endlose Schleife kommt.

(run* [q]
      (condu
        [(== true false)]
        [alwayso])
      (== true q))
; => (true)

; In condu kann ein Goal höchstens einmal gelingen (das ist der einzige Unterschied zu
; conda. Das bedeutet in diesem Beispiel aber, dass alwayso nur einmal verwendet wird, und
; somit endet die Auswertung.

; Zur Terminologie:
; conda steht für "conditional a single line", siehe [1] p. 146
; Das u in condu steht für "uni-", weil ein Gola nur einmal ausgewertet wird.

; Hintergrund:
; Beide Formen entsprechen Varianten des "cut" in Prolog
; TODO: cut in Prolog verstehen
; Bryd: "conda corresponds to a soft-cut (Clocksin 1997), while condu corresponds to 
; Mercury’s committed-choice (Henderson et al. 1996; Naish 1995)."
; siehe auch [1] Chap 10

; Man kann condu verwenden, um onceo zu definieren:

; siehe core-logic:  (defn onceo [g] (condu g))
(run* [q]
      (onceo alwayso))
; => (_0)
; alwayso wird nur einmal versucht und gelint
; q bleibt ungebunden

; copy-term
; copy-term erzeugt eine Kopie des ersten Arguments un dersetzt dabei ungebundene Variablen
; durch frische, diese Kopie wird dann mit dem zweiten Argument unifiziert

; Beispiel aus der Diss
(run* [q]
      (fresh [w x y z]
             (== ['a x 5 y x] w)
             (copy-term w z)
             (== [w z] q)))
; => ([[a _0 5 _1 _0] [a _2 5 _3 _2]])

; das erste Element des Vektors ist die Unifikation von w mit dem angegeben Vektor
; das zweite Element ist die Kopie in z bei der die Terme beibehalten werden, aber die
; ungebundenen Variablen durch frische ersetzt werden.

; Das ist nicht-relational, weil offenbar die Reihenfolge der Zeilen eine Rolle spielt!


; Nun haben wir noch zwei Operatoren:


; lvaro und nonlvaro

(run* [q]
      (lvaro q)
      (== true q))
; => (true)

(run* [q]
      (== true q)
      (lvaro q)
      (== true q))
; => ()

(run* [q]
      (fresh [x]
             (== x 1)
             (nonlvaro x))
      (== true q))
; => (true)

(run* [q]
      (fresh [x]
             (nonlvaro x))
      (== true q))
; => ()

(comment
  Analog zu matche, fne und defne -- siehe vorheriges Kapitel -- gibt es auch
  
  matcha, fna und defna
  
  sowie
  
  matchu, fnu und defnu
  )

; 4. Ungleichheit in core.logic

(comment
  In core.logic gibt es keine Negation. Warum? Die logischen Variablen in core.logic
  sind nicht auf ein bestimmtes, womöglich endliches Universum festgelegt, sondern
  können irgendeinen Wert annehmen. Was für ein Wert sollte das aber sein, wenn
  wir Negation als Operator haben?)

(comment 
  Aber es gibt ein Ungleichheits-Constraint, nämlich !=, das wir jetzt untersuchen wollen:
  )

(run* [q]
      (!= q 1))
; =>  ((_0 :- (!= (_0 1))))
; Das Ergebnis ist eine ungebundene logische Variable _0, die ein Constraint trägt:
; sie darf niemals mit 1 unifiziert werden.

; Daraus ergibt sich:

(run* [q]
      (== q 1)
      (!= q 1))
; => ()
; denn beides kann nicht erfüllt sein

; Nun wollen wir q mal auf ein endliches Universum festlegen
(run* [q]
      (membero q [1 2 3 4 5])
      (!= q 1)
      (!= q 5))
; => (2 3 4)

; Beispiele aus Byrds Diss:
(run* [q]
      (!= (+ 2 3) 5))
; => ()

(run* [q]
      (!= (* 2 3) 5))
; => (_0)

(run* [q]
      (fresh [x]
             (!= 5 q)
             (== x q)
             (!= x 6)))
; => ((_0 :- (!= (_0 5)) (!= (_0 6))))

; !=, distincto, rembero

(comment
  (distincto l)
  "A relation which guarantees no element of l will unify
  with another element of l."
  )

(run* [q]
      (fresh [x y]
             (distincto [x y])
             (conde 
               [(== x 1) (== y 1)]
               [(== x 1) (== y 2)]
               [(== x 2) (== y 1)])
             (== q [x y])))
; => ([1 2] [2 1])

(comment
  (rembero x l o)
  "A relation between l and o where x is removed from
   l exactly one time."
  Byrd zeigt in seiner Diss, dass man != braucht, um rembero korrekt zu formulieren.
  )

(run* [q]
      (rembero 'b '[a b c d e] q))
; => ((a c d e)) 

(run* [q]
      (rembero 'b '[a b c b d] q))
; => ((a c b d)) 

(run* [q]
      (rembero 'b '(b) '(b)))
; => ()

(run* [q]
      (rembero 'b '(b b) q))
; => ((b))

(run* [q]
      (rembero 'b '(b) q))
; => (())

(run* [q]
      (rembero q '(a b b b) '(a b b)))
; => (b)

; Noch ein Beispiel aus der Diss:

(run* [q]
      (fresh [x o]
             (rembero x '(a b c) o)
             (== [x o] q)))
; => ([a (b c)] [b (a c)] [c (a b)])
; aber: das Ergebnis bei Byrd sieht etwas anders aus!      

(run* [q]
      (fresh [x o]
          (rembero x '(a b c) o)
          (== (llist x o) q)))
; => ((a b c) (b a c) (c a b))

(comment
  Noch eine spezielle Variante eines Constraints ist (featurec x fs)
  "Ensure that a map contains at least the key-value pairs
   in the map fs. fs must be partially instantiated - that is,
   it may contain values which are logic variables to support
   feature extraction."
  )

(run* [q]
      (featurec q {:a 1}))
; => ((_0 :- (clojure.core.logic/featurec _0 {:a 1})))
; q kann irgendwas sein, das eine Map ist und den Eintrag [:a 1] enthält!

(run* [q]
      (conde
        [(== q {:a 1})]
        [(== q {:b 2})]
        [(== q {:a 1 :b 2})])
      (featurec q {:a 1}))
; => ({:a 1} {:b 2, :a 1}) 

; featurec ist nicht relational!

; 5. pldb
; pldb steht wohl für persistent logic database

; Wir folgen erstmal dem Tutorial von David Nolen

(db-rel mann x)
; definiert das unäre Relationssymbol mann

(def welt 
  (db
    [mann 'hans]
    [mann 'peter]))
; definiert eine Welt mit 2 Männern  

welt
; => {"mann_1" {:clojure.core.logic.pldb/unindexed #{(peter) (hans)}}} 

; In dieser Welt können wir nun Sachen fragen
(with-db welt
         (run* [q] (mann q)))
;= (peter hans)

(with-db welt
         (run 1 [q] (mann q)))
; => (peter)

(with-db welt
         (run* [q]
               (== q 'felix)
               (mann q)))
; => ()

(with-db welt
         (run* [q]
               (== q 'hans)
               (mann q)))
; =>  (hans)

(db-rel frohnatur x)

(def welt
  (db
    [mann 'hans]
    [mann 'peter]
    [frohnatur 'peter]))

(with-dbs [welt]
          (run* [q]
                (mann q)
                (frohnatur q)))
; => (peter)


(db-rel frau x)

(def welt
  (-> welt
      (db-fact frau 'eva)
      (db-fact frau 'gisela)))

(db-rel mag x y)

(def welt
  (-> welt
      (db-fact mag 'hans 'eva)
      (db-fact mag 'peter 'gisela)))

; Auf diese Weise kann man eine Welt aufbauen. Ist aber nicht sehr funktional
; mit Wertetypen gedacht!!
; Ich denke man sollte dieses Konstrukt nur zur Konstruktion der Datenbank verwenden

; Ansonsten kann man auch verschiedene Datenbanken definieren und sie in with-dbs
; verwenden

(with-dbs [welt]
          (run* [q]
                 (mag 'peter q)))
; => (gisela)

(with-dbs [welt]
          (run* [q]
                (fresh [x y]
                       (mag x y)
                       (== q [x y]))))
; => ([hans eva] [peter gisela])

; Nun machen wir ein Beispiel mit Twitter, wo wir einen gerichteten Graphen haben

(db-rel followed-by user follower)

(def twitter-world
  (db
    [followed-by 'jack 'sarah]
    [followed-by 'sarah 'justin]
    [followed-by 'sarah 'jenny]
    [followed-by 'justin 'jenny]
    [followed-by 'justin 'tommy]))

; Anhänger von Jack?
(with-dbs [twitter-world]
          (run* [q]
                (followed-by 'jack q)))
; => (sarah)

; Anhänger der Anhänger von Jack?
(with-dbs [twitter-world]
          (run* [q]
                (fresh [x]
                       (followed-by 'jack x)
                       (followed-by x q))))
; => (justin jenny)

; Rekursiv die Anhänger von Jack?
(defn follower [u f]
  (conde
    [(followed-by u f)]
    [(fresh [x]
            (followed-by u x)
            (follower x f))]))

(with-dbs [twitter-world]
          (run* [q]
                (follower 'jack q)))
; => (sarah justin jenny tommy jenny)


; Transitiver Abschluss

(with-dbs [twitter-world]
          (run* [q]
                (fresh [x y]
                       (follower x y)
                       (== q [x y]))))
; => ([justin tommy] [sarah justin] [sarah jenny] [jack sarah] 
;     [justin jenny] [sarah tommy] [sarah jenny] [jack justin] 
;     [jack jenny] [jack tommy] [jack jenny])

; Nun bauen wir in unsere Welt noch einen Zyklus ein

(def twitter-world
  (-> twitter-world
      (db-fact followed-by 'tommy 'jack)))

twitter-world
; => {"followed-by_2" {:clojure.core.logic.pldb/unindexed 
;    #{(justin tommy) 
;      (sarah justin) 
;      (sarah jenny)  
;      (jack sarah) 
;      (tommy jack) 
;      (justin jenny)}}}

; Alle Anhänger von Jack?
(with-dbs [twitter-world]
          (run* [q]
                (follower 'jack q)))
; läuft ewig
; wie kann man das verhindern?

; 6. fd
(comment
  core.logic hat auch eine Bibliothek für CLP(FD) "Constraint Logic Programming over Finite
  Domains"

  Im Allgemeinen handelt es sich um endliche Mengen, oft jedoch verwenden viele Constraint
  Solver endliche Mengen von positiven Integern, so auch clojure.core.logic.fd
)

; Definition des Universums

(comment
  Die Funktionen fd/domain und fd/interval dienen der Definition einer Menge,
  mit fd/in wird logische Variable auf eine endliche Menge für die mögliche Substitution
  von Werten beschränkt.

  fd/domain hat als Argumente Integers, die aufsteigend sortiert sein müssen.

  fd/interval hat zwei Ausprägungen: mit einem Argument wird nur die obere Grenze ub
  angegeben und man erhält das Interval 0..ub, bei zwei Argumenten gibt man die untere
  und die obere Grenze an.
)

(run* [q]
      (fd/in q (fd/domain 1 2 3)))
; => (1 2 3)

; Bei einer einzelnen Variablen geht auch dom:
(run* [q]
      (fd/dom q (fd/domain 1 2 3)))
; => (1 2 3)

; fd/in kann beliebig viele Argumente haben, die ersten logische
; Variablen, das letzte eine Domain

(run* [q]
      (fresh [x y]
             (fd/in x y (fd/interval 6))
             (fd/+ x y 10)
             (== q [x y])))
; => ([4 6] [5 5] [6 4])

(run* [q]
      (fd/in q (fd/domain 1 2 3))
      (fd/in q (fd/domain 2 3 4)))
; => (2 3)

(run* [q]
      (fd/in q (fd/interval 3)))
; => (0 1 2 3)

(run* [q]
      (fd/in q (fd/interval 2 5)))
; => (2 3 4 5)

(run* [q]
      (fd/in q (fd/interval 100))
      (fd/>= q 98))
; => (98 99 100)

(comment
  Wie wir bereits in den Beispielen gesehen haben, kann man arithmetische Operatoren
  in der Definition der Goals verwenden.

  Dies wollen wir in den folgenden Beispielen etwas genauer ansehen:)


(run* [q]
      (fresh [x y]
             (fd/in x y (fd/interval 6))
             (fd/- x y 4)
             (== q [x y])))
; => ([4 0] [5 1] [6 2])

(run* [q]
      (fresh [x y]
             (fd/in x y (fd/interval 6))
             (fd/* x y 12)
             (== q [x y])))
; => ([2 6] [3 4] [4 3] [6 2])

(run* [q]
      (fresh [x y]
             (fd/in x y (fd/interval 6))
             (fd/quot x y 2)
             (== q [x y])))
;=> ([0 0] [2 1] [4 2] [6 3])
; Ich hätte nicht erwartet, dass [0 0] vorkommt

(run* [q]
      (fresh [x y]
             (fd/in x y (fd/interval 6))
             (fd/quot x y 2)
             (fd/!= x 0)
             (== q [x y])))
; so wird's richtig
; => ([2 1] [4 2] [6 3])

; Ebenso gehen Vergleichsoperatoren ==, !=, <, <=, >, >=

(comment
  Besonders bequem für arithmetische Operationen is fd/eq, dem
  man einfach eine normale mathematische Berechnung geben kann.
  Sie wird dann von fd/eq in die passende Form für fd gebracht.)

(run* [q]
      (fresh [x y]
             (fd/in x y (fd/interval 9))
             (fd/eq
               (= (+ x y) 9)
               (= (+ (* x 2) (* y 4)) 24))
             (== q [x y])))
; => ([6 3])
; weil x + y 9 sein muss sowie 2x + 4y = 24

; Kann man lineare Gleichssysteme lösen?

; Folgende Aufgabe entspricht dem linearen Gleichungssystem
; 3x + 4y = 9
;  2x + y = 1
; Dummerweise hat es als Lösung x = -1 und y = 3,
; diese Lösung kann also nicht gefunden werden!
(run* [q]
      (fresh [x y]
             (fd/in x y (fd/interval 9))
             (fd/eq
               (= (+ (* 3 x) (* 4 y)) 9)
               (= (+ (* x 2) y) 1))
             (== q [x y])))

(comment
  Für viele Fragestellungen ist wichtig, formulieren zu können, dass die
  Werte von mehreren Variablen verschieden sind. Dazu gibt es fd/distinct.)


(let [vars (vec (repeatedly 3 lvar))]
  (run* [q]
        (== q vars)
        (everyg #(fd/in % (fd/domain 1 2 3)) vars)
        (fd/distinct vars)
        ))
; => ([1 2 3] [2 1 3] [1 3 2] [3 1 2] [2 3 1] [3 2 1])

(comment
  Dokumentation zu fd/distinct:
  (fd/distinct v*)
  "A finite domain constraint that will guarantee that
   all vars that occur in v* will be unified with unique
   values. v* need not be ground. Any vars in v* should
   eventually be given a domain."
)

; 7. Beispiele

; Einsteins Rätsel
; Es kursiert das Gerücht, dass das folgende Rätsel von Einstain stammt
; in der angelsächsischen Literatur wird es oft als Zebra-Rätsel bezeichnet

; Hier die deutsche Fassung:

; Albert Einstein verfasste (angeblich) dieses Rätsel, von dem er be- 
; hauptete, dass 98% der Weltbevölkerung nicht in der Lage seien, 
; es zu lösen. Gehören Sie zu den 2%? Viel Spaß beim Ausprobieren!

; Es gibt fünf Häuser mit je einer anderen Farbe. In jedem Haus wohnt 
; eine Person einer anderen Nationalität. Jeder Hausbewohner bevor- 
; zugt ein bestimmtes Getränk, raucht eine bestimmte Zigarettenmar- 
; ke und hält ein bestimmtes Haustier. Keine der 5 Personen trinkt 
; das gleiche Getränk, raucht die gleichen Zigaretten oder hält das 
; gleiche Tier wie einer seiner Nachbarn.

; Frage: Wer besitzt einen Fisch? Bekannt sind folgende Tatsachen:
; – Der Brite lebt im roten Haus.
; – Der Schwede hält einen Hund.
; – Der Däne trinkt gerne Tee.
; – Das grüne Haus steht links vom weißen Haus.
; – Der Besitzer des grünen Hauses trinkt Kaffee.
; – Die Person, die Pall Mall raucht, hält einen Vogel.
; – Der Mann, der im mittleren Haus wohnt, trinkt Milch.
; – Der Besitzer des gelben Hauses raucht Dunhill.
; – Der Norweger wohnt im ersten Haus.
; – Der Marlboro-Raucher wohnt neben dem, der eine Katze hält.
; – Der Mann, der ein Pferd hält, wohnt neben dem, der Dunhill raucht.
; – Der Winfield-Raucher trinkt gerne Bier.
; – Der Norweger wohnt neben dem blauen Haus.
; – Der Deutsche raucht Rothmanns.
; – Der Marlboro-Raucher hat einen Nachbarn, der Wasser trinkt

(defne righto [x y l]
       ([_ _ [x y . ?r]])
       ([_ _ [_ . ?r]] (righto x y ?r)))
; wenn l die Form [x y und den Rest] hat, dann ist alles klar
; wenn l die Form [irgendwas und den Rest hat, dann rekursiv righto mit x y und dem Rest

(defn nexto [x y l]
  (conde
    ((righto x y l))
    ((righto y x l))))

; schema [Nation Zigarette Getränk Tier Hausfarbe]

(defn einsteino [hs]
  (macro/symbol-macrolet [_ (lvar)]
                         (all
                           (firsto hs ['norweger _ _ _ _])
                           (== [_ _ [_ _ 'milch _ _] _ _] hs)
                           (nexto ['norweger _ _ _ _] [_ _ _ _ 'blau] hs)
                           (righto [_ _ _ _ 'gruen] [_ _ _ _ 'weiss] hs)
                           (membero [_ 'dunhill _ _ 'gelb] hs)
                           (membero ['schwede _ _ 'hund _] hs)
                           (membero [_ _ 'kaffee _ 'gruen] hs)
                           (membero ['daene _ 'tee _ _] hs)
                           (membero [_ 'pallmall _ 'vogel _] hs)
                           (membero ['deutscher 'rothmanns _ _ _] hs)
                           (membero [_ 'winfield 'bier _ _ ] hs)
                           (nexto [_ 'marlboro _ _ _] [_ _ _ 'katze _] hs)
                           (nexto [_ 'dunhill _ _ _] [_ _ _ 'pferd _] hs)
                           (nexto [_ 'marlboro _ _ _] [_ _ 'wasser _ _] hs))))

(run 1 [q]
      (einsteino q))

; =>
; (([norweger dunhill wasser katze gelb] 
;   [daene marlboro tee pferd blau] 
;   [brite pallmall milch vogel rot] 
;   [deutscher rothmanns kaffee _0 gruen] 
;   [schwede winfield bier hund weiss]))

; => d.h. der Deutsche besitzt den Fisch

; Bemerkung: Bei dieser Reihenfolge der Klauseln erreicht man eine sehr gute Laufzeit,
; das Ergebnis ist im Bruchteil einer Sekunde da.
; Verändert man die Reihenfolge, wird alles viel langsamer

(defn einsteino' [hs]
  (macro/symbol-macrolet [_ (lvar)]
                         (all
                           (firsto hs ['norweger _ _ _ _])
                           (nexto ['norweger _ _ _ _] [_ _ _ _ 'blau] hs)
                           (righto [_ _ _ _ 'gruen] [_ _ _ _ 'weiss] hs)
                           (membero [_ 'dunhill _ _ 'gelb] hs)
                           (membero ['schwede _ _ 'hund _] hs)
                           (membero [_ _ 'kaffee _ 'gruen] hs)
                           #_(== [_ _ [_ _ 'milch _ _] _ _] hs)
                           (membero ['daene _ 'tee _ _] hs)
                           (== [_ _ [_ _ 'milch _ _] _ _] hs)
                           (membero [_ 'pallmall _ 'vogel _] hs)
                           (membero ['deutscher 'rothmanns _ _ _] hs)
                           (membero [_ 'winfield 'bier _ _ ] hs)
                           (nexto [_ 'marlboro _ _ _] [_ _ _ 'katze _] hs)
                           (nexto [_ 'dunhill _ _ _] [_ _ _ 'pferd _] hs)
                           (nexto [_ 'marlboro _ _ _] [_ _ 'wasser _ _] hs))))

(run 1 [q]
     (einsteino' q))
; vertauscht man diese beiden Zeilen oben, wird die Laufzeit extrem langsam!!


; TODO
; 8. Anwendungen



