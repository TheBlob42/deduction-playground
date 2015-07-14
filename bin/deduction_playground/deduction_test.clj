(ns deduction-playground.deduction-test
  (:require [deduction-playground.proof-new :refer [proof infer step-f step-b choose-option rename-var]]))

STOP

; Aussagenlogik:
; (16) a - f
; (a)
(def a (proof '[p] '(not (not p))))
(-> a
  (step-b "not-i" 3)
  (step-f "not-e" 1 2))

; (b)
(def b (proof '[(not (not p))] 'p))
(-> b
  (step-b "raa" 3)
  (step-f "not-e" 1 2))

; (c)
(def c (proof '[(impl p q) (not q)] '(not p)))
(-> c
  (step-b "not-i" 4)
  (step-f "impl-e" 1 3)
  (step-f "not-e" 2 4))

; (d)
(def d (proof '(or p (not p))))
(-> d
  (step-b "raa" 2)
  (step-b "not-e" 3)
  (rename-var 'V1 '(or p (not p))); die doppelte Zeile wird entfernt, die "id" wird aber nicht angepasst
  (step-b "or-i2" 3)
  (step-b "not-i" 3)
  (step-f "or-i1" 2)
  (rename-var 'V2 '(not p))
  (step-f "not-e" 1 3))
  

; (e)
(def e (proof '[(impl p q)]
              '(impl (not q) (not p))))
(-> e
  (step-b "impl-i" 3)) ; <- Modus Tollens!

; (f)
(def f (proof '[(impl p q)
                (impl (not p) q)] 'q))
(-> f
  (step-b "raa" 4)) ; <- Modus Tollens!

; (17) a - o
; (a)
(def a (proof '[(and (and p q) r)
                (and s t)]
              '(and q s)))
(-> a
  (step-f "and-e1" 1)
  (step-f "and-e2" 2)
  (step-f "and-e1" 4)
  (step-f "and-i" 3 5)
  (choose-option 6 1))

; (b)
(def b (proof '[(and p q)] '(and q p)))
(-> b
  (step-f "and-e1" 1)
  (step-f "and-e2" 1)
  (step-f "and-i" 2 3)
  (choose-option 4 1))

; (c)
(def c (proof '[(and (and p q) r)]
              '(and p (and q r))))
(-> c
  (step-f "and-e1" 1)
  (step-f "and-e2" 1)
  (step-f "and-e1" 3)
  (step-f "and-e2" 3)
  (step-f "and-i" 2 4)
  (choose-option 5 2)
  (step-f "and-i" 5 6)
  (choose-option 7 2))

; (d)
(def d (proof '[(impl p (impl p q)) p]
              'q))
(-> d
  (step-f "impl-e" 1 2)
  (step-f "impl-e" 2 3))

; (e)
(def e (proof '[(impl q (impl p r))
                (not r) q] 
              '(not p)))
(-> e
  (step-f "impl-e" 1 3)) ; <- Modus Tollens

; (f)
(def f (proof '(impl (and p q) p)))
(-> f
  (step-b "impl-i" 2)
  (step-f "and-e1" 1))

; (g)
(def g (proof '[p]
              '(impl (impl p q) q)))
(-> g
  (step-b "impl-i" 3)
  (step-f "impl-e" 1 2))

; (h)
(def h (proof '[(and (impl p r)
                     (impl q r))]
              '(impl (and p q) r)))
(-> h
  (step-b "impl-i" 3)
  (step-f "and-e1" 1)
  (step-f "and-e1" 3)
  (step-f "impl-e" 2 4))
(-> h
  (step-f "and-e1" 1)
  (step-b "impl-i" 4)
  (step-f "and-e1" 3)
  (step-f "impl-e" 2 4))

; (i)
(def i (proof '[(impl q r)]
              '(impl (impl p q)
                     (impl p r))))
(-> i
  (step-b "impl-i" 3)
  (step-b "impl-i" 4)
  (step-f "impl-e" 2 3)
  (step-f "impl-e" 1 4))

; (j)
(def j (proof '[(impl p q) (impl r s)]
              '(impl (or p r) (or q s))))
(-> j
  (step-b "impl-i" 4)
  (step-b "or-e" 5)
  (rename-var 'V1 'p)
  (rename-var 'V2 'r) ; doppelte Zeile wird entfernt, aber id nicht angepasst
  (step-f "impl-e" 1 4)
  (step-b "or-i1" 7)
  (step-f "impl-e" 2 7)
  (step-b "or-i2" 10))

  
; (k)
(def k (proof '[(and (or p (impl q p)) q)]
              'p))
(-> k
  (step-f "and-e1" 1)
  (step-f "and-e2" 1) 
  (step-b "or-e" 5)
  (rename-var 'V1 'p)
  (rename-var 'V2 '(impl q p))
  (step-f "impl-e" 2 5)) ; falsche ids (ganz besonders extrem)

; (l)
(def l (proof '[(impl p q) (impl r s)]
              '(impl (and p r) (and q s))))
(-> l
  (step-b "impl-i" 4)
  (step-f "and-e1" 3)
  (step-f "and-e2" 3)
  (step-f "impl-e" 1 5)
  (step-f "impl-e" 2 4)
  (step-b "and-i" 9))

; (m)
(def m (proof '[(impl p (and q r))]
              '(and (impl p q) (impl p r))))
(-> m
  (step-b "and-i" 3)
  (step-b "impl-i" 3) 
  (step-b "impl-i" 6)
  (step-f "impl-e" 1 2)
  (step-f "and-e1" 3)
  (step-f "impl-e" 1 6)
  (step-f "and-e2" 7))
  

; (n)
(def n (proof '[(or p (and p q))] 'p))
(-> n
  (step-b "or-e" 3) 
  (rename-var 'V1 'p)
  (rename-var 'V2 '(and p q))
  (step-f "and-e1" 3)) ; ids falsch übernommen...

; (o)
(def o (proof '[(or (and p q) (and p r))]
              '(and p (or q r))))
(-> o
  (step-b "or-e" 3)
  (rename-var 'V1 '(and p q))
  (rename-var 'V2 '(and p r))
  (step-f "and-e1" 2)
  (step-f "and-e2" 2)
  (step-f "or-i1" 3)
  (rename-var 'V3 'r)
  (step-f "and-i" 4 5)
  (choose-option 6 2)
  (step-f "and-e1" 7)
  (step-f "and-e2" 7)
  (step-f "or-i2" 8)
  (rename-var 'V4 'q)
  (step-f "and-i" 9 10)
  (choose-option 11 2)) ; ids stimmen wieder nicht
  


; (18) a - e
; (a)
(def a (proof '(impl (impl (impl p q) p) p)))
(-> a
  (step-b "impl-i" 2)
  (step-b "raa" 3)
  (step-b "not-e" 4)) ; var renaming fehlt

; (b)
(def b (proof '[(not (and p q))]
              '(or (not p) (not q))))
(-> b
  (step-b "raa" 3)
  (step-f "not-e" )) ; siehe (a)

; (c)
(def c (proof '[(or (not p) (not q))]
              '(not (and p q))))
(-> c
  (step-b "not-i" 3)
  (step-f "and-e1" 2)
  (step-f "and-e2" 2)
  (step-b "or-e" 6)) ; <- Möglichkeit Variablen Umzubenennen fehlt

; (d)
(def d (proof '[(not (or p q))]
              '(and (not p) (not q))))
(-> d
  (step-b "and-i" 3)
  (step-b "not-i" 3)
  (step-b "not-i" 6))
; need "not-e" backwards + var renaming

; (e)
(def e (proof '[(and (not p) (not q))]
              '(not (or p q))))
(-> e
  (step-b "not-i" 3))
; need var renaming


; Prädikatenlogik
; (12) a - c
; (13) a - d
; (11) a - j
; (a)
(def a (proof '[(not (forall [x] (P x)))]
              '(exists [x] (not (P x)))))
(-> a
  (step-b "raa" 3) ; fehlt var renaming
;  (step-f "
  
  
  
  
  
  
  