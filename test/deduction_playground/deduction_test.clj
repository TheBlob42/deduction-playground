(ns deduction-playground.deduction-test
  (:require [deduction-playground.deduction :refer [proof step-f step-f-inside step-b classical choose-option rename-var]]
            [deduction-playground.io :as io]
            [deduction-playground.printer :refer [pprint]]))

(io/import-rules "resources/rules-prop-prep.clj")
(io/import-classicals "resources/classical-theorems.clj")
(io/import-theorems "resources/theorems.clj")

STOP

;; Modus Tollens
(pprint (-> (proof '[(impl a b) (not b)] '(not a))
          (step-b "not-i" 4)
          (step-f "impl-e" 1 3)
          (step-f "not-e" 2 4)))
(io/export-theorem
  (-> (proof '[(impl a b) (not b)] '(not a))
    (step-b "not-i" 4)
    (step-f "impl-e" 1 3)
    (step-f "not-e" 2 4))
  "resources/theorems.clj"
  "modus-tollens")

; Aussagenlogik:
; (16) a - f
; (a)
(pprint (-> (proof 'p '(not (not p)))
          (step-b "not-i" 3)
          (step-f "not-e" 1 2)))

; (b)
(pprint (-> (proof '(not (not p)) 'p)
          (step-b "raa" 3)
          (step-f "not-e" 1 2)))

; (c)
(pprint (-> (proof '[(impl p q) (not q)] '(not p))
          (step-b "not-i" 4)
          (step-f "impl-e" 1 3)
          (step-f "not-e" 2 4)))

; (d)
(pprint (-> (proof '(or p (not p)))
          (step-b "raa" 2)
          (step-b "not-e" 1 3)
          (choose-option 3 2)
          (step-b "or-i2" 3)
          (step-b "not-i" 3)
          (step-f "or-i1" 2)
          (rename-var 'V1 '(not p))
          (step-f "not-e" 1 3)
          ))

; (e)
(pprint (-> (proof '(impl p q)
                   '(impl (not q) (not p)))
          (step-b "impl-i" 3)
          (step-f "modus-tollens" 1 2)
          ))

; (f)
(pprint (-> (proof '[(impl p q)
                     (impl (not p) q)] 'q)
          (step-b "raa" 4)
          (step-f "modus-tollens" 1 3)
          (step-f "impl-e" 2 4)
          (step-f "not-e" 3 5)
          ))

; (17) a - o
; (a)
(pprint (-> (proof '[(and (and p q) r)
                     (and s t)]
                   '(and q s))
          (step-f "and-e1" 1)
          (step-f "and-e2" 3)
          (step-f "and-e1" 2)
;          (step-b "and-i" 7))) ; Alternative
          (step-f "and-i" 4 5)
          (choose-option 6 1)))

; (b)
(pprint (-> (proof '(and p q) '(and q p))
          (step-f "and-e1" 1)
          (step-f "and-e2" 1)
;          (step-b "and-i" 5))) ; Alternative
          (step-f "and-i" 2 3)
          (choose-option 4 2)))

; (c)
(pprint (-> (proof '(and (and p q) r) '(and p (and q r)))
          (step-f "and-e1" 1)
          (step-f "and-e2" 1)
          (step-f "and-e1" 2)
          (step-f "and-e2" 2)
          (step-f "and-i" 3 5)
          (choose-option 6 2)
          (step-f "and-i" 4 6)
          (choose-option 7 1)))

; (d)
(pprint (-> (proof '[(impl p (impl p q)) p]
                   'q)
          (step-f "impl-e" 1 2)
          (step-f "impl-e" 2 3)))

; (e)
(pprint (-> (proof '[(impl q (impl p r))
                     (not r) q]
                   '(not p))
          (step-f "impl-e" 1 3)
          (step-f "modus-tollens" 2 4)))

; (f)
(pprint (-> (proof '(impl (and p q) p))
          (step-b "impl-i" 2)
          (step-f "and-e1" 1)))

; (g)
(pprint (-> (proof '[p]
                   '(impl (impl p q) q))
        (step-b "impl-i" 3)
        (step-f "impl-e" 1 2)))

; (h)
(pprint (-> (proof '[(and (impl p r)
                     (impl q r))]
                   '(impl (and p q) r))
          (step-f "and-e1" 1)
          (step-b "impl-i" 4)
          (step-f "and-e1" 3)
          (step-f "impl-e" 2 4)))

; (i)
(pprint (-> (proof '[(impl q r)]
                   '(impl (impl p q)
                          (impl p r)))
          (step-b "impl-i" 3)
          (step-b "impl-i" 4)
          (step-f "impl-e" 2 3)
          (step-f "impl-e" 1 4)))

; (j)
(pprint (-> (proof '[(impl p q) (impl r s)]
                   '(impl (or p r) (or q s)))
          (step-b "impl-i" 4)
          (step-b "or-e" 5 3)
          (choose-option 5 1)
          (step-f "impl-e" 2 4)
          (step-f "or-i2" 5)
          (rename-var 'V1 'q)
          (step-f "impl-e" 7 1)
          (step-f "or-i1" 8)
          (rename-var 'V2 's)))
  
; (k)
(pprint (-> (proof '[(and (or p (impl q p)) q)]
                   'p)
          (step-f "and-e1" 1)
          (step-f "and-e2" 1)
          (step-b "or-e" 5 2)
          (choose-option 5 1)
          (step-f "impl-e" 4 3)))

; (l)
(pprint (-> (proof '[(impl p q) (impl r s)]
                   '(impl (and p r) (and q s)))
          (step-b "impl-i" 4)
          (step-f "and-e1" 3)
          (step-f "and-e2" 3)
          (step-f "impl-e" 2 5)
          (step-f "impl-e" 1 4)
          (step-b "and-i" 9)))

; (m)
(pprint (-> (proof '[(impl p (and q r))]
                   '(and (impl p q) (impl p r)))
          (step-b "and-i" 3)
          (step-b "impl-i" 3)
          (step-b "impl-i" 7)
          (step-f "impl-e" 1 2)
          (step-f "and-e2" 3)
          (step-f "impl-e" 1 5)
          (step-f "and-e1" 6)))
  

; (n)
(pprint (-> (proof '(or p (and p q)) 'p)
          (step-b "or-e" 1 3)
          (choose-option 3 1)
          (step-f "and-e1" 2)
          ))

; (o)
(pprint (-> (proof '[(or (and p q) (and p r))]
              '(and p (or q r)))
          (step-b "or-e" 3 1)
          (choose-option 3 1)
          (step-f "and-e1" 2)
          (step-f "and-e2" 2)
          (step-f "or-i2" 4)
          (rename-var 'V1 'q)
          (step-f "and-i" 3 5)
          (choose-option 6 1)
          (step-f "and-e1" 7)
          (step-f "and-e2" 7)
          (step-f "or-i1" 9)
          (rename-var 'V2 'r)
          (step-f "and-i" 8 10)
          (choose-option 11 1)))
  


; (18) a - e
; (a)
(pprint (-> (proof '(impl (impl (impl p q) p) p))
          (step-b "impl-i" 2)
          (step-b "raa" 3)
          (step-b "not-e" 4 2)
          (choose-option 4 2)
          (step-b "impl-e" 4 1)
          (choose-option 4 2)
          (step-b "impl-i" 4)
          (step-f "not-e" 2 3)
          (step-f "efq" 4)
          (rename-var 'V1 'q)
          ))

; (b)
(pprint (-> (proof '(not (and p q))
                   '(or (not p) (not q)))
          (step-b "raa" 3)
          (step-b "not-e" 1 4)
          (choose-option 4 2)
          (step-b "and-i" 4)
          (step-b "raa" 4)
          (step-b "raa" 8)
          (step-f "or-i2" 3)
          (rename-var 'V1 '(not p))
          (step-f "not-e" 2 4)
          (step-f "or-i1" 6)
          (rename-var 'V2 '(not q))
          (step-f "not-e" 2 7)
          ))

; (c)
(pprint (-> (proof '(or (not p) (not q))
                   '(not (and p q)))
          (step-b "not-i" 3)
          (step-f "and-e1" 2)
          (step-f "and-e2" 2)
          (step-b "or-e" 1 6)
          (choose-option 6 1)
          (step-f "not-e" 5 4)
          (step-f "not-e" 3 7)
          ))

; (d)
(pprint (-> (proof '(not (or p q))
                   '(and (not p) (not q)))
          (step-b "and-i" 3)
          (step-b "not-i" 3)
          (step-b "not-i" 7)
          (step-b "not-e" 1 4)
          (choose-option 4 2)
          (step-f "or-i2" 2)
          (rename-var 'V1 'p)
          (step-b "not-e" 7 1)
          (choose-option 7 2)
          (step-f "or-i1" 5)
          (rename-var 'V2 'q)
          ))

; (e)
(pprint (-> (proof '(and (not p) (not q))
                   '(not (or p q)))
          (step-b "not-i" 3)
          (step-b "or-e" 2 4)
          (choose-option 4 1)
          (step-f "and-e1" 1)
          (step-f "and-e2" 1)
          (step-f "not-e" 2 5)
          (step-f "not-e" 3 7)
          ))


; Prädikatenlogik
; (11) a - j
; (a)
(pprint (-> (proof '(not (forall [x] (P x)))
                   '(exists [x] (not (P x))))
          (step-b "raa" 3)
          (step-b "not-e" 1 4)
          (choose-option 4 2)
          (step-b "forall-i" 4)
          (step-b "raa" 5)
          (step-b "not-e" 2 6)
          (choose-option 6 2)
          (step-b "exists-i" 6 3)
          ))

; (b)
(pprint (-> (proof '(exists [x] (not (P x)))
                   '(not (forall [x] (P x))))
          (step-b "raa" 3)
          (classical 2)
          (step-b "exists-e" 1 5)
          (step-f "forall-e" 3 4)
          (step-f "not-e" 5 6)
          ))

; (c)
(pprint (-> (proof '(not (exists [x] (P x)))
                   '(forall [x] (not (P x))))
          (step-b "forall-i" 3)
          (step-b "not-i" 4)
          (step-b "not-e" 1 5)
          (choose-option 5 2)
          (step-b "exists-i" 5 2)
          ))

; (d)
(pprint (-> (proof '(forall [x] (not (P x)))
                   '(not (exists [x] (P x))))
          (step-b "not-i" 3)
          (step-b "exists-e" 4 2)
          (step-f "forall-e" 1 3)
          (step-f "not-e" 4 5)
          ))

; (e)
(pprint (-> (proof '(and (forall [x] (P x)) (forall [x] (Q x)))
                   '(forall [x] (and (P x) (Q x))))
          (step-f "and-e1" 1)
          (step-f "and-e2" 1)
          (step-b "forall-i" 5)
          (step-f "forall-e" 2 4)
          (step-f "forall-e" 3 4)
          (step-f "and-i" 5 6)
          (choose-option 7 1)
          ))

; (f)
(pprint (-> (proof '(forall [x] (and (P x) (Q x)))
                   '(and (forall [x] (P x)) (forall [x] (Q x))))
          (step-b "and-i" 3)
          (step-b "forall-i" 3)
          (step-f "forall-e" 1 2)
          (step-f "and-e2" 3)
          (step-b "forall-i" 7)
          (step-f "forall-e" 1 5)
          (step-f "and-e1" 6) 
          ))

; (g)
(pprint (-> (proof '(or (exists [x] (P x))
                         (exists [x] (Q x)))
                   '(exists [x] (or (P x) (Q x))))
          (step-b "or-e" 3 1)
          (choose-option 3 1)
          (step-b "exists-e" 2 4)
          (step-b "exists-i" 3 6)
          (step-f "or-i2" 4)
          (rename-var 'V2 '(P V1))
          (step-b "exists-e" 8 10)
          (step-b "exists-i" 12 9)
          (step-f "or-i1" 10)
          (rename-var 'V4 '(Q V3))
          ))

; (h)
(pprint (-> (proof '(exists [x] (or (P x) (Q x)))
                   '(or (exists [x] (P x)) (exists [x] (Q x))))
          (step-b "exists-e" 1 3)
          (step-b "or-e" 5 3)
          (choose-option 5 1)
          (step-b "or-i2" 6)
          (step-b "exists-i" 6 2)
          (step-b "or-i1" 9)
          (step-b "exists-i" 9 2) 
          ))

; (i)
(pprint (-> (proof '(or (forall [x] (P x)) (forall [x] (Q x)))
                   '(forall [x] (or (P x) (Q x))))
          (step-b "or-e" 3 1)
          (choose-option 3 1)
          (step-b "forall-i"  4)
          (step-f "forall-e" 2 3)
          (step-f "or-i2" 4)
          (rename-var 'V2 '(P V1))
          (step-b "forall-i" 9)
          (step-f "forall-e" 7 8)
          (step-f "or-i1" 9)
          (rename-var 'V4 '(Q V3)) 
          ))
  
; (j)
(pprint (-> (proof '(exists [x] (and (P x) (Q x)))
                   '(and (exists [x] (P x)) (exists [x] (Q x))))
          (step-b "exists-e" 1 3)
          (step-b "and-i" 5)
          (step-b "exists-i" 5 2)
          (step-b "exists-i" 7 2)
          (step-f "and-e1" 3)
          (step-f "and-e2" 3)
          ))
  
; (12) a - c
; (a)
(pprint (-> (proof '(forall [x] (forall [y] (P x y)))
                   '(forall [u] (forall [v] (P u v))))
          (step-b "forall-i" 3)
          (step-b "forall-i" 4)
          (step-f "forall-e" 1 2)
          (step-f "forall-e" 3 4)
          ))

; (b)
(pprint (-> (proof '(exists [x] (exists [y] (F x y)))
                   '(exists [u] (exists [v] (F u v))))
          (step-b "exists-e" 3 1)
          (step-b "exists-i" 5 2)
          (rename-var 'y 'v)
          ))

; (c)
(pprint (-> (proof '(exists [x] (forall [y] (P x y)))
                   '(forall [y] (exists [x] (P x y))))
          (step-b "exists-e" 1 3)
          (step-b "forall-i" 5)
          (step-b "exists-i" 2 6)
          (step-f "forall-e" 3  4)
          ))

; (13) a - d
; (a)
(pprint (-> (proof '(exists [x] (impl S (Q x)))
                   '(impl S (exists [x] (Q x))))
          (step-b "impl-i" 3)
          (step-b "exists-e" 1 4)
          (step-f "impl-e" 2 4)
          (step-b "exists-i" 3 7)
          ))

; (b)
(pprint (-> (proof '(not (forall [x] (not (P x))))
                   '(exists [x] (P x)))
          (step-b "raa" 3)
          (step-b "not-e" 4 1)
          (choose-option 4 2)
          (step-b "forall-i" 4)
          (step-b "not-i" 5)
          (step-b "not-e" 2 6)
          (choose-option 6 2)
          (step-b "exists-i" 6 3)
          ))

(io/export-theorem
 (-> (proof '(forall [x] (not (P x)))
             '(not (exists [x] (P x))))
   (step-b "raa" 3)
   (classical 2)
   (step-b "exists-e" 5 3)
   (step-f "forall-e" 1 4)
   (step-f "not-e" 6 5))
 "resources/theorems.clj"
 "rule-13-b")
  
; (c)
(pprint (-> (proof '(exists [x] (P x))
                   '(not (forall [x] (not (P x)))))
          (step-b "raa" 3)
          (classical 2)
          (step-f "rule-13-b" 3)
          (step-f "not-e" 1 4)))

; (d)
(pprint (-> (proof '(impl S (forall [x] (Q x)))
                   '(forall [x] (impl S (Q x))))
          (step-b "forall-i" 3)
          (step-b "impl-i" 4)
          (step-f "impl-e" 1 3)
          (step-f "forall-e" 2 4)
          ))
  
  
  
  
  