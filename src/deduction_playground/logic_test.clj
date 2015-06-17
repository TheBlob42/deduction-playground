(ns deduction-playground.logic-test
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.set :as s]))

; example core.logic functions
; NOTIZEN: 
; - q ist immer der letze Parameter (oder der erste)
; - Anzahl der core.logic Parameter = Anzahl der Regel Parameter (:given) + q
; - Anzahl der fresh Parameter = 
; ***** AND *****
(defn and-i
  [a b q]
  (== q `(~'and ~a ~b)))

(defn and-e-l
  [and q]
  (fresh [b]
         (== and `(~'and ~q ~b))))

(defn and-e-r
  [and q]
  (fresh [a]
         (== and `(~'and ~a ~q))))

; ***** OR *****
(defn or-i-l
  [a q]
  (fresh [b] 
         (== q `(~'or ~a ~b)))) 

(defn or-i-r
  [b q]
  (fresh [a]
         (== q `(~'or ~a ~b))))

(defn or-e
  [or proof1 proof2 q]
  (fresh [a b x]
         (== or `(~'or ~a ~b))
         (== proof1 `(~'infer ~a ~x))
         (== proof2 `(~'infer ~b ~x))
         (== q x)))

; ***** IMPL *****
(defn impl-i
  [proof q]
  (fresh [a b]
         (== proof `(~'infer ~a ~b))
         (== q `(~'impl ~a ~b))))

(defn impl-e
  [a impl q]
  (fresh [b]
         (== impl `(~'impl ~a ~b))
         (== q b)))

; ***** NOT *****
(defn not-i
  [proof q]
  (fresh [a]
         (== proof `(~'infer ~a ~'contradiction))
         (== q `(~'not ~a))))

(defn not-e
  [a not-a q]
  (== `(~'not ~a) not-a)
  (== q 'contradiction))
; ***** RAA/CONTRA *****
(defn raa
  [proof q]
  (fresh [a]
         (== proof `(~'infer (~'not ~a) ~'contradiction))
         (== q a)))

(defn efq
  [con q]
  (== con 'contradiction)
  ; woher kommt a (die Formel)?
  (== q 'a))
; ***** EQUAL *****
(defn equal-i
  [q]
  (== q '(= t t)))

(defn equal-e
  [equal subs q]
  (fresh [a b phi x]
         (== equal `(~'= ~a ~b))
         (== subs `(~'substitution ~phi ~a ~x)); oder a b einbauen
         (== q `(~'substitution ~phi ~b ~x))))

; ***** FORALL *****
(defn forall-i
  [proof q]
  (fresh [x0 x phi]
         ;x0 muss (actual x0) sein
         (== proof `(~'infer ~x0 (~'substitution ~phi ~x0 ~x)))
         (== q `(~'forall [~x] ~phi))))

(defn forall-e
  [forall q]
  (fresh [x phi t]
         (== forall `(~'forall [~x] ~phi))
         (== q `(~'substitution ~phi ~t ~x)))) 

; ***** EXISTS *****
(defn exists-i
  [subs q]
  (fresh [t x phi]
         ; muss t nicht (actual t) sein ?
         (== subs `(~'substitution ~phi ~t ~x))
         (== q `(~'exists [~x] ~phi))))

(defn exists-e
  [ex proof q]
  (fresh [x0 phi x chi]
         (== ex `(~'exists [~x] ~phi))
         (== proof `(~'infer [~x0 (~'substitution ~phi ~x0 ~x)] ~chi))
         (== q chi)))

; WILD TESTING 
(defn m []
  (let [args '[and a]
        args2 '[b]]
    `(fn ~args 
       (fresh ~args2
              (== (first ~args) `(~'~(first args) ~~(second args) ~~(first args2)))))))

(def rule {:name "and-e-l"
           :operator '[and]
           :arguments '[a b]
           :result '[a]})

(def rule2 {:name "and-i"
            :operator '[and]
            :arguments '[a b]
            :result '[and a b]})

(defn diff
  [vec1 vec2]
  (let [s1 (set vec1)
        s2 (set vec2)]
    (vec (s/difference s1 s2))))
  

(defn make [rule]
  (let [op (first (:operator rule))
        args (into [] (concat (:operator rule) (:result rule)))
        fargs (diff (:arguments rule) (:result rule))]
    `(fn ~args
       (fresh ~fargs
              (== ~op `(~'~op ~~(second args) ~~(first fargs)))))))

