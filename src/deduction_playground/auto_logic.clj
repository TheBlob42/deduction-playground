(ns deduction-playground.auto-logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.set :as set]
            [deduction-playground.read-rules :refer [read-rules]]
            [clojure.math.combinatorics :as combo]))

(def rules (read-rules))

(def keywords #{'truth 'contradiction})

;TODO
; - erlaubte operatoren (Terminale)

(defn gen-arg 
  "Turns an input into an argument for the logic function: symbols are passed trough, lists are numbered"
  [x n]
  (cond 
    (symbol? x) x
    (list? x) (symbol (str (first x) n))
    :else (throw (Exception. "Can't generate argument"))))
    
(defn gen-args
  "Generates the arguments for the logic function."
  [given]
  (let [numbers (take (count given) (iterate inc 1))]
    (into [] (map #(gen-arg %1 %2) given numbers))))

(defn get-term-arg
  [arg]
  (cond
    (contains? keywords arg) (list `list (list `quote arg))
    (symbol? arg) (list `list arg)
    (list? arg) (list `list (concat (list `concat) 
                                    (list (list `list (list `quote (first arg)))) 
                                    (map #(get-term-arg %) (rest arg))))
    (vector? arg) (list `list (list `apply `vector (concat (list `concat) (map #(get-term-arg %) arg))))
    :else (throw (Exception. "ERROR"))))

(defn gen-term
  "Converts a given list into a quoted sequence: (and a b) -> `(~'and ~a ~b)"
  [given]
  (let [args (map #(get-term-arg %) (rest given))
        operator (list `list (list `quote (first given)))
        result (conj args operator)]
    (conj (list (conj result `concat)) `seq)))

(defn gen-body-row
  "Converst an argument and an given input into a unify-logic-row:
[and1 (and a b)] -> (== and1 `(~'and ~a ~b))"
  [arg g]
  (cond 
    (contains? keywords g) `(== ~arg ~(list `quote arg))
    (symbol? g) ()
    (list? g) `(== ~arg ~(gen-term g))
    :else (throw (Exception. "Can't create body-row"))))

(defn gen-body
  "Generates all rows for the body of the function, removes empty ones"
  [args given]
  (remove empty? (map #(gen-body-row %1 %2) args given)))

(defn gen-result-row
  [c]
  `(== ~'q ~(cond 
              (contains? keywords c) (list `quote c)
              (symbol? c) c
              (list? c) (gen-term c))))

(defn gen-result
  [conclusion]
  (let [rows (map #(gen-result-row %) conclusion)
        res (for [r rows]
              (vector r))]
    (conj res `conde)))

(defn gen-fresh-arg
  [arg]
  (cond 
    (contains? keywords arg) []
    (symbol? arg) [arg]
    (list? arg) (reduce #(concat %1 (gen-fresh-arg %2)) [] (rest arg))
    (vector? arg) (reduce #(concat %1 (gen-fresh-arg %2)) [] arg)));TODO Vektoren auslesen

(defn gen-fresh-args
  [given conclusion]
  (let [gvars (reduce #(concat %1 (gen-fresh-arg %2)) [] given)
        cvars (reduce #(concat %1 (gen-fresh-arg %2)) [] conclusion)
        vars (distinct (concat gvars cvars))]
    (into [] vars)))

;(defn gen-logic-function
;  [given conclusion]
;  (let [args (gen-args given)
;        fresh-args (apply vector (set/difference (set (gen-fresh-args given conclusion)) (set args)))
;        body (gen-body args given)
;        result (gen-result conclusion)
;        fn-body (conj (concat body (list result)) fresh-args `fresh)]
;    `(fn ~(conj args 'q)
;       ~fn-body)))

; MULTIPLE CONCLUSION TESTING
(defn gen-result-row1
  [q c]
  `(== ~q ~(cond 
              (contains? keywords c) (list `quote c)
              (symbol? c) c
              (list? c) (gen-term c))))

(defn gen-result1
  [conclusion qs]
  (map #(gen-result-row1 %1 %2) qs conclusion))

(defn gen-logic-function
  [given conclusion]
  (let [qs (map #(symbol (str %1 %2)) (take (count conclusion) (cycle ['q])) (take (count conclusion) (iterate inc 1)))
        args (gen-args given)
        fresh-args (apply vector (set/difference (set (gen-fresh-args given conclusion)) (set args)))
        body (gen-body args given)
        result (gen-result1 conclusion qs)
        fn-body (conj (concat body result) fresh-args `fresh)]
    `(fn ~(apply conj args qs)
       ~fn-body)))
; *************************** 

(defn make-rule
  "Takes a map or string (related to one of the rules in the \"rules\"-map) to create a function"
  [rule]
  (cond
    (map? rule)
    (gen-logic-function (:given rule) (:conclusion rule))
  
    (string? rule)
    (let [r ((keyword rule) rules)]
      (gen-logic-function (:given r) (:conclusion r)))
    :else "ERROR"))

 (defn apply-rule
   "Applies a rule to the given arguments and returns the result(s)"
   [name forward? & args]
   (let  [r ((keyword name) rules)
          rule (if forward? r (assoc r :given (:conclusion r) :conclusion (:given r)))
          args-list (map #(conj (list %) `quote) args)
          logic-args (into [] (map #(symbol (str %1 %2)) 
                                    (take (count (:conclusion rule)) (cycle ['q]))
                                    (take (count (:conclusion rule)) (iterate inc 1))))
          fn (eval (make-rule rule))
          fn-args (concat args-list logic-args)]
     (eval (list `run* logic-args (conj fn-args fn)))))
 
 ;TODO Copy "permutations" function from combo-namespace (we don't need the rest)
 
;  (defn apply-rule1
;   "Applies a rule to the given arguments and returns the result(s)"
;   [name forward? & args]
;   (let  [r ((keyword name) rules)
;          rule (if forward? r (assoc r :given (:conclusion r) :conclusion (:given r)))
;          args-list (map #(conj (list %) `quote) args)
;          permutations (combo/permutations args-list)
;          logic-args (into [] (map #(symbol (str %1 %2)) 
;                                    (take (count (:conclusion rule)) (cycle ['q]))
;                                    (take (count (:conclusion rule)) (iterate inc 1))))
;          fn (eval (make-rule rule))
;          fn-args (concat args-list logic-args)]
;     (map first (remove empty? (for [x permutations]
;                                 (eval (list `run* logic-args (conj (concat x logic-args) fn))))))))
  
  (defn apply-rule1
    [name & args]
    (let [rule ((keyword name) rules)
          rarg-count (+ (count (:given rule)) (count (:conclusion rule)))
          args-list (map #(conj (list %) `quote) args)
          logic-args (into [] (map #(symbol (str %1 %2))
                                   (take (- rarg-count (count args)) (cycle ['q]))
                                   (take (- rarg-count (count args)) (iterate inc 1))))
          fn (eval (make-rule rule))
          fn-args (concat args-list logic-args)
          permutations (combo/permutations fn-args)]
      (map first (remove empty? (for [x permutations]
                                  (eval (list `run* logic-args (conj x fn))))))))
          
 
   
 ;HELPER FUNCTIONS
 (defn count-rule-args
    [rule forward?]
    (if forward? 
      (count (:given ((keyword rule) rules)))
      (count (:conclusion ((keyword rule) rules)))))
 
 (defn does-rule-exist?
   [rule]
   (if ((keyword rule) rules) true false))
 
 