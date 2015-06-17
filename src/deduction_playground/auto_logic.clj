(ns deduction-playground.auto-logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.set :as set]))

(def keywords #{'truth 'contradiction})

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
;    (list? arg) (reduce #(cons %1 %2) (list `list (list `quote (first arg))) (rest arg))
    :else (throw (Exception. "ERROR"))))

(defn gen-term
  "Converts a given list into a quoted sequence: (and a b) -> `(~'and ~a ~b)"
  [given]
  (let [
;        args (map #(list `list %) (rest given))
        args (map #(get-term-arg %) (rest given))
        operator (list `list (list `quote (first given)))
        result (conj args operator)]
    (conj (list (conj result `concat)) `seq)))

(defn gen-body-row
  "Converst an argument and an given input into a unify-logic-row:
[and1 (and a b)] -> (== and1 `(~'and ~a ~b))"
  [arg g]
  (cond 
    (symbol? g) ()
    (list? g) `(== ~arg ~(gen-term g))
    :else (throw (Exception. "Can't create body-row"))))

(defn gen-body
  "Generates all rows for the body of the function, removes empty ones"
  [args given]
  (remove #(empty? %) (map #(gen-body-row %1 %2) args given)))

(defn gen-result
  "Generates the result row: (== q <whatever>)"
  [conclusion]
;  `(== ~'q ~(if (symbol? (first conclusion))
;              (first conclusion)
;              (gen-term (first conclusion)))))
  `(== ~'q ~(cond 
              (contains? keywords (first conclusion)) (list `quote (first conclusion))
              (symbol? (first conclusion)) (first conclusion)
              (list? (first conclusion)) (gen-term (first conclusion)))))

(defn gen-fresh-args
  "Generates the arguments for the logic.fresh call.
Takes all variables from the given input und all variables from the conclusion and removes duplicates"
  [given conclusion]
  (let [gvars (reduce #(concat %1 (cond 
                                    (contains? keywords %2) []
                                    (symbol? %2) [%2]
                                    (list? %2) (rest %2)
                                    :else (throw (Exception. "Can't reduce gvars")))) [] given)
        cvars (reduce #(concat %1 (cond
                                    (contains? keywords %2) []
                                    (symbol? %2) [%2]
                                    (list? %2) (rest %2)
                                    :else (throw (Exception. "Can't reduce cvars")))) [] conclusion)
        vars (distinct (concat gvars cvars))]
    (into [] vars)))

(defn gen-logic-function
  [given conclusion]
  (let [args (gen-args given)
        fresh-args (apply vector (set/difference (set (gen-fresh-args given conclusion)) (set args)))
        body (gen-body args given)
        result (gen-result conclusion)
        fn-body (conj (concat body (list result)) fresh-args `fresh)]
    `(fn ~(conj args 'q)
       ~fn-body)))

(defn make-rule
  [rule]
  (gen-logic-function (:given rule) (:conclusion rule)))
        
        
        
        
        
        