(ns deduction-playground.auto-logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.set :as set]))

(def keywords #{'truth 'contradiction})

;TODO
; - erlaubte operatoren 
; - mehrere Conclusionen

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
    (symbol? g) ()
    (list? g) `(== ~arg ~(gen-term g))
    :else (throw (Exception. "Can't create body-row"))))

(defn gen-body
  "Generates all rows for the body of the function, removes empty ones"
  [args given]
  (remove #(empty? %) (map #(gen-body-row %1 %2) args given)))

;(defn gen-result
;  "Generates the result row: (== q <whatever>)"
;  [conclusion]
;  `(== ~'q ~(cond 
;              (contains? keywords (first conclusion)) (list `quote (first conclusion))
;              (symbol? (first conclusion)) (first conclusion)
;              (list? (first conclusion)) (gen-term (first conclusion)))))

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
        
        
        
        
        
        