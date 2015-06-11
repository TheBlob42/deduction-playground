(ns deduction-playground.logic-test
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.set :as s]))

(defn and-e-l
  [and a]
  (fresh [b]
         (== and `(~'and ~a ~b))))

(defn ande1o
  [a and]
  (fresh [b]
         (== and `(~a ~'& ~b))))


(defn m []
  (let [args '[and a]
        args2 '[b]]
    `(fn ~args 
       (fresh ~args2
              (== (first ~args) `(~'~(first args) ~~(second args) ~~(first args2)))))))

(def rule {:name "test"
           :operator '[and]
           :arguments '[a b]
           :result '[a]})

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

