(ns deduction-playground.repl
  (:require [deduction-playground.proof-new :as proof]
            [deduction-playground.read-rules :as read]
            [deduction-playground.printer :refer [pprint]]))

(read/read-rules "resources/rules.clj")
(read/read-theorems "resources/theorems.clj")

(def p (atom []))

(defn show
  []
  (pprint @p))

(defn proof
  [premises formula]
  (reset! p (proof/proof premises formula))
  (show))

(defn step-f
  [rule & lines]
  (swap! p #(apply proof/step-f (conj (conj lines rule) %)))
  (show))

(defn step-b
  [rule & lines]
  (swap! p #(apply proof/step-b (conj (conj lines rule) %)))
  (show))

(defn choose-option
  [line num]
  (swap! p proof/choose-option line num)
  (show))

(defn rename-var
  [old new]
  (swap! p proof/rename-var old new)
  (show))

