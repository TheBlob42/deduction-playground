(ns deduction-playground.repl
  (:require [deduction-playground.proof-new :as proof]
            [deduction-playground.io :as io]
            [deduction-playground.printer :refer [pprint]]))

(io/import-rules "resources/rules.clj")
;(io/import-rules "resources/rules-temporal.clj")
(io/import-classicals "resources/classical-theorems.clj")
(io/import-theorems "resources/theorems.clj")

;; holds the actual state of the proof 
(def p (atom []))
;; holds the second actual state of the proof for undo steps
(def last_step (atom []))

(defn show
  "Print the actual state of the proof"
  []
  (pprint @p))

(defn proof
  "Start a new proof"
  ([formula] (proof [] formula))
  ([premises formula]
    (reset! last_step [])
    (reset! p (proof/proof premises formula))
    (show)))

(defn step-f
  "Execute a forward step"
  [rule & lines]
  (reset! last_step @p)
  (swap! p #(apply proof/step-f (conj (conj lines rule) %)))
  (show))

(defn step-f-inside
  "Executs a forward step inside the chosen line"
  [rule line]
  (reset! last_step @p)
  (swap! p #(apply proof/step-f-inside (conj line rule %)))
  (show))

(defn step-b
  "Execute a backward step"
  [rule & lines]
  (reset! last_step @p)
  (swap! p #(apply proof/step-b (conj (conj lines rule) %)))
  (show))

(defn choose-option
  "Choose an option in the chosen line"
  [line num]
  (reset! last_step @p)
  (swap! p proof/choose-option line num)
  (show))

(defn rename-var
  "Rename a variable"
  [old new]
  (reset! last_step @p)
  (swap! p proof/rename-var old new)
  (show))

(defn classical
  "Apply the classical-theorems inside the chosen line"
  [line]
  (reset! last_step @p)
  (swap! p proof/classical line)
  (show))

(defn export-theorem
  "Export the solved proof to a file as a theorem"
  [filename name]
  (io/export-theorem 
    @p
    filename
    name))

(defn undo []
  "Undo the last change (you can't go further than the last state)"
  (if (empty? @last_step)
    (println "There is nothing to undo (you can only return to the last state not further)")
    (do
      (reset! p @last_step)
      (reset! last_step [])
      (show))))
 
  
