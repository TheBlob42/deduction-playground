(ns deduction-playground.repl
  (:require [deduction-playground.deduction :as deduc]
            [deduction-playground.io :as io]
            [deduction-playground.printer :refer [pprint]]))

(io/import-rules "resources/rules-prop-prep.clj")
;(io/import-rules "resources/rules-temporal.clj")
(io/import-classicals "resources/classical-theorems.clj")
(io/import-theorems "resources/theorems.clj")

;; holds the actual state of the proof 
(def p (atom []))
;; holds the last steps since the beginning of the proof (for undo steps)
(def last_steps (atom []))

(defn show
  "Print the actual state of the proof"
  []
  (pprint @p))

(defn proof
  "Start a new proof"
  ([formula] (proof [] formula))
  ([premises formula]
    (reset! last_steps [])
    (reset! p (deduc/proof premises formula))
    (show)))

(defn step-f
  "Execute a forward step"
  [rule & lines]
  (swap! last_steps conj @p)
  (swap! p #(apply deduc/step-f (conj (conj lines rule) %)))
  (show))

(defn step-f-inside
  "Executs a forward step inside the chosen line"
  [rule line]
  (swap! last_steps conj @p)
  (swap! p #(apply deduc/step-f-inside (conj line rule %)))
  (show))

(defn step-b
  "Execute a backward step"
  [rule & lines]
  (swap! last_steps conj @p)
  (swap! p #(apply deduc/step-b (conj (conj lines rule) %)))
  (show))

(defn choose-option
  "Choose an option in the chosen line"
  [line num]
  (swap! last_steps conj @p)
  (swap! p deduc/choose-option line num)
  (show))

(defn rename-var
  "Rename a variable"
  [old new]
  (swap! last_steps conj @p)
  (swap! p deduc/rename-var old new)
  (show))

(defn classical
  "Apply the classical-theorems inside the chosen line"
  [line]
  (swap! last_steps conj @p)
  (swap! p deduc/classical line)
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
  (if (empty? @last_steps)
    (println "You reached the starting point, there is nothing more to undo")
    (do
      (reset! p (last @last_steps))
      (swap! last_steps #(into [] (drop-last %)))
      (show))))
 
  
