(ns deduction-playground.repl
  (:require [deduction-playground.deduction :as deduc]
            [deduction-playground.prereqs :refer :all]
            [deduction-playground.io :as io]
            [deduction-playground.printer :refer [pprint]]))

(io/import-rules "resources/rules-prop-pred.clj")
;(io/import-rules "resources/rules-temporal.clj")
(io/import-trivials "resources/trivial-theorems.clj")
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
  (swap! p #(apply deduc/step-f-inside (conj (list rule line) %)))
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

(defn trivial
  "Apply the trivial-theorems inside the chosen line"
  [line]
  (swap! last_steps conj @p)
  (swap! p deduc/trivial line)
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

(defn show-rules []
  (for [rule @io/rules]
    (let [name (subs (str (key rule)) 1)
          given (:given (val rule))
          conclusion (:conclusion (val rule))
          forward? (:forwards (val rule))
          backward? (:backwards (val rule))]
      (println (str name ": \t" given " -> " conclusion)))))
    
    
 
  
