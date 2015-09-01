(ns deduction-playground.repl
  (:require [deduction-playground.proof-new :as proof]
            [deduction-playground.read-rules :as read]
            [deduction-playground.printer :refer [pprint]]))

(read/read-rules "resources/rules-temporal.clj")
(read/read-rules "resources/classical-theorems.clj")
;(read/read-rules "resources/rules.clj")
(read/read-theorems "resources/theorems.clj")

(def last_step (atom []))
(def p (atom []))

(defn show
  []
  (pprint @p))

(defn proof
  ([formula] (proof [] formula))
  ([premises formula]
    (reset! last_step [])
    (reset! p (proof/proof premises formula))
    (show)))

(defn step-f
  [rule & lines]
  (reset! last_step @p)
  (swap! p #(apply proof/step-f (conj (conj lines rule) %)))
  (show))

;; TEST
(defn step-f-inside
  [rule & lines]
  (reset! last_step @p)
  (swap! p #(apply proof/step-f-inside (conj lines rule %)))
  (show))
;; TEST END

(defn step-b
  [rule & lines]
  (reset! last_step @p)
  (swap! p #(apply proof/step-b (conj (conj lines rule) %)))
  (show))

(defn choose-option
  [line num]
  (reset! last_step @p)
  (swap! p proof/choose-option line num)
  (show))

(defn rename-var
  [old new]
  (reset! last_step @p)
  (swap! p proof/rename-var old new)
  (show))

(defn classical
  [line]
  (reset! last_step @p)
  (swap! p proof/classical line)
  (show))


(defn export-theorem
  [filename name]
  (read/export-theorem 
    filename
    @p
    name))

(defn undo []
  (if (empty? @last_step)
    (println "There is nothing to undo (you can only return to the last state not further)")
    (do
      (reset! p @last_step)
      (reset! last_step [])
      (show))))


;; STUFF FOR DEBUGGING

  
  
  
  
