(ns deduction-playground.read-rules
  (:require [clojure.java.io :as io])
  (:import [java.io PushbackReader]))

(defn read-rules
  []
  (with-open [reader (io/reader "src/deduction_playground/rules.clj")]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given (eval (:given item))
                                                     :conclusion (eval (:conclusion item))}))
        result))))