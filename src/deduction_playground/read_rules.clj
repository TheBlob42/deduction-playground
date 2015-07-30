(ns deduction-playground.read-rules
  (:require [clojure.java.io :as io]
            [deduction-playground.proof :refer [proved?]])
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

(defn read-theorems
  []
  (with-open [reader (io/reader "src/deduction_playground/theorems.clj")]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given (:given item)
                                                     :conclusion (:conclusion item)}))
        result))))

(defn export-theorem
  [filename proof name]
  (if (proved? proof)
    (let [given (into [] (map :body (filter #(= (:rule %) :premise) (flatten proof))))
          conclusion (vector (:body (last proof)))
          theorem {:name name
                   :given given
                   :conclusion conclusion
                   :proof proof}]
      (with-open [writer (io/writer filename :append true)]
        (.write writer (str theorem))
        (.newLine writer)))))