(ns deduction-playground.read-rules
  (:require [clojure.java.io :as io]
            [deduction-playground.proof :refer [proved?]])
  (:import [java.io PushbackReader]))

(def rules (atom {}))
(def theorems (atom {}))

(def classicals (atom {}))

(defn reset-rules []
  (reset! rules {}))

(defn reset-theorems []
  (reset! theorems {}))

;; read methods for rules and theorems should be unified (see line 23 and 34)
(defn read-rules
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given (eval (:given item))
                                                     :conclusion (eval (:conclusion item))}))
        (swap! rules merge result)))))

(defn read-classicals
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given (eval (:given item))
                                                     :conclusion (eval (:conclusion item))}))
        (swap! classicals merge result)))))

(defn read-theorems
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given      (:given item)
                                                     :conclusion (:conclusion item)
                                                     :proof      (:proof item)}))
        (swap! theorems merge result)))))

(read-rules "resources/rules.clj")
(read-theorems "resources/theorems.clj")
(read-classicals "resources/classical-theorems.clj")

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
        (.newLine writer))
      (swap! theorems merge theorem))))

