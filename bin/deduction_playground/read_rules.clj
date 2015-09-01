(ns deduction-playground.read-rules
  (:require [clojure.java.io :as io]
            [deduction-playground.proof :refer [proved?]])
  (:import [java.io PushbackReader]))

;; storage for rules, theorems and classical-theorems
(def rules (atom {}))
(def classicals (atom {}))
(def theorems (atom {}))

(defn reset-rules 
  "Empties the internal storage for rules"
  [] (reset! rules {}))

(defn reset-classicals 
  "Empties the internal storage for classical-theorems"
  [] (reset! classicals {}))

(defn reset-theorems 
  "Empties the internal storage for theorems"
  [] (reset! theorems {}))

(defn read-rules
  "Imports all rules from filename into the internal rules-storage. Existing rules will be kept."
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given      (:given item)
                                                     :conclusion (:conclusion item)}))
        (swap! rules merge result)))))

(defn read-classicals
  "Imports all classical-theorems from filename into the internal classical-theorems-storage.
Existing classical-theorems will be kept."
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given      (:given item)
                                                     :conclusion (:conclusion item)}))
        (swap! classicals merge result)))))

(defn read-theorems
  "Imports all theorems from filename into the internal theorems-storage. Existing theorems will be kept."
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

(defn export-theorem
  "Exports proof as a theorem with the name to filename"
  [proof filename name]
  (if (proved? proof)
    (if (.exists (io/as-file filename))
      (let [given (into [] (map :body (filter #(= (:rule %) :premise) (flatten proof))))
            conclusion (vector (:body (last proof)))
            theorem {:name name
                     :given given
                     :conclusion conclusion
                     :proof proof}]
        (with-open [writer (io/writer filename :append true)]
          (.write writer (str theorem))
          (.newLine writer))
        (swap! theorems merge (hash-map (keyword name) (dissoc theorem :name))))
      (throw (Exception. (str "The System can't find the file \"" filename "\""))))
    (throw (Exception. "The given proof is not solved yet. You can only export solved proofs as theorems."))))
