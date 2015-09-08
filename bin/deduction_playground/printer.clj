(ns deduction-playground.printer
  (:require [deduction-playground.proof :refer [id-to-line]]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;; distancen between left and right edge
(def distance 40)
;; length of the divider for subproofs
(def divider-length 25)

(defn pprint-line
  [proof depth item]
  (let [line (id-to-line proof (:id item))
        body (str (if (= (:body item) :todo) "..." (:body item)))
        rule (condp = (:rule item)
               nil         ""
               :premise    "premise"
               :assumption "assumption"
               (let [name (subs (:rule item) 0 (.lastIndexOf (:rule item) "("))
                     ids  (subs (:rule item) (inc (.lastIndexOf (:rule item) "(")) (.lastIndexOf (:rule item) ")"))
                     ;; convert ids to line-numbers
                     lines   (str/replace ids #"\b[0-9]+\b" #(str (id-to-line proof (Integer. %))))
                     ;; [12 12] => [12]
                     lines-1 (str/replace lines #"\[([0-9]+)\s\1\]" #(str "[" (second %) "]"))
                     ;; sort the line-numbers asc
                     lines-2 (if (zero? (count lines-1))
                               ""
                               (str "(" (str/join " " (sort (str/split lines-1 #"\s+(?=[^\])}]*([\[({]|$))"))) ")"))]
                 (str name lines-2)))]
    (print (pp/cl-format nil "~3d: " line))
    (when (pos? depth)
      (print " ")
      (dotimes [_ depth] (print "| ")))
    (print body)
    (dotimes [_ (- distance (count body) (if (> depth 0) (inc (* 2 depth)) 0))] (print " "))
    (println rule)))

(defn pprint
  ([proof] (pprint proof proof 0))
  ([proof sub depth]
    (print "     ")
    (when (pos? depth)
      (print " ")
      (dotimes [_ (dec depth)] (print "| ")))
    (dotimes [_ (- divider-length depth)] (print "--"))
    (println)
    (doseq [item sub]
      (if (vector? item)
        (pprint proof item (inc depth))
        (pprint-line proof depth item)))
    (print "     ") 
    (when (pos? depth)
      (print " ")
      (dotimes [_ (dec depth)] (print "| ")))
    (dotimes [_ (- divider-length depth)] (print "--"))
    (println)))
      