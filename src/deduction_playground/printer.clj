(ns deduction-playground.printer
  (:require [deduction-playground.proof-new :as proof]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(def distance 40)
(def divider-length 25)

(defn pprint-line
  [proof depth item]
  (let [line (proof/id-to-line proof (:id item))
        body (str (if (= (:body item) :todo) "..." (:body item)))
        rule (condp = (:rule item)
               :premise    "gegeben"
               :assumption "angenommen"
               (str/replace (str (:rule item)) #"\b[0-9]+\b" #(str (proof/id-to-line proof (Integer. %)))))]
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
      