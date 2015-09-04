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
               :premise    "gegeben"
               :assumption "angenommen"
               ;; split the string into two parts, the name of the rule (in "") and the linked ids (which will be replaced with the respective lines)
               (str (subs (:rule item) 0 (.lastIndexOf (:rule item) "\"")) ;; important so the rule name can have numbers in its name
                    (str/replace (subs (:rule item) (.lastIndexOf (:rule item) "\"")) #"\b[0-9]+\b" #(str (id-to-line proof (Integer. %))))))]
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
      