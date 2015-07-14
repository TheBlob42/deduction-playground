(ns deduction-playground.printer
  (:require [deduction-playground.proof :as proof]))

(def distance 30)
(def divider-length 40)

; Take a look at "dotimes [] (print "")"

(defn print-proof-line
  [line depth body rule]
  (let [sbody (if (= body :todo) "..." (pr-str body))
        srule (if (nil? rule) "" rule)
        start (if (> depth 0) 
                (apply str (concat (take (* (dec depth) 3) (cycle " ")) [" | "]))
                " ")
        spaces (apply str (take (- distance (count sbody) (count start)) (cycle " ")))]
    (println (str line ":" start sbody spaces srule))))

(defn print-divider
  [depth]
  (let [start (apply str (take (* depth 3) (cycle " ")))
        div (apply str (take (- divider-length (* depth 3)) (cycle "-")))]
    (println (str start div))))

(defn pprint
  ([proof] (pprint proof 1 0))
  ([proof line depth]
    (print-divider depth)
    (loop [p proof
           l line]
      (cond 
        (empty? p)
        (print-divider depth)
        
        (map? (first p))
        (do 
          (print-proof-line l depth (:body (first p)) (:rule (first p)))
          (recur (subvec p 1) (inc l)))
        
        (vector? (first p))
        (do
          (pprint (first p) l (inc depth))
          (recur (subvec p 1) (+ l (count (flatten (first p))))))))))