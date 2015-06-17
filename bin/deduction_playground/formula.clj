(ns deduction-playground.formula
  (:require [deduction-playground.signature :refer [signature]]
            [deduction-playground.parser-test :refer [wff?]]))

;(defn split-form [form]
;  (s/split (s/replace (str form) #"\(|\)|\[|\]" {"(" "( " ")" " )" "[" "[ " "]" " ]"}) #" "))
;
;(defn formula [form sign]
;  (let [f (split-form form)
;        meta (parse-formula sign f)]
;    (if (nil? meta)
;      nil
;      (with-meta form meta))))

(defn formula 
  [form sign]
  (if (wff? form sign)
    (with-meta form sign)
    nil))


