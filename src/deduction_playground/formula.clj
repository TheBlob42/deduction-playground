(ns deduction-playground.formula
  (:require [deduction-playground.signature :refer [signature]]
            [deduction-playground.parser :refer [parse-formula]]
            [clojure.string :as s]))

(defn split-form [form]
  (s/split (s/replace (str form) #"\(|\)|\[|\]" {"(" "( " ")" " )" "[" "[ " "]" " ]"}) #" "))

(defn formula [form sign]
  (let [f (split-form form)
        meta (parse-formula sign f)]
    (if (nil? meta)
      nil
      (with-meta form meta))))
    