(ns deduction-playground.prereqs
  (:require [clojure.zip :as zip]))

;; NOTE: If you want to check for list-structures (e.g. (and a b)) always use "seq?" instead of "list?" (seq? will also work with lazy-sequences which may occur)

;; SUBSTITUTION
;; checks whether it is allowed to substitute the variable "old" for the variable "new" inside the formula "formula" or not
(defn- get-paths
  [formula old]
  (loop [zform (zip/next (zip/seq-zip formula))
         paths []]
    (cond
      (zip/end? zform) paths
      (= (first zform) old) (recur (zip/next zform) (conj paths (zip/path zform)))
      :else (recur (zip/next zform) paths))))

(defn- get-bvars
  [paths]
  (let [fn-bvars (fn [path]
                   (reduce #(if (vector? (second %2))
                              (concat %1 (second %2)) %1) [] path))
        vars (reduce #(concat %1 %2) [] (map fn-bvars paths))]
    (set vars)))

(defn- get-vars
  [form]
  (cond
    (symbol? form) #{form}
    (seq? form) (reduce #(clojure.set/union %1 (get-vars %2)) #{} (rest form))
    :else (throw (Exception. (str "Prerequisite | Invalid term (" form "): A term can only contain symbols and lists.")))))

(defn substitutionable?
  [formula old new]
  (let [vars  (get-vars new)
        paths (get-paths formula old)
        bounded-vars (get-bvars paths)
        intersec (clojure.set/intersection bounded-vars vars)]
    (if (or (empty? intersec)
            (and (= (count intersec) 1)
                 (contains? intersec old)))
      true
      false)))