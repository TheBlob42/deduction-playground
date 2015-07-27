(ns deduction-playground.scope)

; Take a look at postwalk and prewalk from clojure.walk

(defn get-todo
  "Returns all empty items (:body = :todo) of a scope"
  [scope]
  (filter #(= (:body %) :todo) scope))
  
(defn scope-for-item
  [proof item]
  (if (contains? (set proof) item)
    {:scope proof
     :sub-scope proof}
    (loop [p proof
           scope []]
      (cond (empty? p) nil
            (vector? (first p))
            (if-let [info (scope-for-item (first p) item)]
              {:scope     (into [] (concat scope (:scope info)))
               :sub-scope (:sub-scope info)}
              (recur (subvec p 1) (conj scope (first p))))
            :else (recur (subvec p 1) (conj scope (first p)))))))

(defn get-scope
  [proof item]
  (let [info (scope-for-item proof item)]
    {:scope (:scope info)
     :todo  (get-todo (:sub-scope info))}))
