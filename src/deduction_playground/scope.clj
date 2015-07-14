(ns deduction-playground.scope)

; Take a look at postwalk and prewalk from clojure.walk

(defn get-todo
  "Returns all empty items (:body = :todo) of a scope"
  [scope]
  (filter #(= (:body %) :todo) scope))

(defn get-conclusions
  "Returns all possible conclusion items (no subproof, not empty, :rule = nil) of a scope.
Only items before the :todo-item are returned"
  [scope]
  (filter #(and (map? %)
                (nil? (:rule %))
                (not= (:body %) :todo)) scope))

(defn get-premises
  "Returns all items that are already proofed (premise, assumption or by rule)"
  [scope]
  (rest (drop-while #(not= (:body %) :todo) (reverse scope))))
  
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
              {:scope (into [] (concat scope (:scope info)))
               :sub-scope (:sub-scope info)}
              (recur (subvec p 1) (conj scope (first p))))
            :else (recur (subvec p 1) (conj scope (first p)))))))

(defn get-scope
  [proof item]
  (let [info (scope-for-item proof item)]
    {:scope (:scope info)
     :todo (get-todo (:sub-scope info))
     :premises (get-premises (:scope info))
     :conclusions (get-conclusions (:sub-scope info))}))


;TODO should these options be in its own namespace (e.g. proof, step)?
(defn get-item-on-line
  "Returns a line or subproof from the given proof
x => returns line x
[x y z] => returns subproof between x and z (including all items and possible subsubproofs)"
  [proof line]
  (if (not (vector? line))
    (nth (flatten proof) (dec line))
    (loop [p proof
           l 1]
      (cond
        (empty? p) nil
        (= (first line) l) (first p)
        (vector? (first p)) (recur (into [] (concat (first p) (subvec p 1))) l)
        :else (recur (subvec p 1) (inc l))))))

(defn edit-proof
  [proof item newitem mode]
  (let [index (.indexOf proof item)]
    (if (not= index -1)
      (condp = mode
        :add-before (into [] (concat (subvec proof 0 index) [newitem] (subvec proof index)))
        :add-after (with-meta (into [] (concat (subvec proof 0 (inc index)) [newitem] (subvec proof (inc index)))) {:found? true})
        :change (with-meta (into [] (concat (subvec proof 0 index) [newitem] (subvec proof (inc index)))) {:found? true})
        :remove (with-meta (into [] (concat (subvec proof 0 index) (subvec proof (inc index)))) {:found? true}))
      (loop [p proof
             res []]
        (cond 
          (empty? p) res
          (vector? (first p)) 
          (let [v (edit-proof (first p) item newitem mode)]
            (if (:found? (meta v))
              (with-meta (into [] (concat res [v] (subvec p 1))) {:found? true})
              (recur (subvec p 1) (conj res v))))
          :else (recur (subvec p 1) (conj res (first p))))))))

(defn add-after-line
  [proof after newitem]
  (let [item (get-item-on-line proof after)]
    (edit-proof proof item newitem :add-after)))

(defn add-after-item
  [proof after newitem]
  (edit-proof proof after newitem :add-after))

(defn add-before-line
  [proof before newitem]
  (let [item (get-item-on-line proof before)]
    (edit-proof proof before newitem :add-before)))

(defn add-before-item
  [proof before newitem]
  (edit-proof proof before newitem :add-before))

(defn remove-item
  [proof item]
  (edit-proof proof item nil :remove))

(defn change-line
  [proof line newitem]
  (let [item (get-item-on-line proof line)]
    (edit-proof proof item newitem :change)))

(defn change-item
  [proof item newitem]
  (edit-proof proof item newitem :change))


