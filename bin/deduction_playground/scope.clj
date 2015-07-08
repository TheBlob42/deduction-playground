(ns deduction-playground.scope)

(def p [1 2 [3 4] [5 [6 7 8] 9] 10 11])

(def p1 '[{:id 1 :body (and (impl p r) (impl q r)) :rule :premise}
          {:id 2 :body (impl p r) :rule "and-e1"}
          [{:id 3 :body (and p q) :rule :assumption}
           {:id 4 :body :todo :rule nil}
           {:id 5 :body r :rule nil}]
          {:id 6 :body (impl (and p q) r) :rule nil}])

(def p2 '[{:id 1 :body (impl p q) :rule :premise}
          {:id 2 :body (impl r s) :rule :premise}
          [{:id 3 :body (or p r) :rule :assumption}
           [{:id 4 :body p :rule :assumption}
            {:id 5 :body :todo :rule nil}
            {:id 6 :body (or q s) :rule nil}]
           [{:id 7 :body r :rule :assumption}
            {:id 8 :body :todo :rule nil}
            {:id 9 :body (or q s) :rule nil}]
           {:id 10 :body (or q s) :rule "or-e (3 [4-6] [7-9])"}]
          {:id 11 :body (impl (or p r) (or q s)) :rule "impl-i ([3 - 10])"}])

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

(defn scope-for-elem
  ([proof elem] (scope-for-elem proof elem 1))
  ([proof elem actual]
   (if (contains? (set proof) elem)
     proof
     (loop [p proof
            s []
            l actual]
       (cond 
         (empty? p) nil
         (vector? (first p))
         (if-let [v (scope-for-elem (first p) elem l)]
           (into [] (concat s v))
           (recur (subvec p 1) (conj s (first p)) (inc l))) 
         :else (recur (subvec p 1) (conj s (first p)) (inc l)))))))

(defn scope-for-line
  [proof line]
  (scope-for-elem proof (get-item-on-line proof line)))

(defn get-todo
  "Returns all empty items (:body = :todo) of a scope"
  [scope]
  (filter #(= (:body %) :todo) scope))

(defn get-result
  "Returns all possible result items (no subproof, not empty, :rule = nil) of a scope"
  [scope]
  (filter #(and (map? %)
                (nil? (:rule %))
                (not= (:body %) :todo)) scope))

  

;TODO add-before, add-after
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


