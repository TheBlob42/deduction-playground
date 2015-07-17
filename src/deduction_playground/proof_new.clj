(ns deduction-playground.proof-new
  (:require [deduction-playground.auto-logic :as log]
            [deduction-playground.scope :as scope]))

; Take a look at "postwalk" from clojure.walk
; replace (filter #(contains? set %) coll) with (filter set coll)

(def id (atom 0))
(defn new-id []
  (swap! id inc))

(def var-id (atom 0))
(defn new-var []
  (symbol (str 'V (swap! var-id inc))))

(defn infer
  "Creates a proof of the given premises (optional, vector for multiples) and the formula"
  ([formula] (infer [] formula))
  ([premises formula & [superproof?]]
    (let [desc (if superproof? :premise :assumption)
          prem (if (vector? premises) 
                 (into [] (map #(hash-map :id (new-id)
                                          :body %
                                          :rule desc) premises))
                 [{:id (new-id) :body premises :rule desc}])
          todo {:id (new-id) :body :todo :rule nil}
          form {:id (new-id) :body formula :rule nil}]
      (conj prem todo form))))

(defn rev-infer
  "Creates \"(infer [premises] formula)\" like depiction of the given (sub)proof"
  [proof]
  (let [premises (into [] (map #(:body %) (filter #(or (= (:rule %) :assumption)
                                                       (= (:rule %) :premise)) proof)))]
    `(~'infer ~premises ~(:body (last proof)))))

(defn proof
  "Starts a new superproof"
  ([formula] (proof [] formula))
  ([premises formula]
    (reset! id 0)
    (reset! var-id 0)
    (apply infer [premises formula true])))

(defn line-to-id
  [proof line]
  (if (not (vector? line))
    (:id (nth (flatten proof) (dec line)))
    [(line-to-id proof (first line)) (line-to-id proof (last line))]))

(defn id-to-line
  [proof id]
	(if (not (vector? id))
	  (loop [p (flatten proof)
	         l 1]
	    (if (= (:id (first p)) id)
	      l
	      (recur (rest p) (inc l))))
	  [(id-to-line proof (first id)) (id-to-line proof (last id))]))

(defn between [x y] [x y])

(defn get-item
  [proof line]
  (if (not (vector? line))
    (nth (flatten proof) (dec line))
    (loop [p proof
           l 1]
      (cond 
        (empty? p) nil
        (= (first line) l) (first p)
        (vector? (first p)) (recur (into [] (concat (first p) (subvec p 1))) (inc l))
        :else (recur (subvec p 1) (inc l))))))

(defn check-args
  [proof rule lines forward?]
  (cond
    ; rule exists
    ; right number of arguments
    ; args distinct?
    ; all lines in range of proof?
    forward?
    (let [lastline    (last (sort-by #(if (vector? %) (first %) %) lines))
          items       (map #(get-item proof %) lines)
          scope-info  (scope/get-scope proof (get-item proof lastline))
          scope       (:scope scope-info)
          todos       (:todo scope-info)
          conclusions (:conclusions scope-info)]
      (cond
        (not-every? #(contains? (set scope) %) items)
        (println "Not all in same scope")

        (> (count todos) 1)
        (println "There can't be more than one :todo line")
        
        (some #(contains? (set items) %) todos)
        (println "Can't use empty line for forward resulting")
        
        (some #(contains? (set items) %) conclusions)
        (println "Can't use conclusion line for forward resulting")
            
        :else {:lastline lastline
               :items items
               :todo (first todos)
               :conclusions conclusions}))
    
    (not forward?)
    (let [line     (first lines)
          item     (get-item proof line)
          scope-info (scope/get-scope proof item)
          scope    (:scope scope-info)
          todos    (:todo scope-info)
          premises (:premises scope-info)]
      (cond 
        ; More than one line
        ; No todos
        ; More than one todo
        ; Used a line with rule != nil to start from
        ; Used a todo line to start from
        :else {:item item
               :todo (first todos)
               :premises premises}))))

(defn item-to-rule-arg
  [item]
  (if (not (vector? item))
    (:body item)
    (rev-infer item)))


;(defn remove-duplicates
;  "Removes lines with duplicate bodies from the given proof.
;Moves recursive over all inner vectors, but only removes duplicates within a vector level.
;The ids of the deleted and remaining items will be saved as meta-data, for further use."
;  [proof]
;  (let [duplicates      (set (map first (filter #(> (val %) 1) (frequencies (map :body (remove vector? proof))))))
;        duplicate-items (filter #(contains? duplicates (:body %)) proof)
;        remain-ids   (map :id (filter :rule duplicate-items)) ; items with a rule (e.g. :assumption, :premise, "<string>") will remain
;        delete-items (remove :rule duplicate-items)
;        delete-ids   (map :id delete-items) ; items without a rule (:rule = nil) will be deleted
;        new-proof (into [] (map #(if (vector? %) (remove-duplicates %) %) (remove (set delete-items) proof)))
;        child-meta (apply merge (map meta (filter vector? new-proof)))
;        meta       (reduce #(assoc %1 %2 (last remain-ids)) {} delete-ids)]
;    (with-meta new-proof (merge meta child-meta))))

(defn remove-duplicates
  "Checks proof for duplicate lines that are in the same scope and deletes them if possible.
The ids of the deleted lines and their replacement will be saved as meta-data in the form {deleted-id replacement-id [...]}"
  ([proof] (remove-duplicates proof proof))
  ([proof sub]
    (let [parent-meta (meta proof); why gets the meta-data lost when we move this definition some lines down???
          scope-info (scope/get-scope proof (last sub))
          scope      (:scope scope-info)
          duplicates (disj (set (map first (filter #(> (val %) 1) (frequencies (map :body (remove vector? scope)))))) :todo)
          duplicate-items (filter #(contains? duplicates (:body %)) scope)
          remain-ids      (map :id (filter :rule duplicate-items))
          delete-items    (remove :rule (filter #(contains? duplicates (:body %)) sub)); just items from the actual sub can be deleted
          delete-ids      (map :id delete-items)
          meta      (reduce #(assoc %1 %2 (last remain-ids)) {} delete-ids)
          new-proof (with-meta (reduce #(scope/remove-item %1 %2) proof delete-items) (merge meta parent-meta))]
;      (println (meta proof)); why is the meta-data lost down here???
      (reduce #(if (vector? %2) (remove-duplicates %1 %2) %1) new-proof sub))))

(defn adjust-ids
  "Adjusts the ids used within the :rule of the items.
Replaces deleted ids with their new target.
Information is provided by the meta-data created through \"remove-duplicates\"."
  [proof]
  (let [meta  (meta proof)
        regex (java.util.regex.Pattern/compile (clojure.string/join "|" (map #(str "\\b" % "\\b") (map key meta)))); \b marks the word boundary to difference 10 from 1|0 etc.
        smap  (apply merge (map #(hash-map (str (key %)) (str (val %))) meta))]
    (if (not-empty meta)
      (clojure.walk/postwalk 
        (fn [node]
          (if (vector? node)
            node
            (if (string? (:rule node))
              (assoc node :rule (clojure.string/replace (:rule node) regex #(get smap %)))
              node))) 
        proof)
      proof)))
    

(defn remove-todos
  "Removes all todo-lines, if all lines inside the (sub)proof are solved (rule =! nil)"
  [proof]
  (let [solved (< (count (remove :rule (remove #(= (:body %) :todo) (remove vector? proof)))) 1)]
    (loop [p proof
           np []]
      (cond 
        (empty? p) np
        (vector? (first p)) (recur (subvec p 1) (conj np (remove-todos (first p))))
        :else 
        (if (and solved (= (:body (first p)) :todo))
          (recur (subvec p 1) np)
          (recur (subvec p 1) (conj np (first p))))))))
          
(defn check-duplicates
  "Removes duplicate lines, adjust leftover ids and remove :todo lines if necessary"
  [proof]
  (remove-todos (adjust-ids (remove-duplicates proof))))

(defn rename-var
  "Renames a variable"
  [proof old new]
  (check-duplicates
    (clojure.walk/postwalk
      (fn [node]
        (if (map? node)
          (cond
            (symbol? (:body node)) (if (= (:body node) old)
                                     (assoc node :body new) 
                                     node)
            (list? (:body node)) (assoc node :body (clojure.walk/prewalk-replace {old new} (:body node)))
            :else node)
          node)) 
      proof)))
                    
(defn init-vars
  [bodies]
  (let [vars (set (filter #(.startsWith (str %) "_") (flatten bodies)))
        smap (reduce #(assoc %1 %2 (new-var)) {} vars)
        new-bodies (map #(if (symbol? %)
                           (if (contains? vars %) (get smap %) %)
                           (clojure.walk/prewalk-replace smap %)) bodies)]
    new-bodies))

(defn create-item
  ([body] (create-item body nil))
  ([body rule]
	  (if (and (list? body)
	           (contains? #{'infer} (first body)))
     (do 
       (condp = (first body)
         'infer (eval (conj (map #(list `quote %) (rest body)) `infer))))
     {:id   (new-id)
	    :body body
	    :rule rule})))

(defn create-items
  "Takes a list of line bodies (and a optional rule) and creates items for the proof"
  ([bodies] (create-items bodies nil))
  ([bodies rule]
    (let [newb (init-vars bodies)
          ; to ensure that all bodies of all items are either symbols or lists, convert all lazy-seq (they come from rule evaluation) to lists
          non-lazy (map #(if (instance? clojure.lang.LazySeq %) (apply list %) %) newb)]
      (map #(create-item % rule) non-lazy))))




; TODO not only check the conclusions but the premises as well
(defn choose-option
  [proof line option]
  (let [item (get-item proof line)
        options (:body item)
        opt (get options option)]
    (cond 
      (not (map? options))
      (throw (Exception. (str "There is nothing to choose in line " line)))
      
      (nil? opt)
      (throw (Exception. (str "There is no option \"" option "\" to choose")))
      
      :else
      (let [items (if (vector? opt) opt [opt])
            scope-info  (scope/get-scope proof item)
            todo-item   (first (:todo scope-info))
            conc-items  (:conclusions scope-info)
            conclusions (set (map :body conc-items))
            match-items (filter #(contains? (set items) (:body %)) conc-items)
            rest        (remove #(contains? conclusions %) items)
            rest-items  (create-items rest (:rule item))
            p1 (reduce #(scope/change-item %1 %2 {:id   (:id %2)
                                                  :body (:body %2)
                                                  :rule (:rule item)}) proof match-items)
            p2 (reduce #(scope/add-after-item %1 item %2) p1 rest-items)
            p3 (scope/remove-item p2 item)]
        (if (= (count conc-items) (count match-items))
          (scope/remove-item p3 todo-item) p3)))))

(defn step-f
  [proof rule & lines]
  (let [info         (check-args proof rule lines true)
        lastline     (:lastline info)
        line-items   (:items info)
        todo-item    (:todo info)
        conc-items   (:conclusions info)
        ids (map #(line-to-id proof %) lines)
        rule-args (map #(item-to-rule-arg %) line-items)
        rule-result (apply log/apply-rule1 (concat [rule true] rule-args))]
    (cond
      (empty? rule-result)
      (throw (Exception. (str "Incorrect parameters for the rule \"" rule "\". Please check the description.")))
      
      ; user has to choose which is the right permutation
      (> (count rule-result) 1)
      (scope/add-after-line proof 
                            lastline 
                            {:id   (new-id)
                             :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                             :rule (pr-str rule ids)}) 
      
      :else
      (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)
            conclusions (set (map :body conc-items))
            matches     (filter #(contains? conclusions %) result)
            rest        (remove #(contains? conclusions %) result)
            match-items (filter #(contains? (set matches) (:body %)) conc-items)
            rest-items  (create-items rest (pr-str rule ids))
            p1 (reduce #(scope/change-item %1 %2 {:id   (:id %2)
                                                  :body (:body %2)
                                                  :rule (pr-str rule ids)}) proof match-items)
            p2 (reduce #(scope/add-after-line %1 lastline %2) p1 rest-items)]
        (if (= (count conc-items) (count match-items))
          (scope/remove-item p2 todo-item) p2)))))
      
; TODO backward step with more than one line
(defn step-b
  [proof rule & lines]
  (let [info (check-args proof rule lines false)
        line-item (:item info)
        todo-item (:todo info)
        prem-items (:premises info)
        rule-args (item-to-rule-arg line-item)
        rule-result (apply log/apply-rule1 (concat [rule false] (list rule-args)))]
    (cond
      (empty? rule-result)
      (throw (Exception. "Incorrect parameters for the given rule"))
      
      :else
      (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)
            premises  (set (map :body prem-items))
            matches   (filter #(contains? premises %) result)
            rest      (into [] (remove #(contains? premises %) result))
            match-items (filter #(contains? (set matches) (:body %)) prem-items)
            match-ids   (map #(if (vector? %) [(:id (first %)) (:id (last %))] (:id %)) match-items)
            rest-items  (create-items rest)
            rest-ids    (map #(if (vector? %) [(:id (first %)) (:id (last %))] (:id %)) rest-items)
            p1 (reduce #(scope/add-before-item %1 line-item %2) proof rest-items)
            p2 (scope/change-item p1 line-item {:id   (:id line-item)
                                                :body (:body line-item)
                                                :rule (pr-str rule (concat match-ids rest-ids))})]
        (if (or (empty? rest) (every? vector? rest-items))
          (scope/remove-item p2 todo-item) p2)))))
 
  
      
      
    
    