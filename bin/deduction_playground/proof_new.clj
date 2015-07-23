(ns deduction-playground.proof-new
  (:require [deduction-playground.auto-logic :as log]
            [deduction-playground.scope :as scope]))

;; atoms to provide unique ids for items and variables
(def id (atom 0))
(defn new-id []
  (swap! id inc))

(def var-id (atom 0))
(defn new-var []
  (symbol (str 'V (swap! var-id inc))))
;; ---------------------------------------------------

; CHECK DUPLICATES STUFF
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
          equals (map val (group-by :body duplicate-items))
          fn-smap (fn [equals]
                    (let [remain (map :id (filter :rule equals))
                          delete (map :id (filter (set sub) (remove :rule equals)))]; just items from the actual sub can be deleted
                      (reduce #(assoc %1 %2 (last remain)) {} delete)))
          meta (apply merge (map fn-smap equals))
          delete-items (remove :rule (filter (set duplicate-items) sub))
          new-proof (with-meta (reduce #(scope/remove-item %1 %2) proof delete-items) (merge meta parent-meta))]
;      (println (meta proof)); why is the meta-data lost down here???
      (reduce #(if (vector? %2) (remove-duplicates %1 %2) %1) new-proof sub))))

(defn adjust-ids
  "Adjusts the ids used within the :rule of the items.
Replaces deleted ids with their new target.
Information is provided by the meta-data created through \"remove-duplicates\"."
  [proof]
  (let [meta  (meta proof)
        ;; \b marks the word boundary to difference 10 from 1|0 etc.
        regex (java.util.regex.Pattern/compile (clojure.string/join "|" (map #(str "\\b" % "\\b") (map key meta))))
        smap  (apply merge (map #(hash-map (str (key %)) 
                                           (if (list? (val %))
                                             (clojure.string/join " " (val %)); swaps one id with a list of others
                                             (str (val %)))) meta))]
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



(defn infer
  "Creates a (sub)proof for premises and formula in the structure needed for further actions.
\"premises\" can be a single object, a vector of objects or skipped
\"formula\" needs to be a single object
\"superproof?\" decides if its a proof or subproof" 
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
      (check-duplicates (conj prem todo form)))))

(defn re-infer
  "Returns a (sub)proof from the internal strucure back to a depiction like \"(infer [premises] formula)\""
  [proof]
  (let [premises (into [] (map #(:body %) (filter #(or (= (:rule %) :assumption)
                                                       (= (:rule %) :premise)) proof)))
        prem-args (if (> (count premises) 1) premises (first premises))]
    `(~'infer ~prem-args ~(:body (last proof)))))

(defn proof
  "Creates a new superproof
This is the entry point for new deductions"
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

(defn substitution
  "Substitute an variable identifier inside a predicate formula with another
e.g. (substitution '(P x) 'x 'Z) => (P Z)"
  [formula old new]
  (cond
    (not (list? formula))
    (throw (Exception. (str "The argument \"" formula "\" is not a list and therefore can't be substituted." 
                            "Maybe you have to provide optional arguments for the step you trying to accomplish.")))
    
    (contains? (set (flatten formula)) new)
    (throw (Exception. (str "Substitution failed. The identifier \"" new "\" is already used inside the formula \"" formula "\"")))
    
    :else (clojure.walk/postwalk-replace {old new} formula)))

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

(defn get-proofed-items
  "Only returns items that are either vectors or have a rule (they are prooved)"
  [items]
  (filter #(or (vector? %)
               (:rule %)) items))

(defn get-unproofed-items
  "Only returns items that are neither vectors nor have a rule (they are unprooved so far)"
  [items]
  (remove #(or (vector? %)
               (:rule %)) items))

(defn check-args
  [proof rule lines forward?]
  (cond
    ; rule exists
    ; args distinct?
    ; all lines in range of proof?
    forward?
    (let [lastline    (last (sort-by #(if (vector? %) (first %) %) lines))
          items       (map #(get-item proof %) lines)
          obligatories (get-proofed-items items)
          optional     (get-unproofed-items items)
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
        
        (not= (count obligatories) (log/rule-givens rule))
        (println (str (if (> (count obligatories) (log/rule-givens rule))
                        "Too many "
                        "Not enough ") "proofed lines (rule != nil) for this rule. You need exactly " (log/rule-givens rule)))
        
        (> (count optional) (dec (log/rule-conclusions rule)))
        (println "Too many optional unproofed lines (rule = nil) for this rule. You can have at a max " (dec (log/rule-conclusions rule)))
        
;        (some #(contains? (set items) %) conclusions)
;        (println "Can't use conclusion line for forward resulting")
            
        :else {:lastline lastline
               :items items
               :todo (first todos)
               :conclusions conclusions
               :obligatories obligatories 
               :optional optional}))
    
    (not forward?)
    (let [lastline  (last (sort-by #(if (vector? %) (first %) %) lines))
          items     (map #(get-item proof %) lines)
          obligatories (get-unproofed-items items)
          optional     (get-proofed-items items)
          scope-info (scope/get-scope proof (get-item proof lastline))
          scope    (:scope scope-info)
          todos    (:todo scope-info)
          premises (:premises scope-info)]
      (cond 
        ; More than one line
        ; Used a line with rule != nil to start from
        (not= (count obligatories) (log/rule-conclusions rule))
        (println (str (if (> (count (remove :rule items)) (log/rule-conclusions rule))
                        "Too many "
                        "Not enough ") "unproofed lines (rule = nil) for this rule. You need exactly " (log/rule-conclusions rule)))
        
        (> (count optional) (dec (log/rule-givens rule)))
        (println "Too many optional proofed lines for this rule. You can have at a max " (dec (log/rule-givens rule)))
        
        (not-every? #(contains? (set scope) %) items)
        (println "Not all lines are in the same scope")
        
        (< (count todos) 1)
        (println "There is no :todo line inside your scope to work towards to")
        
        (> (count todos) 1)
        (println "There can't be more than one :todo line")
        
        (some #(contains? (set items) %) todos)
        (println "Can't use empty line for backward steps")
        
        :else {:lastline lastline
               :item items
               :todo (first todos)
               :premises premises
               :optional optional
               :obligatories obligatories}))))

(defn item-to-rule-arg
  [item]
  (if (not (vector? item))
    (:body item)
    (re-infer item)))

(defn get-item-id
  [item]
  (if (not (vector? item))
    (:id item)
    [(:id (first item)) (:id (last item))]))


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

(defn eval-item
  [body rule]
  (if (and (list? body)
           (contains? #{'infer 'substitution} (first body)))
    (condp = (first body)
      'infer      (let [prem (if (vector? (second body))
                               (into [] (map #(eval-item % rule) (second body)))
                               (eval-item (second body) rule))
                        conc (eval-item (second (rest body)) rule)]
                    (eval (conj (map #(list `quote %) (list prem conc)) `infer)))      
      'substitution (eval (conj (map #(list `quote (eval-item % rule)) (rest body)) `substitution)))
    body))
; TODO TEST substitution
; TODO inner items have to evaluated too e.g. (infer (substitution (P x) x t) 'c)
(defn create-item
  ([body] (create-item body nil))
  ([body rule]
	  (let [newbody (eval-item body rule)]
     (if (vector? newbody)
       newbody
       {:id   (new-id)
        :body newbody
	      :rule rule}))))


(defn create-items
  "Takes a list of line bodies (and a optional rule) and creates items for the proof"
  ([bodies] (create-items bodies nil))
  ([bodies rule]
    (let [newb (init-vars bodies)
          ;; to ensure that all bodies of all items are either symbols or lists, convert all lazy-seq (they come from rule evaluation) to lists
          non-lazy (clojure.walk/postwalk (fn [node]
                                            (if (instance? clojure.lang.LazySeq node)
                                              (apply list node)
                                              node)) newb)]
      (map #(create-item % rule) non-lazy))))




(defn choose-option
  "Chooses option num on line to be inserted into proof.
In case there is nothing to choose or the num is invalid, it throws an exception."
  [proof line num]
  (let [item (get-item proof line)
        options (:body item)
        opt (get options num)]
    (cond 
      (not (map? options))
      (throw (Exception. (str "There is nothing to choose in line " line)))
      
      (nil? opt)
      (throw (Exception. (str "There is no option \"" num "\" to choose")))
      
      :else
      (let [items (if (vector? opt) opt [opt])
            new-items (create-items items (:rule item))
            p1 (reduce #(scope/add-after-item %1 item %2) proof new-items)
            ;; adjust the ids of the proof to point on the newly created items instead of the old "choose-item" before checking for duplicates
            p2 (adjust-ids (with-meta p1 {(:id item) (apply list (map #(if (vector? %)
                                                                         [(:id (first %)) (:id (last %))]
                                                                         (:id %)) new-items))}))]
        (check-duplicates (scope/remove-item p2 item))))))
            
(defn step-f
  "Performs a forward step on proof by applying rule on the lines"
  [proof rule & lines]
  (let [info       (check-args proof rule lines true)
        todo-item        (:todo info)        
        obligatory-items (:obligatories info)
        optional-items   (:optional info)
        obligatory-ids   (map get-item-id obligatory-items)
        obligatory-args (into [] (map item-to-rule-arg obligatory-items))
        optional-args   (into [] (map item-to-rule-arg optional-items))        
        rule-result (apply log/apply-rule2 (conj [rule true] obligatory-args optional-args))]
    (if (empty? rule-result)
      (throw (Exception. (str "Incorrect parameters for the rule \"" rule "\". Please check the description.")))
      ;; add the used rule to the optional items
      (let [p1 (reduce #(scope/change-item %1 %2 {:id   (:id %2)
                                                  :body (:body %2)
                                                  :rule (pr-str rule obligatory-ids)}) proof optional-items)]
        (if (> (count rule-result) 1)
          ;; more than one possible result, the user has to decide which one fits his needs
          (scope/add-before-item p1 
                                 todo-item
                                 {:id   (new-id)
                                  :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                                  :rule (pr-str rule obligatory-ids)})
          ;; only one possible result (which can contain several items to insert)
          (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)               
                new-items (create-items result (pr-str rule obligatory-ids))]
            ;; if there is no empty line, insert everthing behind the last obligatory item
            (check-duplicates 
              (if todo-item
                (reduce #(scope/add-before-item %1 todo-item %2) p1 new-items)
                (reduce #(scope/add-after-item %1 (last obligatory-items) %2) p1 new-items)))))))))

(defn step-b
  "Performs a backward step on proof by applying rule on the lines"
  [proof rule & lines]
  (let [info (check-args proof rule lines false)
        todo-item        (:todo info)        
        obligatory-items (:obligatories info)
        optional-items   (:optional info)
        optional-ids     (map get-item-id optional-items)
        obligatory-args (into [] (map item-to-rule-arg obligatory-items))
        optional-args   (into [] (map item-to-rule-arg optional-items))
        rule-result (apply log/apply-rule2 (conj [rule false] obligatory-args optional-args ))]
     (cond
       (empty? rule-result)
       (throw (Exception. "Incorrect parameters for the given rule"))
      
       (> (count rule-result) 1)
       ;; more than one possible result, the user has to decide which one fits his needs
       (let [id (new-id)
             p1 (reduce #(scope/change-item %1 %2 {:id   (:id %2)
                                                   :body (:body %2)
                                                   :rule (pr-str rule (conj optional-ids id))}) proof obligatory-items)]
         (scope/add-after-item p1 
                               todo-item
                               {:id   id
                                :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                                :rule nil}))
       :else
       ;; only one possible result (which can contain several items to insert)
       (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)   
             new-items (create-items result)
             new-ids   (map get-item-id new-items)
             p1 (reduce #(scope/change-item %1 %2 {:id   (:id %2)
                                                   :body (:body %2)
                                                   :rule (pr-str rule (concat new-ids optional-ids))}) proof obligatory-items)]
         (check-duplicates (reduce #(scope/add-after-item %1 todo-item %2) p1 new-items))))))

    